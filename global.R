# global.R — Packages, connexion DB, helpers partagés
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(bcrypt)
  library(rmarkdown)
  library(knitr)
  library(openxlsx)
  library(lubridate)
  library(glue)
  library(scales)
  library(tidyr)
  library(purrr)
  library(stringr)
})

# ── Détection du backend DB ────────────────────────────────────────────────────
# Si PG_HOST est défini → PostgreSQL (production shinyapps.io / serveur)
# Sinon                  → SQLite local (développement)
USE_POSTGRES <- nchar(Sys.getenv("PG_HOST")) > 0

APP_TITLE       <- "e-Portfolio DES Santé Publique"
N_SEMESTRES_MAX <- 8L

# ── Initialisation DB ──────────────────────────────────────────────────────────
source("data/referentiel.R")
source("data/init_db.R")

if (USE_POSTGRES) {
  # ── Connexion PostgreSQL ─────────────────────────────────────────────────────
  if (!requireNamespace("RPostgres", quietly = TRUE))
    stop("Package RPostgres requis pour le backend PostgreSQL.")

  .pg_params <- list(
    host     = Sys.getenv("PG_HOST"),
    port     = as.integer(Sys.getenv("PG_PORT", "5432")),
    dbname   = Sys.getenv("PG_DB",       "des_portfolio"),
    user     = Sys.getenv("PG_USER",     "postgres"),
    password = Sys.getenv("PG_PASSWORD", "")
  )

  db_connect <- function() {
    do.call(DBI::dbConnect, c(list(drv = RPostgres::Postgres()), .pg_params))
  }

  # Amorçage PostgreSQL (idempotent via IF NOT EXISTS)
  init_database_pg()

} else {
  # ── Connexion SQLite locale ──────────────────────────────────────────────────
  DB_PATH <- Sys.getenv("DES_DB_PATH", "data/portfolio_des.sqlite")
  if (!file.exists(DB_PATH))
    dir.create(dirname(DB_PATH), showWarnings = FALSE, recursive = TRUE)

  db_connect <- function() DBI::dbConnect(RSQLite::SQLite(), DB_PATH)

  init_database(DB_PATH)
}

# ── Helpers DB génériques (identiques SQLite/PG) ──────────────────────────────
.replace_placeholders <- function(sql) {
  # Remplace chaque ? par $1, $2, ... pour PostgreSQL (sub() en boucle, R-compatible)
  n <- 0L
  while (grepl("?", sql, fixed = TRUE)) {
    n <- n + 1L
    sql <- sub("?", paste0("$", n), sql, fixed = TRUE)
  }
  sql
}

.now_sql <- function() {
  if (USE_POSTGRES) "NOW()" else "datetime('now')"
}

db_query <- function(sql, params = list()) {
  sql <- gsub("datetime\\('now'\\)", .now_sql(), sql, fixed = FALSE)
  if (USE_POSTGRES && length(params) > 0) sql <- .replace_placeholders(sql)
  con <- db_connect(); on.exit(DBI::dbDisconnect(con))
  if (length(params)) DBI::dbGetQuery(con, sql, params) else DBI::dbGetQuery(con, sql)
}

db_execute <- function(sql, params = list()) {
  sql <- gsub("datetime\\('now'\\)", .now_sql(), sql, fixed = FALSE)
  if (USE_POSTGRES && length(params) > 0) sql <- .replace_placeholders(sql)
  con <- db_connect(); on.exit(DBI::dbDisconnect(con))
  if (length(params)) DBI::dbExecute(con, sql, params) else DBI::dbExecute(con, sql)
}

# ── Alias courts des domaines (pour graphiques) ──────────────────────────────
DOMAINE_ALIAS <- c(
  "1 - Biostatistiques"                                                          = "Biostat.",
  "2 - Épidémiologie et méthodes en recherche clinique"                          = "Épidémio.",
  "3 - Informatique biomédicale et e-santé"                                      = "Info. médicale",
  "4 - Gestion de la qualité, des risques et de la sécurité des soins"           = "Qualité/Risques",
  "5 -  Économie, administration des services de santé, politiques de santé"     = "Économie santé",
  "6 - Sciences humaines et sociales"                                             = "SHS",
  "7 - Environnement et santé"                                                    = "Environnement",
  "8 - Promotion de la santé"                                                     = "Promo. santé",
  "Connaissances et compétences transversales"                                    = "Transversal"
)

# Convertit un vecteur de noms de domaines en alias courts
.domaine_alias <- function(x) {
  res <- DOMAINE_ALIAS[x]
  ifelse(is.na(res), sub("^\\d+ -+\\s*", "", x), res)
}

# ── Constantes UI ─────────────────────────────────────────────────────────────
STATUT_CONN_CHOICES <- c(
  "Non évalué"  = "non_evalue",
  "Non acquis"  = "non_acquis",
  "Acquis"      = "acquis"
)
STATUT_COMP_CHOICES <- c(
  "Non évalué"         = "non_evalue",
  "Non acquis"         = "non_acquis",
  "En cours d'acquisition" = "en_cours",
  "Acquis"             = "acquis"
)

EVAL_RESP_CHOICES <- c(
  "—"                      = "",
  "Non acquis"             = "non_acquis",
  "En cours d'acquisition" = "en_cours",
  "Acquis"                 = "acquis"
)
PHASES <- c("socle","approfondissement","consolidation")
PHASE_LABELS <- c(socle="Phase Socle", approfondissement="Phase d'Approfondissement", consolidation="Phase de Consolidation")

COULEURS_STATUT <- c(
  non_evalue = "#adb5bd",
  non_acquis = "#e74c3c",
  en_cours   = "#f39c12",
  acquis     = "#27ae60"
)

# ── Helpers Auth ──────────────────────────────────────────────────────────────
verify_user <- function(username, password) {
  row <- db_query("SELECT id, hashed_pw, role, nom, prenom, active FROM users WHERE username = ?",
                  list(username))
  if (nrow(row) == 0 || row$active[1] == 0) return(NULL)
  if (!bcrypt::checkpw(password, row$hashed_pw[1])) return(NULL)
  list(id = row$id[1], username = username, role = row$role[1],
       nom = row$nom[1], prenom = row$prenom[1])
}

get_all_internes <- function(include_termine = FALSE) {
  if (include_termine) {
    db_query("SELECT id, username, nom, prenom, email, promotion, faculte, statut
              FROM users WHERE role = 'interne' AND active = 1
              ORDER BY statut, nom, prenom")
  } else {
    db_query("SELECT id, username, nom, prenom, email, promotion, faculte, statut
              FROM users WHERE role = 'interne' AND active = 1 AND statut = 'actif'
              ORDER BY nom, prenom")
  }
}

# ── Helpers DB portfolio ──────────────────────────────────────────────────────

get_identite <- function(user_id) {
  db_query("SELECT * FROM identite WHERE user_id = ?", list(user_id))
}
save_identite <- function(user_id, nom, prenom, date_naissance,
                           faculte_2e_cycle, annee_edn, des_initial, faculte_3e_cycle) {
  # nom/prenom sont la source unique : on écrit dans users ET identite
  nom_val    <- nom    %||% NA
  prenom_val <- prenom %||% NA
  # Mise à jour table users (source de vérité pour nom/prenom)
  db_execute("UPDATE users SET nom=?, prenom=? WHERE id=?",
             list(nom_val, prenom_val, user_id))
  # Mise à jour ou insertion identite
  ex <- db_query("SELECT id FROM identite WHERE user_id = ?", list(user_id))
  if (nrow(ex) == 0) {
    db_execute("INSERT INTO identite
                (user_id,date_naissance,faculte_2e_cycle,annee_edn,
                 des_initial,faculte_3e_cycle,updated_at)
                VALUES (?,?,?,?,?,?,datetime('now'))",
               list(user_id, date_naissance %||% NA, faculte_2e_cycle %||% NA,
                    annee_edn %||% NA, des_initial %||% NA, faculte_3e_cycle %||% NA))
  } else {
    db_execute("UPDATE identite SET date_naissance=?,faculte_2e_cycle=?,annee_edn=?,
                des_initial=?,faculte_3e_cycle=?,updated_at=datetime('now')
                WHERE user_id=?",
               list(date_naissance %||% NA, faculte_2e_cycle %||% NA, annee_edn %||% NA,
                    des_initial %||% NA, faculte_3e_cycle %||% NA, user_id))
  }
  # Sauvegarder les diplomes via fonction séparée
  invisible(TRUE)
}

get_contrat <- function(user_id) {
  db_query("SELECT * FROM contrat_formation WHERE user_id = ?", list(user_id))
}
save_contrat_full <- function(user_id, projet, obj_conn, obj_comp,
                               formations_envisagees,
                               these_statut, these_sujet, these_directeur, these_date) {
  ex <- db_query("SELECT id FROM contrat_formation WHERE user_id = ?", list(user_id))
  if (nrow(ex) == 0) {
    db_execute("INSERT INTO contrat_formation
                (user_id,projet_professionnel,obj_connaissances,obj_competences,
                 formations_envisagees,
                 these_statut,these_sujet,these_directeur,these_date,updated_at)
                VALUES (?,?,?,?,?,?,?,?,?,datetime('now'))",
               list(user_id, projet %||% NA, obj_conn %||% NA, obj_comp %||% NA,
                    formations_envisagees %||% NA,
                    these_statut %||% "non_debutee", these_sujet %||% NA,
                    these_directeur %||% NA, these_date %||% NA))
  } else {
    db_execute("UPDATE contrat_formation SET
                projet_professionnel=?,obj_connaissances=?,obj_competences=?,
                formations_envisagees=?,
                these_statut=?,these_sujet=?,these_directeur=?,these_date=?,
                updated_at=datetime('now') WHERE user_id=?",
               list(projet %||% NA, obj_conn %||% NA, obj_comp %||% NA,
                    formations_envisagees %||% NA,
                    these_statut %||% "non_debutee", these_sujet %||% NA,
                    these_directeur %||% NA, these_date %||% NA, user_id))
  }
}

get_eval_connaissances <- function(user_id) {
  db_query("SELECT
              r.id              AS ref_id,
              r.domaine, r.niveau, r.libelle,
              e.id              AS eval_id,
              e.autoeval, e.eval_senior, e.evaluateur_senior,
              e.date_eval_senior, e.commentaire, e.updated_at
            FROM ref_connaissances r
            LEFT JOIN eval_connaissances e ON e.ref_id = r.id AND e.user_id = ?
            ORDER BY r.id", list(user_id)) |>
    mutate(
      autoeval    = ifelse(is.na(autoeval), "non_evalue", autoeval),
      eval_senior = ifelse(is.na(eval_senior), NA_character_, eval_senior)
    )
}

upsert_eval_connaissance <- function(user_id, ref_id, autoeval, eval_senior,
                                      evaluateur_senior, date_eval_senior, commentaire) {
  today <- format(Sys.Date(), "%d/%m/%Y")
  date_val <- if (!is.null(date_eval_senior) && !is.na(date_eval_senior) && nchar(date_eval_senior) > 0)
    date_eval_senior else today
  ex <- db_query("SELECT id FROM eval_connaissances WHERE user_id=? AND ref_id=?", list(user_id, ref_id))
  if (nrow(ex) == 0) {
    db_execute("INSERT INTO eval_connaissances
                (user_id,ref_id,autoeval,eval_senior,evaluateur_senior,date_eval_senior,commentaire,updated_at)
                VALUES (?,?,?,?,?,?,?,datetime('now'))",
               list(user_id, ref_id, autoeval, eval_senior %||% NA,
                    evaluateur_senior %||% NA, date_val, commentaire %||% NA))
  } else {
    db_execute("UPDATE eval_connaissances SET autoeval=?,eval_senior=?,evaluateur_senior=?,
                date_eval_senior=?,commentaire=?,updated_at=datetime('now')
                WHERE user_id=? AND ref_id=?",
               list(autoeval, eval_senior %||% NA, evaluateur_senior %||% NA,
                    date_val, commentaire %||% NA, user_id, ref_id))
  }
}

get_eval_competences <- function(user_id) {
  db_query("SELECT
              r.id              AS ref_id,
              r.domaine, r.niveau, r.libelle,
              e.id              AS eval_id,
              e.autoeval, e.eval_senior, e.evaluateur_senior,
              e.date_eval, e.commentaire, e.updated_at
            FROM ref_competences r
            LEFT JOIN eval_competences e ON e.ref_id = r.id AND e.user_id = ?
            ORDER BY r.id", list(user_id)) |>
    mutate(
      autoeval    = ifelse(is.na(autoeval), "non_evalue", autoeval),
      eval_senior = ifelse(is.na(eval_senior), NA_character_, eval_senior)
    )
}

upsert_eval_competence <- function(user_id, ref_id, autoeval, eval_senior,
                                    evaluateur_senior, date_eval, commentaire) {
  today <- format(Sys.Date(), "%d/%m/%Y")
  date_val <- if (!is.null(date_eval) && !is.na(date_eval) && nchar(date_eval) > 0) date_eval else today
  ex <- db_query("SELECT id FROM eval_competences WHERE user_id=? AND ref_id=?", list(user_id, ref_id))
  if (nrow(ex) == 0) {
    db_execute("INSERT INTO eval_competences
                (user_id,ref_id,autoeval,eval_senior,evaluateur_senior,date_eval,commentaire,updated_at)
                VALUES (?,?,?,?,?,?,?,datetime('now'))",
               list(user_id, ref_id, autoeval, eval_senior %||% NA,
                    evaluateur_senior %||% NA, date_val, commentaire %||% NA))
  } else {
    db_execute("UPDATE eval_competences SET autoeval=?,eval_senior=?,evaluateur_senior=?,
                date_eval=?,commentaire=?,updated_at=datetime('now') WHERE user_id=? AND ref_id=?",
               list(autoeval, eval_senior %||% NA, evaluateur_senior %||% NA,
                    date_val, commentaire %||% NA, user_id, ref_id))
  }
}

get_stages <- function(user_id) {
  existing <- db_query("SELECT * FROM stages WHERE user_id = ? ORDER BY semestre", list(user_id))
  n_sem <- max(N_SEMESTRES_MAX, if(nrow(existing)>0) max(existing$semestre, na.rm=TRUE) else 0)
  all_sem <- data.frame(semestre = seq_len(n_sem), stringsAsFactors = FALSE)
  merge(all_sem, existing, by = "semestre", all.x = TRUE) |>
    mutate(user_id = user_id, across(where(is.character), ~tidyr::replace_na(., "")))
}

upsert_stage <- function(user_id, semestre, periode, lieu, resp_stage,
                          travaux, valorisations, commentaire, stage_valide) {
  ex <- db_query("SELECT id FROM stages WHERE user_id=? AND semestre=?", list(user_id, semestre))
  valide_int <- switch(stage_valide %||% "en_attente",
    valide    = 1L,
    non_valide = 0L,
    en_attente = NA_integer_
  )
  if (nrow(ex) == 0) {
    db_execute("INSERT INTO stages
                (user_id,semestre,periode,lieu,responsable_stage,travaux_realises,
                 valorisations,commentaire,stage_valide,updated_at)
                VALUES (?,?,?,?,?,?,?,?,?,datetime('now'))",
               list(user_id, semestre, periode %||% NA, lieu %||% NA,
                    resp_stage %||% NA, travaux %||% NA, valorisations %||% NA,
                    commentaire %||% NA, valide_int))
  } else {
    db_execute("UPDATE stages SET periode=?,lieu=?,responsable_stage=?,travaux_realises=?,
                valorisations=?,commentaire=?,stage_valide=?,
                updated_at=datetime('now') WHERE user_id=? AND semestre=?",
               list(periode %||% NA, lieu %||% NA, resp_stage %||% NA, travaux %||% NA,
                    valorisations %||% NA, commentaire %||% NA, valide_int, user_id, semestre))
  }
}

get_phases <- function(user_id) {
  db_query("SELECT * FROM phases_validation WHERE user_id = ? ORDER BY
            CASE phase WHEN 'socle' THEN 1 WHEN 'approfondissement' THEN 2 ELSE 3 END",
           list(user_id))
}
upsert_phase <- function(user_id, phase, avis, commentaire, date_val, validateur) {
  ex <- db_query("SELECT id FROM phases_validation WHERE user_id=? AND phase=?", list(user_id, phase))
  if (nrow(ex) == 0) {
    db_execute("INSERT INTO phases_validation (user_id,phase,avis_commission,commentaire,date_validation,validateur,updated_at)
                VALUES (?,?,?,?,?,?,datetime('now'))",
               list(user_id, phase, avis, commentaire, date_val, validateur))
  } else {
    db_execute("UPDATE phases_validation SET avis_commission=?,commentaire=?,date_validation=?,validateur=?,
                updated_at=datetime('now') WHERE user_id=? AND phase=?",
               list(avis, commentaire, date_val, validateur, user_id, phase))
  }
}

# ── Helpers visualisation ─────────────────────────────────────────────────────
# ── Helpers diplômes ──────────────────────────────────────────────────────────
get_diplomes <- function(user_id) {
  db_query("SELECT * FROM diplomes WHERE user_id = ? ORDER BY id", list(user_id))
}
save_diplome <- function(user_id, id_diplome, type_d, intitule, universite, annee) {
  if (is.null(id_diplome) || is.na(id_diplome)) {
    db_execute("INSERT INTO diplomes (user_id,type_diplome,intitule,universite,annee,updated_at)
                VALUES (?,?,?,?,?,datetime('now'))",
               list(user_id, type_d, intitule, universite, annee))
  } else {
    db_execute("UPDATE diplomes SET type_diplome=?,intitule=?,universite=?,annee=?,
                updated_at=datetime('now') WHERE id=? AND user_id=?",
               list(type_d, intitule, universite, annee, id_diplome, user_id))
  }
}
delete_diplome <- function(id_diplome, user_id) {
  db_execute("DELETE FROM diplomes WHERE id=? AND user_id=?", list(id_diplome, user_id))
}

# ── Opérateur null-coalesce ───────────────────────────────────────────────────
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && (is.na(a) || a == "")) return(b)
  a
}

# ── Chargement modules ────────────────────────────────────────────────────────
source("modules/mod_auth.R")
source("modules/mod_identite.R")
source("modules/mod_contrat.R")
source("modules/mod_connaissances.R")
source("modules/mod_competences.R")
source("modules/mod_stages.R")
source("modules/mod_phases.R")
source("modules/mod_dashboard.R")
source("modules/mod_export.R")
source("modules/mod_admin.R")
source("modules/mod_change_pw.R")

# ── Constantes ajoutées ────────────────────────────────────────────────────────
EVAL_SENIOR_CHOICES <- c(
  "—"          = "",
  "Non acquis" = "non_acquis",
  "Acquis"     = "acquis"
)
