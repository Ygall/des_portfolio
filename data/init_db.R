# init_db.R — Schéma SQLite + amorçage (migrations idempotentes)
source("data/referentiel.R")

FACULTES_MEDECINE <- c(
  "Aix-Marseille Université","Université d'Amiens","Université d'Angers",
  "Université de Besançon","Université de Bordeaux","Université de Brest",
  "Université de Caen","Université de Clermont-Ferrand","Université de Dijon",
  "Université de Grenoble","Université de Lille","Université de Limoges",
  "Université de Lyon 1","Université de Montpellier","Université de Nancy",
  "Université de Nantes","Université de Nice","Université de Paris Cité",
  "Université Paris Saclay","Université Sorbonne Paris Nord",
  "Université Paris-Est Créteil","Université Versailles Saint-Quentin",
  "Université de Poitiers","Université de Reims","Université de Rennes 1",
  "Université de Rouen","Université de Saint-Étienne","Université de Strasbourg",
  "Université de Tours","Université de Toulouse III","Université des Antilles",
  "Université de La Réunion","Autre"
)

init_database <- function(db_path = "data/portfolio_des.sqlite") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")

  # ── users ──────────────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      username   TEXT UNIQUE NOT NULL,
      nom        TEXT NOT NULL DEFAULT '',
      prenom     TEXT NOT NULL DEFAULT '',
      email      TEXT NOT NULL DEFAULT '',
      role       TEXT NOT NULL DEFAULT 'interne'
                       CHECK(role IN ('interne','coordinateur','admin')),
      statut     TEXT NOT NULL DEFAULT 'actif'
                       CHECK(statut IN ('actif','internat_termine')),
      hashed_pw  TEXT,
      faculte    TEXT NOT NULL DEFAULT '',
      promotion  TEXT NOT NULL DEFAULT '',
      active     INTEGER NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL DEFAULT (datetime('now'))
    );")
  .add_col(con, "users", "statut", "TEXT NOT NULL DEFAULT 'actif'")

  # ── ref_connaissances ──────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_connaissances (
      id      INTEGER PRIMARY KEY,
      domaine TEXT NOT NULL,
      niveau  TEXT NOT NULL CHECK(niveau IN ('Base','Avancé')),
      libelle TEXT NOT NULL
    );")

  # ── ref_competences ────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_competences (
      id      INTEGER PRIMARY KEY,
      domaine TEXT NOT NULL,
      niveau  TEXT NOT NULL CHECK(niveau IN ('Base','Avancé')),
      libelle TEXT NOT NULL
    );")

  # ── eval_connaissances ─────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS eval_connaissances (
      id                INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id           INTEGER NOT NULL REFERENCES users(id),
      ref_id            INTEGER NOT NULL REFERENCES ref_connaissances(id),
      autoeval          TEXT NOT NULL DEFAULT 'non_evalue'
                             CHECK(autoeval IN ('non_evalue','non_acquis','acquis')),
      eval_senior       TEXT DEFAULT NULL
                             CHECK(eval_senior IN ('non_acquis','acquis') OR eval_senior IS NULL),
      evaluateur_senior TEXT,
      date_eval_senior  TEXT,
      commentaire       TEXT,
      updated_at        TEXT NOT NULL DEFAULT (datetime('now')),
      UNIQUE(user_id, ref_id)
    );")
  .add_col(con, "eval_connaissances", "eval_senior",       "TEXT DEFAULT NULL")
  .add_col(con, "eval_connaissances", "evaluateur_senior", "TEXT")
  .add_col(con, "eval_connaissances", "date_eval_senior",  "TEXT")
  # Migrate old column names
  tryCatch({
    ecols <- DBI::dbGetQuery(con, "PRAGMA table_info(eval_connaissances)")$name
    if ("eval_hu" %in% ecols) {
      DBI::dbExecute(con, "UPDATE eval_connaissances SET
        eval_senior = COALESCE(eval_senior, eval_hu),
        evaluateur_senior = COALESCE(evaluateur_senior, evaluateur_hu),
        date_eval_senior  = COALESCE(date_eval_senior, date_eval_hu)
        WHERE eval_senior IS NULL AND eval_hu IS NOT NULL")
    }
  }, error = function(e) NULL)

  # ── eval_competences ───────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS eval_competences (
      id                INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id           INTEGER NOT NULL REFERENCES users(id),
      ref_id            INTEGER NOT NULL REFERENCES ref_competences(id),
      autoeval          TEXT NOT NULL DEFAULT 'non_evalue'
                             CHECK(autoeval IN ('non_evalue','non_acquis','en_cours','acquis')),
      eval_senior       TEXT DEFAULT NULL
                             CHECK(eval_senior IN ('non_acquis','en_cours','acquis') OR eval_senior IS NULL),
      evaluateur_senior TEXT,
      date_eval         TEXT,
      commentaire       TEXT,
      updated_at        TEXT NOT NULL DEFAULT (datetime('now')),
      UNIQUE(user_id, ref_id)
    );")
  .add_col(con, "eval_competences", "eval_senior",       "TEXT DEFAULT NULL")
  .add_col(con, "eval_competences", "evaluateur_senior", "TEXT")
  .add_col(con, "eval_competences", "date_eval",         "TEXT")
  tryCatch({
    ccols <- DBI::dbGetQuery(con, "PRAGMA table_info(eval_competences)")$name
    if ("eval_responsable" %in% ccols) {
      DBI::dbExecute(con, "UPDATE eval_competences SET
        eval_senior = COALESCE(eval_senior, eval_responsable),
        evaluateur_senior = COALESCE(evaluateur_senior, evaluateur_responsable)
        WHERE eval_senior IS NULL AND eval_responsable IS NOT NULL")
    }
  }, error = function(e) NULL)

  # ── stages ─────────────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stages (
      id                INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id           INTEGER NOT NULL REFERENCES users(id),
      semestre          INTEGER NOT NULL CHECK(semestre >= 1),
      periode           TEXT,
      lieu              TEXT,
      responsable_stage TEXT,
      travaux_realises  TEXT,
      valorisations     TEXT,
      commentaire       TEXT,
      updated_at        TEXT NOT NULL DEFAULT (datetime('now')),
      UNIQUE(user_id, semestre)
    );")
  .add_col(con, "stages", "periode",           "TEXT")
  .add_col(con, "stages", "responsable_stage", "TEXT")
  .add_col(con, "stages", "travaux_realises",  "TEXT")
  .add_col(con, "stages", "valorisations",     "TEXT")
  .add_col(con, "stages", "commentaire",       "TEXT")
  tryCatch({
    DBI::dbExecute(con, "UPDATE stages SET responsable_stage = COALESCE(responsable_stage, responsable_medical)
                         WHERE responsable_stage IS NULL AND responsable_medical IS NOT NULL")
  }, error = function(e) NULL)

  # ── identite ───────────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS identite (
      id               INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id          INTEGER NOT NULL UNIQUE REFERENCES users(id),
      nom              TEXT,
      prenom           TEXT,
      date_naissance   TEXT,
      faculte_2e_cycle TEXT,
      annee_edn        TEXT,
      des_initial      TEXT,
      faculte_3e_cycle TEXT,
      updated_at       TEXT NOT NULL DEFAULT (datetime('now'))
    );")
  .add_col(con, "identite", "annee_edn", "TEXT")
  tryCatch({
    DBI::dbExecute(con, "UPDATE identite SET annee_edn = COALESCE(annee_edn, annee_iecn)
                         WHERE annee_edn IS NULL AND annee_iecn IS NOT NULL")
  }, error = function(e) NULL)

  # ── diplomes ───────────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS diplomes (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id      INTEGER NOT NULL REFERENCES users(id),
      type_diplome TEXT NOT NULL DEFAULT 'Autres'
                        CHECK(type_diplome IN ('DU/DIU','M1','M2','Autres')),
      intitule     TEXT,
      universite   TEXT,
      annee        TEXT,
      updated_at   TEXT NOT NULL DEFAULT (datetime('now'))
    );")

  # ── contrat_formation ──────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS contrat_formation (
      id                   INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id              INTEGER NOT NULL UNIQUE REFERENCES users(id),
      projet_professionnel TEXT,
      obj_connaissances    TEXT,
      obj_competences      TEXT,
      these_statut         TEXT DEFAULT 'non_debutee',
      these_sujet          TEXT,
      these_directeur      TEXT,
      these_date           TEXT,
      updated_at           TEXT NOT NULL DEFAULT (datetime('now'))
    );")
  .add_col(con, "contrat_formation", "these_statut",    "TEXT DEFAULT 'non_debutee'")
  .add_col(con, "contrat_formation", "these_sujet",     "TEXT")
  .add_col(con, "contrat_formation", "these_directeur", "TEXT")
  .add_col(con, "contrat_formation", "these_date",      "TEXT")

  # ── phases_validation ──────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS phases_validation (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id         INTEGER NOT NULL REFERENCES users(id),
      phase           TEXT NOT NULL
                           CHECK(phase IN ('socle','approfondissement','consolidation')),
      avis_commission TEXT,
      commentaire     TEXT,
      date_validation TEXT,
      validateur      TEXT,
      updated_at      TEXT NOT NULL DEFAULT (datetime('now')),
      UNIQUE(user_id, phase)
    );")

  # ── Amorçage référentiels ──────────────────────────────────────────────────
  if (DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM ref_connaissances")$n == 0)
    DBI::dbWriteTable(con, "ref_connaissances",
                      REF_CONNAISSANCES[, c("id","domaine","niveau","libelle")], append = TRUE)
  if (DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM ref_competences")$n == 0)
    DBI::dbWriteTable(con, "ref_competences",
                      REF_COMPETENCES[, c("id","domaine","niveau","libelle")], append = TRUE)

  # ── Comptes démo ───────────────────────────────────────────────────────────
  if (DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n == 0) {
    pw <- function(p) if (requireNamespace("bcrypt",quietly=TRUE)) bcrypt::hashpw(p) else "CHANGE_ME"
    for (r in list(
      list("admin",    "Admin",  "Super", "admin@univ.fr", "admin",        "actif", "Démo",  "2024"),
      list("coord1",   "Dupont", "Marie", "coord@univ.fr", "coordinateur", "actif", "Paris", "2024"),
      list("interne1", "Martin", "Paul",  "paul@univ.fr",  "interne",      "actif", "Paris", "2024")
    )) DBI::dbExecute(con,
      "INSERT INTO users(username,nom,prenom,email,role,statut,hashed_pw,faculte,promotion)
       VALUES(?,?,?,?,?,?,?,?,?)",
      c(r[1:6], list(pw(paste0(r[[1]],"123"))), r[7:8]))
    message("Comptes demo: admin/admin123  coord1/coord123  interne1/interne123")
  }

  invisible(TRUE)
}

# Helper: ajouter une colonne si absente (idempotent)
.add_col <- function(con, table, col, def) {
  cols <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(",table,")"))$name
  if (!col %in% cols)
    tryCatch(DBI::dbExecute(con, paste0("ALTER TABLE ",table," ADD COLUMN ",col," ",def)),
             error = function(e) NULL)
}

# ── init_database_pg : amorçage PostgreSQL ─────────────────────────────────────
# Syntaxe PostgreSQL : SERIAL, TEXT (pas de CHECK avec SQLite-style), BOOLEAN
init_database_pg <- function() {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))

  # users
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id         SERIAL PRIMARY KEY,
      username   TEXT UNIQUE NOT NULL,
      nom        TEXT NOT NULL DEFAULT '',
      prenom     TEXT NOT NULL DEFAULT '',
      email      TEXT NOT NULL DEFAULT '',
      role       TEXT NOT NULL DEFAULT 'interne',
      statut     TEXT NOT NULL DEFAULT 'actif',
      hashed_pw  TEXT,
      faculte    TEXT NOT NULL DEFAULT '',
      promotion  TEXT NOT NULL DEFAULT '',
      active     INTEGER NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL DEFAULT NOW()::TEXT
    );")

  # ref_connaissances
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_connaissances (
      id      INTEGER PRIMARY KEY,
      domaine TEXT NOT NULL,
      niveau  TEXT NOT NULL,
      libelle TEXT NOT NULL
    );")

  # ref_competences
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_competences (
      id      INTEGER PRIMARY KEY,
      domaine TEXT NOT NULL,
      niveau  TEXT NOT NULL,
      libelle TEXT NOT NULL
    );")

  # eval_connaissances
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS eval_connaissances (
      id                SERIAL PRIMARY KEY,
      user_id           INTEGER NOT NULL REFERENCES users(id),
      ref_id            INTEGER NOT NULL REFERENCES ref_connaissances(id),
      autoeval          TEXT NOT NULL DEFAULT 'non_evalue',
      eval_senior       TEXT DEFAULT NULL,
      evaluateur_senior TEXT,
      date_eval_senior  TEXT,
      commentaire       TEXT,
      updated_at        TEXT NOT NULL DEFAULT NOW()::TEXT,
      UNIQUE(user_id, ref_id)
    );")

  # eval_competences
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS eval_competences (
      id                SERIAL PRIMARY KEY,
      user_id           INTEGER NOT NULL REFERENCES users(id),
      ref_id            INTEGER NOT NULL REFERENCES ref_competences(id),
      autoeval          TEXT NOT NULL DEFAULT 'non_evalue',
      eval_senior       TEXT DEFAULT NULL,
      evaluateur_senior TEXT,
      date_eval         TEXT,
      commentaire       TEXT,
      updated_at        TEXT NOT NULL DEFAULT NOW()::TEXT,
      UNIQUE(user_id, ref_id)
    );")

  # stages
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stages (
      id                SERIAL PRIMARY KEY,
      user_id           INTEGER NOT NULL REFERENCES users(id),
      semestre          INTEGER NOT NULL,
      periode           TEXT,
      lieu              TEXT,
      responsable_stage TEXT,
      travaux_realises  TEXT,
      valorisations     TEXT,
      commentaire       TEXT,
      updated_at        TEXT NOT NULL DEFAULT NOW()::TEXT,
      UNIQUE(user_id, semestre)
    );")

  # identite
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS identite (
      id               SERIAL PRIMARY KEY,
      user_id          INTEGER NOT NULL UNIQUE REFERENCES users(id),
      nom              TEXT,
      prenom           TEXT,
      date_naissance   TEXT,
      faculte_2e_cycle TEXT,
      annee_edn        TEXT,
      des_initial      TEXT,
      faculte_3e_cycle TEXT,
      updated_at       TEXT NOT NULL DEFAULT NOW()::TEXT
    );")

  # diplomes
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS diplomes (
      id           SERIAL PRIMARY KEY,
      user_id      INTEGER NOT NULL REFERENCES users(id),
      type_diplome TEXT NOT NULL DEFAULT 'Autres',
      intitule     TEXT,
      universite   TEXT,
      annee        TEXT,
      updated_at   TEXT NOT NULL DEFAULT NOW()::TEXT
    );")

  # contrat_formation
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS contrat_formation (
      id                   SERIAL PRIMARY KEY,
      user_id              INTEGER NOT NULL UNIQUE REFERENCES users(id),
      projet_professionnel TEXT,
      obj_connaissances    TEXT,
      obj_competences      TEXT,
      these_statut         TEXT DEFAULT 'non_debutee',
      these_sujet          TEXT,
      these_directeur      TEXT,
      these_date           TEXT,
      updated_at           TEXT NOT NULL DEFAULT NOW()::TEXT
    );")

  # phases_validation
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS phases_validation (
      id              SERIAL PRIMARY KEY,
      user_id         INTEGER NOT NULL REFERENCES users(id),
      phase           TEXT NOT NULL,
      avis_commission TEXT,
      commentaire     TEXT,
      date_validation TEXT,
      validateur      TEXT,
      updated_at      TEXT NOT NULL DEFAULT NOW()::TEXT,
      UNIQUE(user_id, phase)
    );")

  # Amorçage référentiels
  n_conn <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM ref_connaissances")$n
  if (n_conn == 0) {
    DBI::dbWriteTable(con, "ref_connaissances",
                      REF_CONNAISSANCES[, c("id","domaine","niveau","libelle")],
                      append = TRUE)
    message(sprintf("PG: ref_connaissances amorcé (%d items)", nrow(REF_CONNAISSANCES)))
  }

  n_comp <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM ref_competences")$n
  if (n_comp == 0) {
    DBI::dbWriteTable(con, "ref_competences",
                      REF_COMPETENCES[, c("id","domaine","niveau","libelle")],
                      append = TRUE)
    message(sprintf("PG: ref_competences amorcé (%d items)", nrow(REF_COMPETENCES)))
  }

  # Compte admin par défaut
  n_users <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n
  if (n_users == 0) {
    pw <- function(p) if (requireNamespace("bcrypt",quietly=TRUE)) bcrypt::hashpw(p) else "CHANGE_ME"
    for (r in list(
      list("admin",    "Admin",  "Super", "admin@univ.fr", "admin",        "actif", "Démo",  "2024"),
      list("coord1",   "Dupont", "Marie", "coord@univ.fr", "coordinateur", "actif", "Paris", "2024"),
      list("interne1", "Martin", "Paul",  "paul@univ.fr",  "interne",      "actif", "Paris", "2024")
    )) DBI::dbExecute(con,
      "INSERT INTO users(username,nom,prenom,email,role,statut,hashed_pw,faculte,promotion)
       VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9)",
      c(r[1:6], list(pw(paste0(r[[1]],"123"))), r[7:8]))
    message("PG: comptes démo créés")
  }

  invisible(TRUE)
}
