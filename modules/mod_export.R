# modules/mod_export.R — Export HTML + CSV + Excel
# ─────────────────────────────────────────────────────────────────────────────

mod_export_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Export du portfolio individuel", width = 6,
        status = "primary", solidHeader = TRUE,
        uiOutput(ns("banner_interne")),
        br(),
        p(style = "color:#555;",
          icon("info-circle"),
          " L'export génère un fichier HTML autonome et lisible, incluant toutes les sections.
          Chaque export est horodaté — conservez les fichiers téléchargés comme traces de votre progression."),
        downloadButton(ns("dl_html"), "Télécharger le portfolio HTML",
                       class = "btn-primary btn-block btn-lg"),
        br(),
        downloadButton(ns("dl_csv_individuel"), "Télécharger CSV brut",
                       class = "btn-default btn-block")
    ),
    uiOutput(ns("panel_pilotage"))
  )
}

mod_export_server <- function(id, user, user_id_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner_interne <- renderUI({
      req(user())
      if (user()$role == "interne") return(NULL)
      uid <- user_id_r()
      if (is.null(uid))
        return(div(class="alert alert-warning",
                   icon("triangle-exclamation"),
                   " Aucun interne sélectionné dans la barre latérale."))
      row <- db_query("SELECT nom, prenom FROM users WHERE id=?", list(uid))
      if (nrow(row)==0) return(NULL)
      div(class="alert alert-info",
          icon("user"), " Export pour : ", strong(paste(row$prenom, row$nom)))
    })

    output$panel_pilotage <- renderUI({
      req(user())
      if (!user()$role %in% c("coordinateur","admin")) return(NULL)
      box(title="Export pilotage pédagogique", width=6,
          status="warning", solidHeader=TRUE,
          selectInput(ns("promo_export"), "Promotion",
                      choices = {
                        promos <- db_query(
                          "SELECT DISTINCT promotion FROM users WHERE role='interne' AND active=1")$promotion
                        c("Toutes"="toutes", setNames(promos,promos))
                      }),
          checkboxInput(ns("include_termine_exp"),"Inclure internes ayant terminé",FALSE),
          downloadButton(ns("dl_csv_promo"), "CSV promotion", class="btn-warning btn-block"),
          br(),
          downloadButton(ns("dl_excel"), "Excel multi-onglets", class="btn-success btn-block"))
    })

    # ── Génération HTML portfolio ──────────────────────────────────────────────
    .build_html <- function(uid, user_info) {
      id_data  <- get_identite(uid)
      dip      <- get_diplomes(uid)
      contrat  <- get_contrat(uid)
      dc       <- get_eval_connaissances(uid)
      dco      <- get_eval_competences(uid)
      ds       <- get_stages(uid)
      dp       <- get_phases(uid)

      nom_complet <- if (nrow(id_data)>0 && nchar(id_data$nom[1]%||%"")>0)
        paste(id_data$prenom[1]%||%"", id_data$nom[1]%||%"")
      else paste(user_info$prenom, user_info$nom)

      date_export <- format(Sys.time(), "%d/%m/%Y à %H:%M")

      nn <- function(x) if(is.null(x)||is.na(x)||identical(x,"")) "—" else as.character(x)

      # ── helpers HTML ─────────────────────────────────────────────────────────
      badge <- function(s, type="conn") {
        if (type=="conn") {
          col <- switch(s%||%"non_evalue",
            non_evalue="#6c757d", non_acquis="#dc3545", acquis="#198754", "#6c757d")
          lbl <- switch(s%||%"non_evalue",
            non_evalue="Non évalué", non_acquis="Non acquis", acquis="Acquis", "—")
        } else {
          col <- switch(s%||%"non_evalue",
            non_evalue="#6c757d", non_acquis="#dc3545", en_cours="#fd7e14",
            acquis="#198754", "#6c757d")
          lbl <- switch(s%||%"non_evalue",
            non_evalue="Non évalué", non_acquis="Non acquis",
            en_cours="En cours", acquis="Acquis", "—")
        }
        sprintf('<span style="background:%s;color:#fff;padding:2px 8px;border-radius:12px;font-size:.8em;white-space:nowrap;">%s</span>', col, lbl)
      }

      tbl_html <- function(headers, rows_list, id="") {
        th <- paste0("<th>", headers, "</th>", collapse="")
        trs <- paste0(sapply(rows_list, function(r) {
          paste0("<tr>", paste0("<td>", r, "</td>", collapse=""), "</tr>")
        }), collapse="\n")
        sprintf('<div class="table-wrap"><table id="%s"><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>',
                id, th, trs)
      }

      progress_bar <- function(n_acq, n_tot, color="#198754") {
        pct <- if(n_tot>0) round(100*n_acq/n_tot) else 0
        sprintf('<div class="prog-wrap"><div class="prog-bar" style="width:%d%%;background:%s;"></div></div>
                 <span class="prog-label">%d / %d (%d%%)</span>',
                pct, color, n_acq, n_tot, pct)
      }

      # ── Section identité ──────────────────────────────────────────────────────
      html_identite <- ""
      if (nrow(id_data)>0) {
        html_identite <- sprintf('
<section id="identite">
  <h2>Fiche d\'identité</h2>
  <table class="info-table">
    <tr><th>Nom</th><td>%s</td><th>Prénom</th><td>%s</td></tr>
    <tr><th>Date de naissance</th><td>%s</td><th>Faculté 2ème cycle</th><td>%s</td></tr>
    <tr><th>Année ECN/EDN</th><td>%s</td><th>DES initial</th><td>%s</td></tr>
    <tr><th>Faculté 3ème cycle</th><td colspan="3">%s</td></tr>
  </table>',
          nn(id_data$nom[1]), nn(id_data$prenom[1]),
          nn(id_data$date_naissance[1]), nn(id_data$faculte_2e_cycle[1]),
          nn(id_data$annee_edn[1]), nn(id_data$des_initial[1]),
          nn(id_data$faculte_3e_cycle[1]))

        if (nrow(dip)>0) {
          dip_rows <- lapply(seq_len(nrow(dip)), function(i)
            c(nn(dip$type_diplome[i]), nn(dip$intitule[i]),
              nn(dip$universite[i]), nn(dip$annee[i])))
          html_identite <- paste0(html_identite,
            "<h3>Diplômes</h3>",
            tbl_html(c("Type","Intitulé","Université","Année"), dip_rows))
        }
        html_identite <- paste0(html_identite, "</section>")
      }

      # ── Section contrat ───────────────────────────────────────────────────────
      html_contrat <- ""
      if (nrow(contrat)>0) {
        statut_these <- switch(contrat$these_statut[1]%||%"non_debutee",
          non_debutee="Non débutée", sujet_en_cours="Sujet en cours",
          sujet_valide="Sujet validé", en_cours="En cours de réalisation",
          validee="Validée", "—")
        html_contrat <- sprintf('
<section id="contrat">
  <h2>Contrat de formation</h2>
  <h3>Projet professionnel</h3>
  <div class="text-block">%s</div>
  <h3>Thèse de médecine</h3>
  <table class="info-table">
    <tr><th>Statut</th><td>%s</td><th>Date soutenance</th><td>%s</td></tr>
    <tr><th>Sujet</th><td colspan="3">%s</td></tr>
    <tr><th>Directeur</th><td colspan="3">%s</td></tr>
  </table>
  <h3>Objectifs — Connaissances</h3>
  <div class="text-block">%s</div>
  <h3>Objectifs — Compétences</h3>
  <div class="text-block">%s</div>
</section>',
          nn(contrat$projet_professionnel[1]), statut_these,
          nn(contrat$these_date[1]), nn(contrat$these_sujet[1]),
          nn(contrat$these_directeur[1]),
          nn(contrat$obj_connaissances[1]), nn(contrat$obj_competences[1]))
      }

      # ── Section connaissances ─────────────────────────────────────────────────
      html_conn <- '<section id="connaissances"><h2>Connaissances</h2>'
      if (nrow(dc)>0) {
        n_acq <- sum(dc$autoeval=="acquis", na.rm=TRUE)
        html_conn <- paste0(html_conn,
          "<div class='prog-section'>", progress_bar(n_acq, nrow(dc)), "</div>")
        for (dom in unique(dc$domaine)) {
          sub_d  <- dc[dc$domaine==dom,]
          alias  <- DOMAINE_ALIAS[dom] %||% dom
          n_d    <- sum(sub_d$autoeval=="acquis", na.rm=TRUE)
          rows   <- lapply(seq_len(nrow(sub_d)), function(i)
            c(sub_d$niveau[i],
              paste0(toupper(substr(sub_d$libelle[i],1,1)),
                     substr(sub_d$libelle[i],2,nchar(sub_d$libelle[i]))),
              badge(sub_d$autoeval[i],"conn"),
              badge(sub_d$eval_senior[i]%||%"","conn"),
              nn(sub_d$evaluateur_senior[i]),
              nn(sub_d$date_eval_senior[i]),
              nn(sub_d$commentaire[i])))
          html_conn <- paste0(html_conn,
            sprintf('<h3>%s <span class="domain-count">%d/%d acquis</span></h3>', alias, n_d, nrow(sub_d)),
            tbl_html(c("Niv.","Connaissance","Auto-éval","Val. Senior","Évaluateur","Date","Commentaire"),
                     rows))
        }
      }
      html_conn <- paste0(html_conn, "</section>")

      # ── Section compétences ───────────────────────────────────────────────────
      html_comp <- '<section id="competences"><h2>Compétences</h2>'
      if (nrow(dco)>0) {
        n_acq <- sum(dco$autoeval %in% c("acquis","en_cours"), na.rm=TRUE)
        html_comp <- paste0(html_comp,
          "<div class='prog-section'>", progress_bar(n_acq, nrow(dco), "#fd7e14"), "</div>")
        for (dom in unique(dco$domaine)) {
          sub_d <- dco[dco$domaine==dom,]
          alias <- DOMAINE_ALIAS[dom] %||% dom
          rows  <- lapply(seq_len(nrow(sub_d)), function(i)
            c(sub_d$niveau[i],
              paste0(toupper(substr(sub_d$libelle[i],1,1)),
                     substr(sub_d$libelle[i],2,nchar(sub_d$libelle[i]))),
              badge(sub_d$autoeval[i],"comp"),
              badge(sub_d$eval_senior[i]%||%"","comp"),
              nn(sub_d$evaluateur_senior[i]),
              nn(sub_d$date_eval[i]),
              nn(sub_d$commentaire[i])))
          html_comp <- paste0(html_comp,
            sprintf('<h3>%s</h3>', alias),
            tbl_html(c("Niv.","Compétence","Auto-éval","Éval. Senior","Évaluateur","Date","Commentaire"),
                     rows))
        }
      }
      html_comp <- paste0(html_comp, "</section>")

      # ── Section stages ────────────────────────────────────────────────────────
      html_stages <- '<section id="stages"><h2>Carnet de stages</h2>'
      if (nrow(ds)>0) {
        stage_rows <- lapply(seq_len(nrow(ds)), function(i)
          c(paste("S",ds$semestre[i]),
            nn(ds$periode[i]), nn(ds$lieu[i]),
            nn(ds$responsable_stage[i]),
            nn(ds$travaux_realises[i]),
            nn(ds$valorisations[i]),
            nn(ds$commentaire[i])))
        html_stages <- paste0(html_stages,
          tbl_html(c("Sem.","Période","Lieu","Responsable","Travaux","Valorisations","Commentaire"),
                   stage_rows, "tbl-stages"))
      }
      html_stages <- paste0(html_stages, "</section>")

      # ── Section phases ────────────────────────────────────────────────────────
      phase_labels <- c(socle="Phase Socle",
                        approfondissement="Phase d'Approfondissement",
                        consolidation="Phase de Consolidation")
      html_phases <- '<section id="phases"><h2>Validation des phases</h2>'
      for (ph in c("socle","approfondissement","consolidation")) {
        row <- if(nrow(dp)>0) dp[dp$phase==ph,] else data.frame()
        avis_col <- if(nrow(row)>0) {
          switch(row$avis_commission[1]%||%"",
            valide   = "background:#d4edda;color:#155724;",
            non_valide="background:#fde2e2;color:#721c24;",
            ajourne  = "background:#fff3cd;color:#856404;",
            "background:#f8f9fa;color:#495057;"
          )
        } else "background:#f8f9fa;color:#495057;"
        avis_lbl <- if(nrow(row)>0)
          switch(row$avis_commission[1]%||%"",
            valide="Validé",non_valide="Non validé",ajourne="Ajourné","En attente")
        else "En attente"
        html_phases <- paste0(html_phases, sprintf('
<div class="phase-card" style="%s">
  <div class="phase-title">%s</div>
  <div class="phase-avis">%s</div>',
          avis_col, phase_labels[[ph]], avis_lbl))
        if(nrow(row)>0) {
          if(nchar(row$date_validation[1]%||%"")>0)
            html_phases <- paste0(html_phases,
              sprintf('<div class="phase-info">Date : %s — Signataire : %s</div>',
                      nn(row$date_validation[1]), nn(row$validateur[1])))
          if(!is.na(row$commentaire[1])&&nchar(row$commentaire[1])>0)
            html_phases <- paste0(html_phases,
              sprintf('<div class="phase-comment">%s</div>', row$commentaire[1]))
        }
        html_phases <- paste0(html_phases, "</div>")
      }
      html_phases <- paste0(html_phases, "</section>")

      # ── Assemblage HTML final ─────────────────────────────────────────────────
      sprintf('<!DOCTYPE html>
<html lang="fr">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Portfolio DES SP — %s</title>
<style>
*{box-sizing:border-box;margin:0;padding:0}
body{font-family:\'Segoe UI\',Arial,sans-serif;font-size:14px;color:#212529;background:#f4f6f8;display:flex}
#toc{position:fixed;top:0;left:0;width:200px;height:100vh;background:#1a2a4a;overflow-y:auto;padding:16px 0;z-index:100}
#toc h1{font-size:.85em;color:#7fb3c8;padding:8px 16px 12px;text-transform:uppercase;letter-spacing:.08em;border-bottom:1px solid rgba(255,255,255,.1);margin-bottom:8px}
#toc a{display:block;padding:7px 16px;color:#b8c7ce;font-size:.82em;text-decoration:none;border-left:3px solid transparent;transition:all .15s}
#toc a:hover,#toc a.active{color:#fff;border-left-color:#2980b9;background:rgba(41,128,185,.2)}
#content{margin-left:200px;padding:24px 32px;max-width:1100px;width:100%%}
.meta{background:#fff;border-radius:8px;padding:16px 20px;margin-bottom:20px;border-left:4px solid #2980b9;font-size:.9em;color:#555}
.meta strong{color:#1a2a4a;font-size:1.05em;display:block;margin-bottom:4px}
section{background:#fff;border-radius:8px;padding:24px;margin-bottom:24px;box-shadow:0 1px 4px rgba(0,0,0,.06)}
h2{font-size:1.3em;color:#1a2a4a;margin-bottom:16px;padding-bottom:8px;border-bottom:2px solid #e9ecef}
h3{font-size:1em;color:#2980b9;margin:16px 0 8px;font-weight:600}
.domain-count{font-size:.78em;color:#6c757d;font-weight:400;margin-left:6px}
.info-table{width:100%%;border-collapse:collapse;margin-bottom:12px}
.info-table th{background:#f8f9fa;padding:7px 10px;font-weight:600;color:#495057;text-align:left;width:18%%;font-size:.85em;white-space:nowrap}
.info-table td{padding:7px 10px;border-bottom:1px solid #f0f0f0;font-size:.88em}
.table-wrap{overflow-x:auto;margin-bottom:16px}
table:not(.info-table){width:100%%;border-collapse:collapse;font-size:.82em}
table:not(.info-table) thead tr{background:#1a2a4a;color:#fff}
table:not(.info-table) th{padding:8px 10px;text-align:left;font-weight:500}
table:not(.info-table) td{padding:7px 10px;border-bottom:1px solid #f0f0f0;vertical-align:top}
table:not(.info-table) tr:hover{background:#f8f9fa}
.text-block{background:#f8f9fa;border-left:3px solid #dee2e6;padding:10px 14px;border-radius:0 4px 4px 0;font-size:.9em;line-height:1.6;white-space:pre-wrap;min-height:40px}
.prog-section{display:flex;align-items:center;gap:12px;margin-bottom:16px}
.prog-wrap{flex:1;background:#e9ecef;border-radius:100px;height:10px;overflow:hidden}
.prog-bar{height:100%%;border-radius:100px;transition:width .4s}
.prog-label{font-size:.85em;color:#495057;white-space:nowrap}
.phase-card{border-radius:6px;padding:14px 18px;margin-bottom:10px}
.phase-title{font-weight:600;font-size:1em;margin-bottom:4px}
.phase-avis{font-size:.9em;font-weight:500}
.phase-info{font-size:.8em;margin-top:4px;opacity:.8}
.phase-comment{font-size:.8em;margin-top:6px;font-style:italic;opacity:.85}
@media print{#toc{display:none}#content{margin-left:0}}
</style>
</head>
<body>
<nav id="toc">
  <h1>e-Portfolio</h1>
  <a href="#identite">Identité</a>
  <a href="#contrat">Contrat</a>
  <a href="#connaissances">Connaissances</a>
  <a href="#competences">Compétences</a>
  <a href="#stages">Stages</a>
  <a href="#phases">Phases</a>
</nav>
<div id="content">
  <div class="meta">
    <strong>e-Portfolio DES Santé Publique — %s</strong>
    Exporté le %s | Référentiel pédagogique 2016 (CUESP/CIMES/CLISP)
  </div>
  %s
  %s
  %s
  %s
  %s
  %s
</div>
<script>
const sections=document.querySelectorAll("section[id]");
const links=document.querySelectorAll("#toc a");
window.addEventListener("scroll",()=>{
  let cur="";
  sections.forEach(s=>{if(window.scrollY>=s.offsetTop-80)cur=s.id;});
  links.forEach(l=>{l.classList.toggle("active",l.getAttribute("href")==="#"+cur);});
});
</script>
</body></html>',
        nom_complet, nom_complet, date_export,
        html_identite, html_contrat, html_conn,
        html_comp, html_stages, html_phases)
    }

    # ── Download HTML ──────────────────────────────────────────────────────────
    output$dl_html <- downloadHandler(
      filename = function() {
        uid <- user_id_r()
        row <- tryCatch(db_query("SELECT nom,prenom FROM users WHERE id=?",list(uid)),
                        error=function(e)data.frame())
        base <- if(nrow(row)>0)
          gsub("[^a-zA-Z0-9]","_",iconv(paste0(row$nom[1],"_",row$prenom[1]),to="ASCII//TRANSLIT"))
        else "portfolio"
        paste0("portfolio_", base, "_", format(Sys.Date(),"%Y%m%d"), ".html")
      },
      content = function(file) {
        uid <- req(user_id_r())
        tryCatch({
          html <- .build_html(uid, user())
          writeLines(html, file, useBytes=FALSE)
        }, error=function(e){
          writeLines(paste("<html><body><h1>Erreur</h1><p>",e$message,"</p></body></html>"),file)
        })
      },
      contentType = "text/html"
    )

    # ── CSV individuel ─────────────────────────────────────────────────────────
    output$dl_csv_individuel <- downloadHandler(
      filename=function() paste0("portfolio_",format(Sys.Date(),"%Y%m%d"),".csv"),
      content=function(file){
        uid <- req(user_id_r())
        dc  <- get_eval_connaissances(uid)
        dco <- get_eval_competences(uid)
        ds  <- get_stages(uid)
        dp  <- get_phases(uid)
        write_sec <- function(name, df, cols) {
          tmp <- tempfile()
          keep <- intersect(cols, names(df))
          write.csv(df[,keep,drop=FALSE],tmp,row.names=FALSE,fileEncoding="UTF-8")
          c(paste0("### ",name," ###"),readLines(tmp,warn=FALSE),"")
        }
        lines <- c()
        if(nrow(dc)>0) lines <- c(lines,write_sec("CONNAISSANCES",dc,
          c("domaine","niveau","libelle","autoeval","eval_senior","evaluateur_senior","date_eval_senior","commentaire")))
        if(nrow(dco)>0) lines <- c(lines,write_sec("COMPETENCES",dco,
          c("domaine","niveau","libelle","autoeval","eval_senior","evaluateur_senior","date_eval","commentaire")))
        if(nrow(ds)>0) lines <- c(lines,write_sec("STAGES",ds,
          c("semestre","periode","lieu","responsable_stage","travaux_realises","valorisations","commentaire")))
        if(nrow(dp)>0) lines <- c(lines,write_sec("PHASES",dp,
          c("phase","avis_commission","commentaire","date_validation","validateur")))
        writeLines(lines,file)
      }
    )

    # ── CSV promotion ──────────────────────────────────────────────────────────
    output$dl_csv_promo <- downloadHandler(
      filename=function() paste0("suivi_promo_",format(Sys.Date(),"%Y%m%d"),".csv"),
      content=function(file){
        req(user()$role %in% c("coordinateur","admin"))
        internes <- get_all_internes(include_termine=isTRUE(input$include_termine_exp))
        if(!is.null(input$promo_export)&&input$promo_export!="toutes")
          internes <- internes[internes$promotion==input$promo_export,]
        rows <- lapply(seq_len(nrow(internes)),function(i){
          uid <- internes$id[i]
          dc  <- get_eval_connaissances(uid)
          dco <- get_eval_competences(uid)
          dp  <- get_phases(uid)
          data.frame(username=internes$username[i],nom=internes$nom[i],
            prenom=internes$prenom[i],promotion=internes$promotion[i],
            statut=internes$statut[i],
            pct_conn=if(nrow(dc)==0)0 else round(100*sum(dc$autoeval=="acquis",na.rm=TRUE)/nrow(dc),1),
            pct_comp=if(nrow(dco)==0)0 else round(100*sum(dco$autoeval%in%c("acquis","en_cours"),na.rm=TRUE)/nrow(dco),1),
            phases_validees=sum(dp$avis_commission=="valide",na.rm=TRUE),
            stringsAsFactors=FALSE)
        })
        write.csv(do.call(rbind,rows),file,row.names=FALSE,fileEncoding="UTF-8")
      }
    )

    # ── Excel promotion ────────────────────────────────────────────────────────
    output$dl_excel <- downloadHandler(
      filename=function() paste0("suivi_DES_SP_",format(Sys.Date(),"%Y%m%d"),".xlsx"),
      content=function(file){
        req(user()$role %in% c("coordinateur","admin"))
        internes <- get_all_internes(include_termine=isTRUE(input$include_termine_exp))
        if(!is.null(input$promo_export)&&input$promo_export!="toutes")
          internes <- internes[internes$promotion==input$promo_export,]
        wb <- openxlsx::createWorkbook()
        addWorksheet(wb,"Synthèse")
        rows <- lapply(seq_len(nrow(internes)),function(i){
          uid <- internes$id[i]; dc <- get_eval_connaissances(uid)
          dco <- get_eval_competences(uid); dp <- get_phases(uid)
          data.frame(Nom=internes$nom[i],Prénom=internes$prenom[i],
            Promotion=internes$promotion[i],Statut=internes$statut[i],
            `% Conn.`=if(nrow(dc)==0)0 else round(100*sum(dc$autoeval=="acquis",na.rm=TRUE)/nrow(dc),1),
            `% Comp.`=if(nrow(dco)==0)0 else round(100*sum(dco$autoeval%in%c("acquis","en_cours"),na.rm=TRUE)/nrow(dco),1),
            Phases=sum(dp$avis_commission=="valide",na.rm=TRUE),
            check.names=FALSE,stringsAsFactors=FALSE)
        })
        openxlsx::writeData(wb,"Synthèse",do.call(rbind,rows))
        openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
      }
    )
  })
}
