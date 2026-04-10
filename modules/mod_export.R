# modules/mod_export.R
# ─────────────────────────────────────────────────────────────────────────────

mod_export_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Export du portfolio", width = 6,
        status = "primary", solidHeader = TRUE,
        # Info : pour coord/admin, rappel de l'interne sélectionné
        uiOutput(ns("banner_interne")),
        checkboxGroupInput(ns("sections_pdf"), "Sections à inclure",
          choices = c(
            "Identité & Diplômes"    = "identite",
            "Contrat de formation"   = "contrat",
            "Connaissances"          = "connaissances",
            "Compétences"            = "competences",
            "Carnet de stages"       = "stages",
            "Validation des phases"  = "phases"
          ),
          selected = c("identite","contrat","connaissances","competences","stages","phases")
        ),
        downloadButton(ns("dl_pdf"), "Télécharger PDF",
                       class = "btn-danger btn-block"),
        br(),
        downloadButton(ns("dl_csv_individuel"), "Télécharger CSV individuel",
                       class = "btn-info btn-block")
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
      if (is.null(uid)) {
        return(div(class = "alert alert-warning",
                   icon("triangle-exclamation"),
                   " Aucun interne sélectionné dans la barre latérale."))
      }
      row <- db_query("SELECT nom, prenom FROM users WHERE id=?", list(uid))
      if (nrow(row) == 0) return(NULL)
      div(class = "alert alert-info", style = "margin-bottom:10px;",
          icon("user"), " Export pour : ",
          strong(paste(row$prenom, row$nom)))
    })

    output$panel_pilotage <- renderUI({
      req(user())
      if (!user()$role %in% c("coordinateur","admin")) return(NULL)
      box(title = "Export pilotage pédagogique", width = 6,
          status = "warning", solidHeader = TRUE,
          selectInput(ns("promo_export"), "Promotion",
                      choices = {
                        promos <- db_query(
                          "SELECT DISTINCT promotion FROM users WHERE role='interne' AND active=1"
                        )$promotion
                        c("Toutes" = "toutes", setNames(promos, promos))
                      }),
          checkboxInput(ns("include_termine_exp"),
                        "Inclure internes ayant terminé", FALSE),
          downloadButton(ns("dl_csv_promo"), "CSV promotion",
                         class = "btn-warning btn-block"),
          br(),
          downloadButton(ns("dl_excel"), "Excel multi-onglets",
                         class = "btn-success btn-block")
      )
    })

    # ── PDF ────────────────────────────────────────────────────────────────────
    output$dl_pdf <- downloadHandler(
      filename = function() {
        uid <- user_id_r()
        if (!is.null(uid)) {
          row <- tryCatch(
            db_query("SELECT nom, prenom FROM users WHERE id=?", list(uid)),
            error = function(e) data.frame()
          )
          if (nrow(row) > 0) {
            clean <- function(s) gsub("[^a-zA-Z0-9]", "_", iconv(s, to="ASCII//TRANSLIT"))
            return(paste0("portfolio_", clean(row$nom[1]), "_",
                          clean(row$prenom[1]), "_",
                          format(Sys.Date(), "%Y%m%d"), ".pdf"))
          }
        }
        paste0("portfolio_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content  = function(file) {
        uid <- req(user_id_r())

        # Toutes les données pré-chargées — le Rmd ne touche pas la DB
        data_list <- list(
          user_info = user(),
          sections  = input$sections_pdf,
          id_data   = get_identite(uid),
          diplomes  = get_diplomes(uid),
          contrat   = get_contrat(uid),
          eval_conn = get_eval_connaissances(uid),
          eval_comp = get_eval_competences(uid),
          stages    = get_stages(uid),
          phases    = get_phases(uid)
        )

        # Environnement minimal : uniquement ce dont le Rmd a besoin
        rmd_env <- new.env(parent = baseenv())
        rmd_env$`%||%` <- function(a, b) {
          if (is.null(a) || length(a) == 0) return(b)
          if (length(a) == 1 && (is.na(a) || identical(a, ""))) return(b)
          a
        }
        rmd_env$PHASE_LABELS <- c(
          socle             = "Phase Socle",
          approfondissement = "Phase d'Approfondissement",
          consolidation     = "Phase de Consolidation"
        )

        # Chemin absolu vers le Rmd
        rmd_path <- file.path(getwd(), "templates", "portfolio_pdf.Rmd")
        out_dir  <- tempdir()
        out_file <- file.path(out_dir, paste0("portfolio_", uid, ".pdf"))

        tryCatch({
          rmarkdown::render(
            input         = normalizePath(rmd_path),
            output_file   = out_file,
            output_dir    = out_dir,
            knit_root_dir = getwd(),
            params        = data_list,
            envir         = rmd_env,
            quiet         = TRUE
          )
          file.copy(out_file, file, overwrite = TRUE)
        }, error = function(e) {
          msg <- conditionMessage(e)
          # Fallback texte
          out_txt <- file.path(out_dir, "err.txt")
          writeLines(c("ERREUR RENDU PDF", msg), out_txt)
          file.copy(out_txt, file)
        })
      }
    )

    # ── CSV individuel ─────────────────────────────────────────────────────────
    output$dl_csv_individuel <- downloadHandler(
      filename = function() paste0("portfolio_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content  = function(file) {
        uid <- req(user_id_r())
        dc  <- get_eval_connaissances(uid)
        dco <- get_eval_competences(uid)
        ds  <- get_stages(uid)
        dp  <- get_phases(uid)

        write_section <- function(name, df, cols) {
          tmp <- tempfile()
          keep <- intersect(cols, names(df))
          write.csv(df[, keep, drop = FALSE], tmp,
                    row.names = FALSE, fileEncoding = "UTF-8")
          c(paste0("### ", name, " ###"), readLines(tmp, warn = FALSE), "")
        }

        lines <- c()
        if (nrow(dc) > 0)
          lines <- c(lines, write_section("CONNAISSANCES", dc,
            c("domaine","niveau","libelle","autoeval","eval_senior","evaluateur_senior","date_eval_senior","commentaire")))
        if (nrow(dco) > 0)
          lines <- c(lines, write_section("COMPETENCES", dco,
            c("domaine","niveau","libelle","autoeval","eval_senior","evaluateur_senior","date_eval","commentaire")))
        if (nrow(ds) > 0)
          lines <- c(lines, write_section("STAGES", ds,
            c("semestre","periode","lieu","responsable_stage","travaux_realises","valorisations","commentaire")))
        if (nrow(dp) > 0)
          lines <- c(lines, write_section("PHASES", dp,
            c("phase","avis_commission","commentaire","date_validation","validateur")))
        writeLines(lines, file)
      }
    )

    # ── CSV promotion ──────────────────────────────────────────────────────────
    output$dl_csv_promo <- downloadHandler(
      filename = function() paste0("suivi_promo_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content  = function(file) {
        req(user()$role %in% c("coordinateur","admin"))
        internes <- get_all_internes(
          include_termine = isTRUE(input$include_termine_exp))
        if (!is.null(input$promo_export) && input$promo_export != "toutes")
          internes <- internes[internes$promotion == input$promo_export, ]

        rows <- lapply(seq_len(nrow(internes)), function(i) {
          uid <- internes$id[i]
          dc  <- get_eval_connaissances(uid)
          dco <- get_eval_competences(uid)
          dp  <- get_phases(uid)
          data.frame(
            username  = internes$username[i],
            nom       = internes$nom[i],
            prenom    = internes$prenom[i],
            promotion = internes$promotion[i],
            statut    = internes$statut[i],
            pct_conn  = if(nrow(dc)==0) 0 else
              round(100*sum(dc$autoeval=="acquis",na.rm=TRUE)/nrow(dc),1),
            pct_comp  = if(nrow(dco)==0) 0 else
              round(100*sum(dco$autoeval%in%c("acquis","en_cours"),na.rm=TRUE)/nrow(dco),1),
            phases_validees = sum(dp$avis_commission=="valide",na.rm=TRUE),
            stringsAsFactors = FALSE
          )
        })
        write.csv(do.call(rbind, rows), file,
                  row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # ── Excel promotion ────────────────────────────────────────────────────────
    output$dl_excel <- downloadHandler(
      filename = function() paste0("suivi_DES_SP_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content  = function(file) {
        req(user()$role %in% c("coordinateur","admin"))
        internes <- get_all_internes(
          include_termine = isTRUE(input$include_termine_exp))
        if (!is.null(input$promo_export) && input$promo_export != "toutes")
          internes <- internes[internes$promotion == input$promo_export, ]

        wb <- openxlsx::createWorkbook()

        addWorksheet(wb, "Synthèse")
        rows <- lapply(seq_len(nrow(internes)), function(i) {
          uid <- internes$id[i]
          dc  <- get_eval_connaissances(uid)
          dco <- get_eval_competences(uid)
          dp  <- get_phases(uid)
          data.frame(
            Nom = internes$nom[i], Prénom = internes$prenom[i],
            Promotion = internes$promotion[i], Statut = internes$statut[i],
            `% Conn.` = if(nrow(dc)==0) 0 else
              round(100*sum(dc$autoeval=="acquis",na.rm=TRUE)/nrow(dc),1),
            `% Comp.` = if(nrow(dco)==0) 0 else
              round(100*sum(dco$autoeval%in%c("acquis","en_cours"),na.rm=TRUE)/nrow(dco),1),
            `Phases` = sum(dp$avis_commission=="valide",na.rm=TRUE),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        })
        openxlsx::writeData(wb, "Synthèse", do.call(rbind, rows))

        .write_sheet <- function(wb, sheet_name, get_fn, cols) {
          addWorksheet(wb, sheet_name)
          all_d <- do.call(rbind, lapply(seq_len(nrow(internes)), function(i) {
            d <- get_fn(internes$id[i])
            if (is.null(d) || nrow(d) == 0) return(NULL)
            d$interne <- paste(internes$nom[i], internes$prenom[i])
            d[, intersect(c("interne", cols), names(d)), drop = FALSE]
          }))
          if (!is.null(all_d) && nrow(all_d) > 0)
            openxlsx::writeData(wb, sheet_name, all_d)
        }

        .write_sheet(wb, "Connaissances", get_eval_connaissances,
          c("domaine","niveau","libelle","autoeval","eval_senior","evaluateur_senior","commentaire"))
        .write_sheet(wb, "Compétences", get_eval_competences,
          c("domaine","niveau","libelle","autoeval","eval_senior","evaluateur_senior","commentaire"))

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}
