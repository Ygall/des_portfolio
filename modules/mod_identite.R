# modules/mod_identite.R — Fiche d'identité
# ─────────────────────────────────────────────────────────────────────────────

mod_identite_ui <- function(id) {
  ns <- NS(id)
  fluidRow(

    # ── Colonne gauche : Etat civil + Études (2e et 3e cycle fusionnés) ────────
    column(6,
      box(title = "État civil", width = NULL, status = "primary", solidHeader = TRUE,
        textInput(ns("nom"),    "Nom"),
        textInput(ns("prenom"), "Prénom(s)"),
        dateInput(ns("date_naissance"), "Date de naissance",
                  language = "fr", format = "dd/mm/yyyy", value = NA, max = Sys.Date())
      ),
      box(title = "Études médicales", width = NULL, status = "info", solidHeader = TRUE,
        h5(style = "color:#555; margin-bottom:10px; font-weight:600;",
           icon("graduation-cap"), " 2ème cycle"),
        selectInput(ns("faculte_2"), "Faculté",
                    choices = c("— Sélectionner —" = "", FACULTES_MEDECINE)),
        numericInput(ns("annee_edn"), "Année ECN / EDN",
                     value = NA_real_, min = 2016, max = 2050, step = 1),
        hr(style = "margin: 14px 0;"),
        h5(style = "color:#555; margin-bottom:10px; font-weight:600;",
           icon("user-md"), " 3ème cycle"),
        selectInput(ns("faculte_3"), "Faculté",
                    choices = c("— Sélectionner —" = "", FACULTES_MEDECINE)),
        textInput(ns("des_initial"), "DES initial (si autre que Santé Publique)")
      )
    ),

    # ── Colonne droite : Diplômes ──────────────────────────────────────────────
    column(6,
      box(title = "Diplômes", width = NULL, status = "info", solidHeader = TRUE,
        uiOutput(ns("diplomes_ui")),
        br(),
        actionButton(ns("btn_add_diplome"), "+ Ajouter un diplôme",
                     class = "btn-default btn-sm", icon = icon("plus"))
      )
    ),

    # ── Bouton save pleine largeur ─────────────────────────────────────────────
    column(12,
      box(width = NULL,
        actionButton(ns("btn_save"), "Enregistrer l'identité",
                     class = "btn-success", icon = icon("save")),
        uiOutput(ns("msg"))
      )
    )
  )
}

mod_identite_server <- function(id, user_id_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dip_trigger <- reactiveVal(0L)

    # Chargement identité
    observe({
      req(user_id_r())
      uid <- user_id_r()
      d <- get_identite(uid)
      if (nrow(d) > 0) {
        updateTextInput(session, "nom",    value = d$nom[1]    %||% "")
        updateTextInput(session, "prenom", value = d$prenom[1] %||% "")
        dn <- d$date_naissance[1]
        if (!is.null(dn) && !is.na(dn) && nchar(dn) > 0)
          tryCatch(updateDateInput(session, "date_naissance", value = as.Date(dn)),
                   error = function(e) NULL)
        updateSelectInput(session, "faculte_2",   selected = d$faculte_2e_cycle[1] %||% "")
        val_edn <- suppressWarnings(as.numeric(d$annee_edn[1]))
        if (!is.na(val_edn)) updateNumericInput(session, "annee_edn", value = val_edn)
        updateSelectInput(session, "faculte_3",   selected = d$faculte_3e_cycle[1] %||% "")
        updateTextInput(session, "des_initial",   value = d$des_initial[1] %||% "")
      }
    })

    # Diplômes
    output$diplomes_ui <- renderUI({
      dip_trigger()
      uid <- req(user_id_r())
      dip <- get_diplomes(uid)
      if (nrow(dip) == 0)
        return(div(class = "text-muted", style = "padding:6px;", "Aucun diplôme enregistré."))

      lapply(seq_len(nrow(dip)), function(i) {
        row <- dip[i, ]
        wellPanel(style = "padding:10px; margin-bottom:6px; background:#f9f9f9;",
          fluidRow(
            column(1,
              div(style = "padding-top:25px; font-weight:bold; color:#aaa;", paste0("#", i))
            ),
            column(3,
              selectInput(ns(paste0("dip_type_", row$id)), "Type",
                          choices = c("DU/DIU","M1","M2","Autres"),
                          selected = row$type_diplome)
            ),
            column(5,
              textInput(ns(paste0("dip_intitule_", row$id)), "Intitulé",
                        value = row$intitule %||% "")
            ),
            column(3,
              textInput(ns(paste0("dip_annee_", row$id)), "Année",
                        value = row$annee %||% "")
            )
          ),
          fluidRow(
            column(8,
              textInput(ns(paste0("dip_univ_", row$id)), "Université",
                        value = row$universite %||% "")
            ),
            column(2, style = "padding-top:25px;",
              actionButton(ns(paste0("btn_save_dip_", row$id)),
                           label = "", icon = icon("save"),
                           class = "btn-primary btn-sm", title = "Enregistrer")
            ),
            column(2, style = "padding-top:25px;",
              actionButton(ns(paste0("btn_del_dip_", row$id)),
                           label = "", icon = icon("trash"),
                           class = "btn-danger btn-sm", title = "Supprimer")
            )
          )
        )
      })
    })

    # Observers dynamiques save/delete
    observe({
      uid <- user_id_r(); req(uid)
      dip <- get_diplomes(uid)
      if (nrow(dip) == 0) return()
      lapply(seq_len(nrow(dip)), function(i) {
        did <- dip$id[i]
        observeEvent(input[[paste0("btn_save_dip_", did)]], {
          tryCatch({
            save_diplome(uid, did,
                         input[[paste0("dip_type_",     did)]] %||% "Autres",
                         input[[paste0("dip_intitule_", did)]] %||% NA,
                         input[[paste0("dip_univ_",     did)]] %||% NA,
                         input[[paste0("dip_annee_",    did)]] %||% NA)
            showNotification(paste("Diplôme #", i, "enregistré ✓"),
                             type = "message", duration = 2)
          }, error = function(e) showNotification(e$message, type = "error"))
        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        observeEvent(input[[paste0("btn_del_dip_", did)]], {
          tryCatch({
            delete_diplome(did, uid)
            dip_trigger(dip_trigger() + 1L)
          }, error = function(e) showNotification(e$message, type = "error"))
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })

    observeEvent(input$btn_add_diplome, {
      uid <- req(user_id_r())
      tryCatch({
        db_execute("INSERT INTO diplomes(user_id,type_diplome,updated_at)
                    VALUES(?,\'Autres\',datetime(\'now\'))", list(uid))
        dip_trigger(dip_trigger() + 1L)
      }, error = function(e) showNotification(e$message, type = "error"))
    })

    observeEvent(input$btn_save, {
      uid <- req(user_id_r())
      tryCatch({
        annee_val <- if (!is.na(input$annee_edn) &&
                         input$annee_edn >= 2016 && input$annee_edn <= 2050)
          as.character(as.integer(input$annee_edn)) else NA_character_
        dn_val <- tryCatch(as.character(input$date_naissance),
                           error = function(e) NA_character_)
        save_identite(uid, list(
          input$nom, input$prenom, dn_val,
          input$faculte_2, annee_val,
          input$des_initial, input$faculte_3
        ))
        output$msg <- renderUI(
          div(class = "alert alert-success", icon("check"), " Identité enregistrée."))
      }, error = function(e)
        output$msg <- renderUI(
          div(class = "alert alert-danger", " Erreur : ", e$message)))
    })
  })
}
