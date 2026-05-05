# modules/mod_identite.R — Fiche d'identité
# ─────────────────────────────────────────────────────────────────────────────

mod_identite_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # Colonne gauche : état civil + études
      column(6,
        box(title = "État civil", width = NULL, status = "primary", solidHeader = TRUE,
          # Nom et prénom issus de users — source unique de vérité
          textInput(ns("nom"),    "Nom",     placeholder = "Identique au compte"),
          textInput(ns("prenom"), "Prénom(s)", placeholder = "Identique au compte"),
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
          hr(style = "margin:14px 0;"),
          h5(style = "color:#555; margin-bottom:10px; font-weight:600;",
             icon("user-md"), " 3ème cycle"),
          selectInput(ns("faculte_3"), "Faculté",
                      choices = c("— Sélectionner —" = "", FACULTES_MEDECINE)),
          textInput(ns("des_initial"), "DES initial (si autre que Santé Publique)")
        )
      ),

      # Colonne droite : diplômes
      column(6,
        box(title = "Diplômes", width = NULL, status = "info", solidHeader = TRUE,
          uiOutput(ns("diplomes_ui")),
          br(),
          fluidRow(
            column(6,
              actionButton(ns("btn_add_diplome"), "+ Ajouter un diplôme",
                           class = "btn-default btn-sm", icon = icon("plus"))),
            column(6,
              actionButton(ns("btn_save_diplomes"), "Enregistrer les diplômes",
                           class = "btn-success btn-sm", icon = icon("save")))
          ),
          uiOutput(ns("msg_diplomes"))
        )
      ),

      # Bouton global
      column(12,
        box(width = NULL,
          actionButton(ns("btn_save"), "Enregistrer l'identité",
                       class = "btn-success", icon = icon("save")),
          uiOutput(ns("msg"))
        )
      )
    )
  )
}

mod_identite_server <- function(id, user_id_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Compteur de diplomes en mémoire (lignes UI courantes)
    dip_count <- reactiveVal(0L)

    # Chargement identité depuis users + identite
    observe({
      req(user_id_r())
      uid <- user_id_r()

      # Nom/prénom depuis table users (source unique)
      row_user <- db_query("SELECT nom, prenom FROM users WHERE id=?", list(uid))
      if (nrow(row_user) > 0) {
        updateTextInput(session, "nom",    value = row_user$nom[1]    %||% "")
        updateTextInput(session, "prenom", value = row_user$prenom[1] %||% "")
      }

      # Reste depuis identite
      d <- get_identite(uid)
      if (nrow(d) > 0) {
        dn <- d$date_naissance[1]
        if (!is.null(dn) && !is.na(dn) && nchar(dn) > 0)
          tryCatch(updateDateInput(session, "date_naissance", value = as.Date(dn)),
                   error = function(e) NULL)
        updateSelectInput(session, "faculte_2",   selected = d$faculte_2e_cycle[1] %||% "")
        val_edn <- suppressWarnings(as.numeric(d$annee_edn[1]))
        if (!is.na(val_edn)) updateNumericInput(session, "annee_edn", value = val_edn)
        updateSelectInput(session, "faculte_3",   selected = d$faculte_3e_cycle[1] %||% "")
        updateTextInput(session,   "des_initial", value = d$des_initial[1] %||% "")
      }

      # Initialiser le compteur de diplomes
      dip <- get_diplomes(uid)
      dip_count(nrow(dip))
    })

    # Rendu des diplomes — basé uniquement sur dip_count (pas de DB reload dans renderUI)
    output$diplomes_ui <- renderUI({
      n <- dip_count()
      if (n == 0)
        return(div(class = "text-muted", style = "padding:6px;", "Aucun diplôme."))

      uid <- user_id_r()
      dip <- get_diplomes(uid)

      lapply(seq_len(n), function(i) {
        row <- if (i <= nrow(dip)) dip[i, ] else
          list(id = NA, type_diplome = "Autres", intitule = "",
               universite = "", annee = "")
        wellPanel(style = "padding:10px; margin-bottom:4px; background:#f9f9f9;",
          fluidRow(
            column(1,
              div(style = "padding-top:25px; font-weight:bold; color:#aaa;",
                  paste0("#", i))),
            column(3,
              selectInput(ns(paste0("dip_type_",  i)), "Type",
                          choices  = c("DU/DIU","M1","M2","Autres"),
                          selected = row$type_diplome %||% "Autres")),
            column(5,
              textInput(ns(paste0("dip_intitule_", i)), "Intitulé",
                        value = row$intitule %||% "")),
            column(3,
              textInput(ns(paste0("dip_annee_",   i)), "Année",
                        value = row$annee %||% ""))
          ),
          fluidRow(
            column(10,
              textInput(ns(paste0("dip_univ_", i)), "Université",
                        value = row$universite %||% "")),
            column(2, style = "padding-top:25px;",
              actionButton(ns(paste0("btn_del_dip_", i)),
                           label = "", icon = icon("trash"),
                           class = "btn-danger btn-sm"))
          )
        )
      })
    })

    # Suppression d'une ligne de diplôme
    observe({
      n   <- dip_count()
      uid <- user_id_r()
      lapply(seq_len(n), function(i) {
        observeEvent(input[[paste0("btn_del_dip_", i)]], {
          dip <- get_diplomes(uid)
          if (i <= nrow(dip)) {
            tryCatch(delete_diplome(dip$id[i], uid),
                     error = function(e) showNotification(e$message, type="error"))
          }
          dip_count(max(0L, dip_count() - 1L))
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Ajouter un diplôme vide (ne sauvegarde pas en DB, juste incrémente le compteur)
    observeEvent(input$btn_add_diplome, {
      dip_count(dip_count() + 1L)
    })

    # Enregistrer TOUS les diplômes d'un coup
    observeEvent(input$btn_save_diplomes, {
      uid <- req(user_id_r())
      n   <- dip_count()

      tryCatch({
        # Récupérer les diplomes existants en DB pour connaître leurs IDs
        dip_db <- get_diplomes(uid)

        for (i in seq_len(n)) {
          intitule <- input[[paste0("dip_intitule_", i)]] %||% ""
          # Si ligne vide on skip
          if (nchar(trimws(intitule)) == 0 && nchar(trimws(input[[paste0("dip_univ_", i)]] %||% "")) == 0) next

          id_dip <- if (i <= nrow(dip_db)) dip_db$id[i] else NULL
          save_diplome(uid, id_dip,
                       input[[paste0("dip_type_",     i)]] %||% "Autres",
                       intitule,
                       input[[paste0("dip_univ_",     i)]] %||% NA,
                       input[[paste0("dip_annee_",    i)]] %||% NA)
        }

        output$msg_diplomes <- renderUI(
          div(class = "alert alert-success", style = "margin-top:8px;",
              icon("check"), " Diplômes enregistrés."))
        # Auto-dismiss après 5 secondes
        shinyjs::delay(5000, output$msg_diplomes <- renderUI(NULL))

      }, error = function(e)
        output$msg_diplomes <- renderUI(
          div(class = "alert alert-danger", " Erreur : ", e$message)))
    })

    # Enregistrer identité (hors diplômes)
    observeEvent(input$btn_save, {
      uid <- req(user_id_r())
      tryCatch({
        annee_val <- if (!is.na(input$annee_edn) &&
                         input$annee_edn >= 2016 && input$annee_edn <= 2050)
          as.character(as.integer(input$annee_edn)) else NA_character_
        dn_val <- tryCatch(as.character(input$date_naissance),
                           error = function(e) NA_character_)

        save_identite(uid,
          nom              = trimws(input$nom    %||% ""),
          prenom           = trimws(input$prenom %||% ""),
          date_naissance   = dn_val,
          faculte_2e_cycle = input$faculte_2   %||% NA,
          annee_edn        = annee_val,
          des_initial      = input$des_initial %||% NA,
          faculte_3e_cycle = input$faculte_3   %||% NA
        )

        output$msg <- renderUI(
          div(class = "alert alert-success", icon("check"), " Identité enregistrée."))
        shinyjs::delay(5000, output$msg <- renderUI(NULL))

      }, error = function(e)
        output$msg <- renderUI(
          div(class = "alert alert-danger", " Erreur : ", e$message)))
    })
  })
}
