# modules/mod_contrat.R — Contrat de formation
# ─────────────────────────────────────────────────────────────────────────────

mod_contrat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Projet professionnel", width = 6,
          status = "primary", solidHeader = TRUE,
          textAreaInput(ns("projet"), NULL, rows = 7,
            placeholder = "Décrivez votre projet professionnel...")
      ),
      box(title = "Thèse de médecine", width = 6,
          status = "warning", solidHeader = TRUE,
          selectInput(ns("these_statut"), "Statut",
                      choices = c("Non débutée"              = "non_debutee",
                                  "Sujet en cours"           = "sujet_en_cours",
                                  "Sujet validé"             = "sujet_valide",
                                  "En cours de réalisation"  = "en_cours",
                                  "Validée"                  = "validee"),
                      selected = "non_debutee"),
          textInput(ns("these_sujet"),     "Sujet de thèse",
                    placeholder = "Titre / sujet envisagé..."),
          textInput(ns("these_directeur"), "Directeur de thèse"),
          dateInput(ns("these_date"),      "Date de soutenance (prévisionnelle)",
                    value = NA, language = "fr", format = "dd/mm/yyyy",
                    min = Sys.Date())
      )
    ),
    fluidRow(
      box(title = "Objectifs pédagogiques — Connaissances", width = 6,
          status = "info", solidHeader = TRUE,
          textAreaInput(ns("obj_conn"), NULL, rows = 5,
            placeholder = "Domaines de connaissances à prioritiser...")
      ),
      box(title = "Objectifs pédagogiques — Compétences", width = 6,
          status = "info", solidHeader = TRUE,
          textAreaInput(ns("obj_comp"), NULL, rows = 5,
            placeholder = "Compétences à développer...")
      )
    ),
    fluidRow(
      box(title = "Formations envisagées", width = 12,
          status = "info", solidHeader = TRUE,
          textAreaInput(ns("formations_envisagees"), NULL, rows = 4,
            placeholder = "Master 2, Option administration de la santé, DU/DIU...")
      )
    ),
    fluidRow(
      box(width = 12,
        actionButton(ns("btn_save"), "Enregistrer",
                     class = "btn-success", icon = icon("save")),
        uiOutput(ns("msg"))
      )
    )
  )
}

mod_contrat_server <- function(id, user_id_r) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(user_id_r())
      uid <- user_id_r()
      d <- get_contrat(uid)
      if (nrow(d) > 0) {
        updateTextAreaInput(session, "projet",   value = d$projet_professionnel[1] %||% "")
        updateTextAreaInput(session, "obj_conn", value = d$obj_connaissances[1]    %||% "")
        updateTextAreaInput(session, "obj_comp", value = d$obj_competences[1]      %||% "")
        updateTextAreaInput(session, "formations_envisagees",
                            value = d$formations_envisagees[1] %||% "")
        updateSelectInput(session, "these_statut",
                          selected = d$these_statut[1] %||% "non_debutee")
        updateTextInput(session, "these_sujet",
                        value = d$these_sujet[1] %||% "")
        updateTextInput(session, "these_directeur",
                        value = d$these_directeur[1] %||% "")
        if (!is.na(d$these_date[1]) && nchar(d$these_date[1]) > 0)
          tryCatch(updateDateInput(session, "these_date", value = as.Date(d$these_date[1])),
                   error = function(e) NULL)
      }
    })

    observeEvent(input$btn_save, {
      req(user_id_r())
      uid <- user_id_r()
      tryCatch({
        these_date_val <- tryCatch(
          as.character(input$these_date), error = function(e) NA_character_)

        save_contrat_full(uid,
          projet                = input$projet,
          obj_conn              = input$obj_conn,
          obj_comp              = input$obj_comp,
          formations_envisagees = input$formations_envisagees %||% NA,
          these_statut          = input$these_statut,
          these_sujet           = input$these_sujet    %||% NA,
          these_directeur       = input$these_directeur %||% NA,
          these_date            = these_date_val
        )
        output$msg <- renderUI(
          div(class = "alert alert-success", icon("check"), " Contrat enregistré."))
        shinyjs::delay(5000, output$msg <- renderUI(NULL))

      }, error = function(e)
        output$msg <- renderUI(
          div(class = "alert alert-danger", " Erreur : ", e$message)))
    })
  })
}
