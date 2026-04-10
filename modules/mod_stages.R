# modules/mod_stages.R — Carnet de stages
# ─────────────────────────────────────────────────────────────────────────────

# Génère les choix de période pour un semestre (Mai/Novembre depuis 2020)
.periodes_semestre <- function() {
  annees <- seq(2018, as.integer(format(Sys.Date(), "%Y")) + 2)
  choices <- unlist(lapply(annees, function(a)
    c(paste0("Mai ", a), paste0("Novembre ", a))))
  c("— Sélectionner —" = "", setNames(choices, choices))
}

mod_stages_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Carnet de stages", width = 12, status = "primary", solidHeader = TRUE,
        DTOutput(ns("tbl_stages")),
        br(),
        actionButton(ns("btn_add"), "+ Ajouter un semestre supplémentaire",
                     class = "btn-default btn-sm", icon = icon("plus"))
      )
    ),
    fluidRow(
      box(title = uiOutput(ns("edit_title")), width = 12,
          status = "warning", solidHeader = TRUE,
          uiOutput(ns("panel_edit"))
      )
    )
  )
}

mod_stages_server <- function(id, user_id_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    n_extra     <- reactiveVal(0L)
    stages_data <- reactiveVal(NULL)

    load_data <- function() {
      req(user_id_r())
      stages_data(get_stages(user_id_r()))
    }

    observe({ req(user_id_r()); n_extra(0L); load_data() })

    n_total <- reactive({
      d <- stages_data()
      base <- if (!is.null(d) && nrow(d) > 0) max(d$semestre, na.rm = TRUE) else N_SEMESTRES_MAX
      max(N_SEMESTRES_MAX, base) + n_extra()
    })

    observeEvent(input$btn_add, { n_extra(n_extra() + 1L) })

    output$tbl_stages <- renderDT({
      req(stages_data())
      nt    <- n_total()
      d     <- stages_data()
      full  <- merge(data.frame(semestre = seq_len(nt)), d, by = "semestre", all.x = TRUE)

      display <- data.frame(
        Semestre             = paste("Semestre", full$semestre),
        Periode              = ifelse(is.na(full$periode) | full$periode == "", "—", full$periode),
        Lieu                 = ifelse(is.na(full$lieu)    | full$lieu    == "", "—", full$lieu),
        `Responsable de stage` = ifelse(is.na(full$responsable_stage) | full$responsable_stage == "",
                                        "—", full$responsable_stage),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      datatable(display, rownames = FALSE, selection = "single",
        options = list(pageLength = 12, dom = "t",
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"))
      )
    }, server = TRUE)

    output$edit_title <- renderUI({
      sel <- input$tbl_stages_rows_selected
      if (is.null(sel)) "Modifier un stage" else paste("Modifier — Semestre", sel)
    })

    output$panel_edit <- renderUI({
      sel <- input$tbl_stages_rows_selected
      if (is.null(sel))
        return(div(class = "text-muted", icon("hand-pointer"),
                   " Sélectionnez un semestre dans le tableau."))

      d    <- stages_data()
      item <- if (!is.null(d) && nrow(d) >= sel && !is.na(d$semestre[sel])) d[sel, ] else
        data.frame(semestre = sel, periode = "", lieu = "", responsable_stage = "",
                   travaux_realises = "", valorisations = "", commentaire = "",
                   stringsAsFactors = FALSE)

      tagList(
        fluidRow(
          column(6,
            selectInput(ns("edit_periode"), "Période du semestre",
                        choices  = .periodes_semestre(),
                        selected = item$periode %||% "")
          ),
          column(6,
            textInput(ns("edit_lieu"), "Lieu du stage", value = item$lieu %||% "")
          )
        ),
        textInput(ns("edit_resp"), "Responsable de stage",
                  value = item$responsable_stage %||% ""),
        textAreaInput(ns("edit_travaux"), "Travaux réalisés", rows = 3,
                      value = item$travaux_realises %||% ""),
        textAreaInput(ns("edit_valorisations"), "Valorisations / Communications", rows = 3,
                      value = item$valorisations %||% ""),
        textAreaInput(ns("edit_commentaire"), "Commentaire / Précisions", rows = 3,
                      value = item$commentaire %||% ""),
        actionButton(ns("btn_save_stage"), "Enregistrer ce stage",
                     class = "btn-success", icon = icon("save"))
      )
    })

    observeEvent(input$btn_save_stage, {
      sel <- input$tbl_stages_rows_selected
      req(sel, user_id_r())
      tryCatch({
        upsert_stage(user_id_r(), sel,
                     input$edit_periode    %||% NA,
                     input$edit_lieu       %||% NA,
                     input$edit_resp       %||% NA,
                     input$edit_travaux    %||% NA,
                     input$edit_valorisations %||% NA,
                     input$edit_commentaire %||% NA)
        load_data()
        showNotification(paste("Semestre", sel, "enregistré ✓"),
                         type = "message", duration = 2)
      }, error = function(e) showNotification(e$message, type = "error"))
    })
  })
}
