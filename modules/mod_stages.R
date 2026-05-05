# modules/mod_stages.R — Carnet de stages
# ─────────────────────────────────────────────────────────────────────────────

.periodes_semestre <- function() {
  annees <- seq(2018, as.integer(format(Sys.Date(), "%Y")) + 2)
  choices <- unlist(lapply(annees, function(a)
    c(paste0("Mai ", a), paste0("Novembre ", a))))
  c("— Sélectionner —" = "", setNames(choices, choices))
}

.valide_label <- function(v) {
  if (is.null(v) || is.na(v) || identical(v, ""))
    return("⬜ En attente")
  if (v == 1L || identical(v, 1) || identical(v, TRUE))
    return("✅ Validé")
  if (v == 0L || identical(v, 0) || identical(v, FALSE))
    return("❌ Non validé")
  "⬜ En attente"
}

mod_stages_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Carnet de stages", width = 12, status = "primary", solidHeader = TRUE,
        DTOutput(ns("tbl_stages")),
        br(),
        fluidRow(
          column(6,
            actionButton(ns("btn_add"), "+ Ajouter un semestre supplémentaire",
                         class = "btn-default btn-sm", icon = icon("plus"))),
          column(6,
            actionButton(ns("btn_del_last"), "Supprimer le dernier semestre",
                         class = "btn-danger btn-sm", icon = icon("trash")))
        )
      )
    ),
    fluidRow(
      box(title = uiOutput(ns("edit_title")), width = 12,
          status = "warning", solidHeader = TRUE,
          uiOutput(ns("panel_edit")))
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
      n_db <- if (!is.null(d) && nrow(d) > 0) max(d$semestre, na.rm = TRUE) else 0L
      max(N_SEMESTRES_MAX, n_db) + n_extra()
    })

    observeEvent(input$btn_add, { n_extra(n_extra() + 1L) })

    observeEvent(input$btn_del_last, {
      current <- n_total()
      if (current <= N_SEMESTRES_MAX) {
        showNotification("Le minimum est 8 semestres.", type = "warning", duration = 2)
        return()
      }
      d   <- stages_data()
      uid <- user_id_r()
      if (!is.null(d) && nrow(d) > 0 && current %in% d$semestre) {
        tryCatch(
          db_execute("DELETE FROM stages WHERE user_id=? AND semestre=?",
                     list(uid, current)),
          error = function(e) showNotification(e$message, type = "error")
        )
        load_data()
      } else {
        n_extra(max(0L, n_extra() - 1L))
      }
    })

    output$tbl_stages <- renderDT({
      req(stages_data())
      nt   <- n_total()
      d    <- stages_data()
      full <- merge(data.frame(semestre = seq_len(nt)), d, by = "semestre", all.x = TRUE)

      display <- data.frame(
        Semestre             = paste("Semestre", full$semestre),
        Periode              = ifelse(is.na(full$periode)  | full$periode  == "", "—", full$periode),
        Lieu                 = ifelse(is.na(full$lieu)     | full$lieu     == "", "—", full$lieu),
        `Responsable`        = ifelse(is.na(full$responsable_stage) | full$responsable_stage == "",
                                      "—", full$responsable_stage),
        `Stage validé`       = sapply(full$stage_valide, .valide_label),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      datatable(display, rownames = FALSE, selection = "single",
        options = list(pageLength = 12, dom = "t",
          language = list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"))
      ) |>
        formatStyle("Stage validé",
          backgroundColor = styleEqual(
            c("✅ Validé","❌ Non validé","⬜ En attente"),
            c("#d4edda","#fde2e2","#f8f9fa")))
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
      item <- if (!is.null(d) && nrow(d) > 0 && sel %in% d$semestre) d[d$semestre == sel, ] else
        data.frame(semestre = sel, periode = "", lieu = "", responsable_stage = "",
                   travaux_realises = "", valorisations = "", commentaire = "",
                   stage_valide = NA_integer_, stringsAsFactors = FALSE)

      # Valeur actuelle de stage_valide
      valide_cur <- if (is.null(item$stage_valide) || is.na(item$stage_valide)) "en_attente"
                   else if (item$stage_valide == 1L) "valide"
                   else "non_valide"

      tagList(
        fluidRow(
          column(6,
            selectInput(ns("edit_periode"), "Période du semestre",
                        choices = .periodes_semestre(), selected = item$periode %||% "")),
          column(6,
            textInput(ns("edit_lieu"), "Lieu du stage", value = item$lieu %||% ""))
        ),
        fluidRow(
          column(6,
            textInput(ns("edit_resp"), "Responsable de stage",
                      value = item$responsable_stage %||% "")),
          column(6,
            selectInput(ns("edit_stage_valide"), "Stage validé",
                        choices = c("En attente"  = "en_attente",
                                    "Validé"       = "valide",
                                    "Non validé"   = "non_valide"),
                        selected = valide_cur))
        ),
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
                     input$edit_periode         %||% NA,
                     input$edit_lieu            %||% NA,
                     input$edit_resp            %||% NA,
                     input$edit_travaux         %||% NA,
                     input$edit_valorisations   %||% NA,
                     input$edit_commentaire     %||% NA,
                     input$edit_stage_valide    %||% "en_attente")
        load_data()
        showNotification(paste("Semestre", sel, "enregistré ✓"),
                         type = "message", duration = 2)
      }, error = function(e) showNotification(e$message, type = "error"))
    })
  })
}
