# modules/mod_competences.R — Portfolio de compétences (bulk edit)
# ─────────────────────────────────────────────────────────────────────────────

mod_competences_ui <- function(id) {
  ns <- NS(id)
  domaines_raw   <- unique(REF_COMPETENCES$domaine)
  domaines_clean <- gsub("\\s+", " ", trimws(domaines_raw))

  tagList(
    fluidRow(
      column(12,
        div(class = "well well-sm",
          fluidRow(
            column(4,
              selectInput(ns("filtre_domaine"), "Filtrer par domaine",
                          choices = c("Tous" = "tous",
                                      setNames(domaines_raw, domaines_clean)),
                          selected = "tous")),
            column(4,
              selectInput(ns("filtre_niveau"), "Niveau",
                          choices = c("Tous" = "tous",
                                      "Base" = "Base", "Avancé" = "Avancé"),
                          selected = "tous")),
            column(4,
              selectInput(ns("filtre_statut"), "Statut",
                          choices = c("Tous"          = "tous",
                                      "Non évalué"    = "non_evalue",
                                      "Non acquis"    = "non_acquis",
                                      "En cours"      = "en_cours",
                                      "Acquis"        = "acquis"),
                          selected = "tous"))
          )
        )
      )
    ),
    fluidRow(
      column(8,
        box(title = uiOutput(ns("tbl_title")), width = NULL,
            status = "primary", solidHeader = TRUE,
            div(style = "font-size:0.82em; color:#666; margin-bottom:8px;",
                icon("info-circle"),
                " Maintenez ", tags$kbd("Ctrl"), " (ou ", tags$kbd("⌘"), ")",
                " pour une sélection multiple. Shift+clic pour une plage."),
            div(style = "overflow-x:auto;", DTOutput(ns("tbl_comp")))
        )
      ),
      column(4,
        box(title = uiOutput(ns("panel_title")), width = NULL,
            status = "warning", solidHeader = TRUE,
            uiOutput(ns("panel_bulk"))
        )
      )
    )
  )
}

mod_competences_server <- function(id, user_id_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    eval_data <- reactiveVal(NULL)

    load_data <- function() {
      req(user_id_r())
      eval_data(get_eval_competences(user_id_r()))
    }
    observe({ req(user_id_r()); load_data() })

    filtered_data <- reactive({
      req(eval_data())
      d <- eval_data()
      if (input$filtre_domaine != "tous") d <- d[d$domaine == input$filtre_domaine, ]
      if (input$filtre_niveau  != "tous") d <- d[d$niveau  == input$filtre_niveau,  ]
      if (input$filtre_statut  != "tous")
        d <- d[!is.na(d$autoeval) & d$autoeval == input$filtre_statut, ]
      d
    })

    n_sel <- reactive({
      sel <- input$tbl_comp_rows_selected
      if (is.null(sel)) 0L else length(sel)
    })

    output$tbl_title <- renderUI({
      n <- n_sel()
      if (n == 0) "Référentiel de compétences"
      else span("Référentiel de compétences —",
                span(style = "color:#f39c12; font-weight:700;",
                     paste(n, "item(s) sélectionné(s)")))
    })

    output$panel_title <- renderUI({
      n <- n_sel()
      if (n == 0)      "Sélectionnez des items"
      else if (n == 1) "Modifier l'item"
      else             paste("Modifier", n, "items en bloc")
    })

    label_ae <- function(s) switch(s %||% "non_evalue",
      non_evalue="Non évalué", non_acquis="Non acquis",
      en_cours="En cours", acquis="Acquis", "Non évalué")
    label_sr <- function(s) {
      if (is.null(s)||is.na(s)||identical(s,"")) return("—")
      switch(s, acquis="Acquis", en_cours="En cours", "Non acquis")
    }
    norm_lib <- function(s) {
      if (is.null(s)||is.na(s)||nchar(s)==0) return(s)
      paste0(toupper(substr(s,1,1)), substr(s,2,nchar(s)))
    }

    output$tbl_comp <- renderDT({
      d <- filtered_data()
      if (nrow(d) == 0)
        return(datatable(
          data.frame(Domaine=character(), Niveau=character(),
                     Competence=character(), `Auto-éval`=character(),
                     Senior=character(), check.names=FALSE),
          colnames = c("Domaine","Niveau","Compétence","Auto-éval","Éval. Senior"),
          rownames = FALSE,
          options  = list(dom="t",
            language=list(emptyTable="Aucun item pour ce filtre."))))

      display <- data.frame(
        Domaine   = sub("^(\\d+) -+\\s*","\\1. ", d$domaine),
        Niveau    = d$niveau,
        Competence= sapply(d$libelle, norm_lib),
        AutoEval  = sapply(d$autoeval, label_ae),
        Senior    = sapply(d$eval_senior, label_sr),
        stringsAsFactors = FALSE
      )
      datatable(display,
        colnames  = c("Domaine","Niveau","Compétence","Auto-éval","Éval. Senior"),
        rownames  = FALSE,
        selection = list(mode="multiple", selected=NULL, target="row"),
        options   = list(
          pageLength = 50, scrollX = TRUE,
          language   = list(
            url    = "//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json",
            select = list(rows = list(`1`="%d item sélectionné",
                                      `_`="%d items sélectionnés"))
          ),
          columnDefs = list(
            list(width="42%", targets=2),
            list(className="dt-center", targets=c(0,1,3,4))
          ),
          dom = "frtip"
        )
      ) |>
        formatStyle(columns=4,
          backgroundColor=styleEqual(
            c("Non évalué","Non acquis","En cours","Acquis"),
            c("#f8f9fa","#fde2e2","#fff3cd","#d4edda"))) |>
        formatStyle(columns=5,
          color=styleEqual(
            c("Acquis","En cours","Non acquis"),
            c("#27ae60","#e67e22","#e74c3c")))
    }, server=TRUE)

    output$panel_bulk <- renderUI({
      sel <- input$tbl_comp_rows_selected
      n   <- length(sel)

      if (n == 0)
        return(div(class="text-muted", style="padding:16px;text-align:center;",
                   icon("hand-pointer"),
                   p(style="margin-top:8px;",
                     "Sélectionnez une ou plusieurs lignes dans le tableau.")))

      d    <- filtered_data()
      item <- if (n == 1 && sel[1] <= nrow(d)) d[sel[1], ] else NULL

      date_def <- if (!is.null(item)) {
        tryCatch(
          if (!is.na(item$date_eval) && nchar(item$date_eval) > 0)
            as.Date(item$date_eval, format = "%d/%m/%Y") else Sys.Date(),
          error=function(e) Sys.Date())
      } else Sys.Date()

      tagList(
        if (n > 1) div(
          class="alert alert-info",
          style="padding:6px 10px;font-size:0.85em;margin-bottom:10px;",
          icon("bolt"),
          sprintf(" Les valeurs cochées seront appliquées aux %d items.", n),
          br(),
          "Les champs non cochés ", strong("ne seront pas modifiés"), "."
        ),

        div(style="margin-bottom:8px;font-size:0.82em;color:#888;",
            if (n == 1 && !is.null(item)) em(norm_lib(item$libelle))
            else em(paste(n, "items sélectionnés"))),
        hr(style="margin:8px 0;"),

        div(class="bulk-field",
          if (n>1) checkboxInput(ns("apply_autoeval"),
                                 "Modifier l'auto-évaluation", value=TRUE),
          selectInput(ns("edit_autoeval"), "Auto-évaluation (interne)",
                      choices  = STATUT_COMP_CHOICES,
                      selected = if(!is.null(item)) item$autoeval%||%"non_evalue"
                                 else "non_evalue")
        ),
        div(class="bulk-field",
          if (n>1) checkboxInput(ns("apply_senior"),
                                 "Modifier l'évaluation Senior", value=FALSE),
          selectInput(ns("edit_eval_senior"), "Évaluation Senior",
                      choices  = EVAL_RESP_CHOICES,
                      selected = if(!is.null(item)) item$eval_senior%||%"" else "")
        ),
        div(class="bulk-field",
          if (n>1) checkboxInput(ns("apply_evaluateur"),
                                 "Modifier l'évaluateur", value=FALSE),
          textInput(ns("edit_evaluateur"), "Évaluateur Senior (nom / fonction)",
                    value = if(!is.null(item)) item$evaluateur_senior%||%"" else "")
        ),
        div(class="bulk-field",
          if (n>1) checkboxInput(ns("apply_date"),
                                 "Modifier la date", value=FALSE),
          dateInput(ns("edit_date"), "Date d'évaluation",
                    value=date_def, language="fr", format="dd/mm/yyyy")
        ),
        div(class="bulk-field",
          if (n>1) checkboxInput(ns("apply_commentaire"),
                                 "Modifier le commentaire", value=FALSE),
          textAreaInput(ns("edit_commentaire"), "Commentaire", rows=3,
                        value=if(!is.null(item)) item$commentaire%||%"" else "")
        ),

        tags$button(
          id    = ns("btn_save_item"),
          class = "btn action-button btn-block",
          style = paste0("background-color:#27ae60;color:#fff;font-size:1.1em;",
                         "font-weight:600;padding:10px;border:none;",
                         "border-radius:6px;margin-top:8px;width:100%;cursor:pointer;"),
          icon("save"),
          if (n == 1) " Enregistrer" else paste0(" Appliquer aux ", n, " items")
        )
      )
    })

    observeEvent(input$btn_save_item, {
      sel <- input$tbl_comp_rows_selected
      req(sel, user_id_r())
      d <- filtered_data()
      if (nrow(d) == 0 || max(sel) > nrow(d)) return()

      n   <- length(sel)
      uid <- user_id_r()

      apply_ae <- if (n==1) TRUE else isTRUE(input$apply_autoeval)
      apply_sr <- if (n==1) TRUE else isTRUE(input$apply_senior)
      apply_ev <- if (n==1) TRUE else isTRUE(input$apply_evaluateur)
      apply_dt <- if (n==1) TRUE else isTRUE(input$apply_date)
      apply_cm <- if (n==1) TRUE else isTRUE(input$apply_commentaire)

      val_ae  <- input$edit_autoeval
      val_sr  <- if (input$edit_eval_senior == "") NULL else input$edit_eval_senior
      val_ev  <- if (nchar(input$edit_evaluateur) > 0) input$edit_evaluateur else NULL
      val_dt  <- format(input$edit_date, "%d/%m/%Y")
      val_cm  <- if (nchar(input$edit_commentaire) > 0) input$edit_commentaire else NULL

      n_ok <- 0L; n_err <- 0L

      for (idx in sel) {
        if (idx > nrow(d)) next
        item <- d[idx, ]
        tryCatch({
          upsert_eval_competence(
            user_id           = uid,
            ref_id            = item$ref_id,
            autoeval          = if(apply_ae) val_ae else item$autoeval%||%"non_evalue",
            eval_senior       = if(apply_sr) val_sr else item$eval_senior%||%NULL,
            evaluateur_senior = if(apply_ev) val_ev else item$evaluateur_senior%||%NULL,
            date_eval         = if(apply_dt) val_dt else item$date_eval%||%val_dt,
            commentaire       = if(apply_cm) val_cm else item$commentaire%||%NULL
          )
          n_ok <- n_ok + 1L
        }, error=function(e){ n_err <<- n_err + 1L })
      }

      load_data()

      if (n_err == 0)
        showNotification(paste0(n_ok," item(s) enregistré(s) ✓"),
                         type="message", duration=3)
      else
        showNotification(paste0(n_ok," ok / ",n_err," erreur(s)"),
                         type="warning", duration=4)
    })
  })
}
