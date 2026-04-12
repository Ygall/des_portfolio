# modules/mod_competences.R — Portfolio de compétences
# ─────────────────────────────────────────────────────────────────────────────

mod_competences_ui <- function(id) {
  ns <- NS(id)
  domaines_raw   <- unique(REF_COMPETENCES$domaine)
  domaines_clean <- gsub("\\s+", " ", trimws(domaines_raw))
  domaine_choices <- setNames(domaines_raw, domaines_clean)

  tagList(
    fluidRow(
      column(12,
        div(class="well well-sm",
          fluidRow(
            column(4,
              selectInput(ns("filtre_domaine"),"Filtrer par domaine",
                          choices=c("Tous"="tous", domaine_choices), selected="tous")),
            column(4,
              selectInput(ns("filtre_niveau"),"Niveau",
                          choices=c("Tous"="tous","Base"="Base","Avancé"="Avancé"),
                          selected="tous")),
            column(4,
              selectInput(ns("filtre_statut"),"Statut",
                          choices=c("Tous"="tous","Non évalué"="non_evalue",
                                    "Non acquis"="non_acquis","En cours"="en_cours",
                                    "Acquis"="acquis"),
                          selected="tous"))
          )
        )
      )
    ),
    fluidRow(
      column(8,
        box(title="Référentiel de compétences",width=NULL,status="primary",solidHeader=TRUE,
            div(style="overflow-x:auto;",DTOutput(ns("tbl_comp"))))),
      column(4,
        box(title="Modifier l'item sélectionné",width=NULL,status="warning",solidHeader=TRUE,
            uiOutput(ns("panel_edit"))))
    )
  )
}

mod_competences_server <- function(id, user_id_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    eval_data <- reactiveVal(NULL)

    load_data <- function() {
      req(user_id_r()); eval_data(get_eval_competences(user_id_r()))
    }
    observe({ req(user_id_r()); load_data() })

    filtered_data <- reactive({
      req(eval_data())
      d <- eval_data()
      if (input$filtre_domaine != "tous") d <- d[d$domaine == input$filtre_domaine, ]
      if (input$filtre_niveau  != "tous") d <- d[d$niveau  == input$filtre_niveau,  ]
      if (input$filtre_statut  != "tous") {
        st <- input$filtre_statut
        d <- d[!is.na(d$autoeval) & d$autoeval == st, ]
      }
      d
    })

    label_ae <- function(s) switch(s%||%"non_evalue",
      non_evalue="Non évalué",non_acquis="Non acquis",en_cours="En cours",acquis="Acquis","Non évalué")
    label_sr <- function(s) {
      if(is.null(s)||is.na(s)||identical(s,""))return("—")
      switch(s,acquis="Acquis",en_cours="En cours","Non acquis")
    }
    norm_lib <- function(s) {
      if(is.null(s)||is.na(s)||nchar(s)==0)return(s)
      paste0(toupper(substr(s,1,1)),substr(s,2,nchar(s)))
    }

    output$tbl_comp <- renderDT({
      d <- filtered_data()
      if (nrow(d)==0) {
        return(datatable(
          data.frame(Domaine=character(),Niveau=character(),
                     Competence=character(),`Auto-éval`=character(),Senior=character(),
                     check.names=FALSE),
          colnames=c("Domaine","Niveau","Compétence","Auto-éval","Éval. Senior"),
          rownames=FALSE,
          options=list(dom="t",language=list(emptyTable="Aucun item pour ce filtre."))
        ))
      }
      display <- data.frame(
        Domaine   = sub("^(\\d+) -+\\s*","\\1. ",d$domaine),
        Niveau    = d$niveau,
        Competence= sapply(d$libelle,norm_lib),
        AutoEval  = sapply(d$autoeval,label_ae),
        Senior    = sapply(d$eval_senior,label_sr),
        stringsAsFactors=FALSE
      )
      datatable(display,
        colnames=c("Domaine","Niveau","Compétence","Auto-éval","Éval. Senior"),
        rownames=FALSE,selection="single",
        options=list(pageLength=50,scrollX=TRUE,
          language=list(url="//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"),
          columnDefs=list(list(width="42%",targets=2),list(className="dt-center",targets=c(0,1,3,4))),
          dom="frtip")
      ) |>
        formatStyle(columns=4,
          backgroundColor=styleEqual(
            c("Non évalué","Non acquis","En cours","Acquis"),
            c("#f8f9fa","#fde2e2","#fff3cd","#d4edda"))) |>
        formatStyle(columns=5,
          color=styleEqual(c("Acquis","En cours","Non acquis"),c("#27ae60","#e67e22","#e74c3c")))
    },server=TRUE)

    output$panel_edit <- renderUI({
      sel <- input$tbl_comp_rows_selected
      if(is.null(sel)||length(sel)==0)
        return(div(class="text-muted",style="padding:16px;text-align:center;",
                   icon("hand-pointer"),p(style="margin-top:8px;","Cliquez sur une ligne.")))
      d <- filtered_data()
      if(nrow(d)==0||sel>nrow(d)) return(NULL)
      item <- d[sel,]
      date_def <- tryCatch(
        if(!is.na(item$date_eval)&&nchar(item$date_eval)>0)
          as.Date(item$date_eval,format="%d/%m/%Y") else Sys.Date(),
        error=function(e)Sys.Date())
      tagList(
        div(style="font-size:1.05em;font-weight:600;color:#2c3e50;text-align:center;
                   padding:10px 4px 14px;line-height:1.5;border-bottom:1px solid #e0e0e0;
                   margin-bottom:12px;",
            item$libelle),
        selectInput(ns("edit_autoeval"),"Auto-évaluation (interne)",
                    choices=STATUT_COMP_CHOICES,selected=item$autoeval%||%"non_evalue"),
        selectInput(ns("edit_eval_senior"),"Évaluation Senior",
                    choices=EVAL_RESP_CHOICES,selected=item$eval_senior%||%""),
        textInput(ns("edit_evaluateur"),"Évaluateur Senior (nom / fonction)",
                  value=item$evaluateur_senior%||%""),
        dateInput(ns("edit_date"),"Date d'évaluation",
                  value=date_def,language="fr",format="dd/mm/yyyy"),
        textAreaInput(ns("edit_commentaire"),"Commentaire",rows=3,value=item$commentaire%||%""),
        tags$button(id=ns("btn_save_item"),class="btn action-button btn-block",
          style="background-color:#27ae60;color:#fff;font-size:1.1em;font-weight:600;
                 padding:10px;border:none;border-radius:6px;margin-top:8px;width:100%;cursor:pointer;",
          icon("save")," Enregistrer")
      )
    })

    observeEvent(input$btn_save_item,{
      sel <- input$tbl_comp_rows_selected
      req(sel,user_id_r())
      d <- filtered_data()
      if(nrow(d)==0||sel>nrow(d)) return()
      item <- d[sel,]
      tryCatch({
        upsert_eval_competence(
          user_id           = user_id_r(),
          ref_id            = item$ref_id,
          autoeval          = input$edit_autoeval,
          eval_senior       = if(input$edit_eval_senior=="")NULL else input$edit_eval_senior,
          evaluateur_senior = if(nchar(input$edit_evaluateur)>0)input$edit_evaluateur else NULL,
          date_eval         = format(input$edit_date,"%d/%m/%Y"),
          commentaire       = if(nchar(input$edit_commentaire)>0)input$edit_commentaire else NULL
        )
        load_data()
        showNotification("Compétence enregistrée ✓",type="message",duration=2)
      },error=function(e)showNotification(paste("Erreur :",e$message),type="error"))
    })
  })
}
