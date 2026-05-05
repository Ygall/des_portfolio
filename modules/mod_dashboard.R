# modules/mod_dashboard.R — Tableaux de bord
# ─────────────────────────────────────────────────────────────────────────────

mod_dashboard_interne_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Bouton actualiser en haut, bien visible
    fluidRow(
      column(12,
        div(style = "margin-bottom:16px;",
          fluidRow(
            column(4,
              selectInput(ns("filtre_phase"), "Niveau",
                          choices = c("Tous" = "tous", "Base" = "Base", "Avancé" = "Avancé"),
                          selected = "tous")
            ),
            column(4,
              div(style = "padding-top:25px;",
                actionButton(ns("btn_refresh"), "↺  Actualiser le tableau de bord",
                             class = "btn-primary btn-block",
                             icon = icon("rotate")))
            )
          )
        )
      )
    ),
    fluidRow(
      valueBoxOutput(ns("vb_conn"),   width = 4),
      valueBoxOutput(ns("vb_comp"),   width = 4),
      valueBoxOutput(ns("vb_stages"), width = 4)
    ),
    fluidRow(
      box(title = "Connaissances — progression par domaine",
          width = 6, status = "primary", solidHeader = TRUE,
          plotOutput(ns("plot_conn"), height = "520px")),
      box(title = "Compétences — progression par domaine",
          width = 6, status = "info", solidHeader = TRUE,
          plotOutput(ns("plot_comp"), height = "520px"))
    ),
    fluidRow(
      box(title = "Validation des phases", width = 12, status = "success",
          solidHeader = TRUE, DTOutput(ns("tbl_phases")))
    )
  )
}

mod_dashboard_interne_server <- function(id, user_id_r) {
  moduleServer(id, function(input, output, session) {

    refresh_trigger <- reactiveVal(0L)
    observeEvent(input$btn_refresh, { refresh_trigger(refresh_trigger() + 1L) })

    # Toutes les données réactives au refresh_trigger
    d_conn  <- reactive({
      refresh_trigger(); req(user_id_r()); get_eval_connaissances(user_id_r())
    })
    d_comp  <- reactive({
      refresh_trigger(); req(user_id_r()); get_eval_competences(user_id_r())
    })
    d_stage <- reactive({
      refresh_trigger(); req(user_id_r()); get_stages(user_id_r())
    })
    d_phase <- reactive({
      refresh_trigger(); req(user_id_r()); get_phases(user_id_r())
    })

    # ── Value boxes ────────────────────────────────────────────────────────────
    output$vb_conn <- renderValueBox({
      d <- d_conn(); n_a <- sum(d$autoeval == "acquis", na.rm=TRUE); n_t <- nrow(d)
      pct <- if (n_t>0) round(100*n_a/n_t) else 0
      valueBox(paste0(n_a,"/",n_t," (",pct,"%)"), "Connaissances acquises",
               icon = icon("book"),
               color = if(pct>=70)"green" else if(pct>=40)"yellow" else "red")
    })
    output$vb_comp <- renderValueBox({
      d <- d_comp(); n_a <- sum(d$autoeval %in% c("acquis","en_cours"), na.rm=TRUE); n_t <- nrow(d)
      pct <- if (n_t>0) round(100*n_a/n_t) else 0
      valueBox(paste0(n_a,"/",n_t," (",pct,"%)"), "Compétences acquises/en cours",
               icon = icon("star"),
               color = if(pct>=70)"green" else if(pct>=40)"yellow" else "red")
    })
    output$vb_stages <- renderValueBox({
      d   <- d_stage()
      n_r <- sum(!is.na(d$lieu) & nchar(d$lieu)>0, na.rm=TRUE)
      n_v <- sum(!is.na(d$stage_valide) & d$stage_valide==1L, na.rm=TRUE)
      valueBox(
        paste0(n_v," validé(s) / ",n_r," renseigné(s)"),
        "Stages",
        icon  = icon("hospital"),
        color = if(n_v==n_r && n_r>0)"green" else "yellow"
      )
    })

    # ── Barplot helper ─────────────────────────────────────────────────────────
    .barplot <- function(d, cols_statut, labels_statut, colors, titre, filtre) {
      if (!is.null(filtre) && filtre != "tous") d <- d[d$niveau == filtre, ]
      if (nrow(d) == 0)
        return(ggplot() + theme_void() +
                 labs(title = paste0(titre, if(!is.null(filtre)&&filtre!="tous")
                   paste0(" [",filtre,"]") else "")) +
                 theme(plot.title = element_text(face="bold", size=15)))

      prog <- d |>
        mutate(alias = .domaine_alias(domaine)) |>
        group_by(alias) |>
        summarise(
          counts = list(setNames(
            sapply(cols_statut, function(st) sum(autoeval == st, na.rm=TRUE)),
            cols_statut
          )),
          .groups = "drop"
        ) |>
        tidyr::unnest_wider(counts) |>
        tidyr::pivot_longer(all_of(cols_statut), names_to="s", values_to="n") |>
        mutate(s = factor(s, levels=cols_statut, labels=labels_statut))

      ggplot(prog, aes(x=reorder(alias,n), y=n, fill=s)) +
        geom_col(position="stack", width=0.68) +
        coord_flip() +
        scale_fill_manual(values=colors) +
        labs(title=paste0(titre,
               if(!is.null(filtre)&&filtre!="tous") paste0(" [",filtre,"]") else ""),
             x=NULL, y="Items", fill=NULL) +
        theme_minimal(base_size=14) +
        theme(legend.position   = "bottom",
              panel.grid.major.y = element_blank(),
              plot.title         = element_text(face="bold", size=15),
              axis.text.y        = element_text(size=13),
              legend.text        = element_text(size=13))
    }

    output$plot_conn <- renderPlot({
      .barplot(d_conn(),
               c("non_evalue","non_acquis","acquis"),
               c("Non évalué","Non acquis","Acquis"),
               c("Non évalué"="#adb5bd","Non acquis"="#e74c3c","Acquis"="#27ae60"),
               "Connaissances", input$filtre_phase)
    }, bg="transparent")

    output$plot_comp <- renderPlot({
      .barplot(d_comp(),
               c("non_evalue","non_acquis","en_cours","acquis"),
               c("Non évalué","Non acquis","En cours","Acquis"),
               c("Non évalué"="#adb5bd","Non acquis"="#e74c3c","En cours"="#f39c12","Acquis"="#27ae60"),
               "Compétences", input$filtre_phase)
    }, bg="transparent")

    output$tbl_phases <- renderDT({
      dp <- d_phase()
      df <- data.frame(
        Phase = PHASE_LABELS,
        Avis  = sapply(PHASES, function(ph) {
          row <- dp[dp$phase==ph,]
          if (nrow(row)==0) "En attente"
          else switch(row$avis_commission[1]%||%"",
            valide="Validé", non_valide="Non validé", ajourne="Ajourné", "En attente")
        }),
        Date  = sapply(PHASES, function(ph) {
          row <- dp[dp$phase==ph,]
          if (nrow(row)==0) "—" else row$date_validation[1]%||%"—"
        }),
        Signataire = sapply(PHASES, function(ph) {
          row <- dp[dp$phase==ph,]
          if (nrow(row)==0) "—" else row$validateur[1]%||%"—"
        }),
        stringsAsFactors=FALSE
      )
      datatable(df, rownames=FALSE, options=list(dom="t", pageLength=3)) |>
        formatStyle("Avis",
          backgroundColor=styleEqual(
            c("Validé","Non validé","Ajourné","En attente"),
            c("#d4edda","#fde2e2","#fff3cd","#f8f9fa")))
    }, server=FALSE)
  })
}

# ── DASHBOARD COORDINATEUR ─────────────────────────────────────────────────────

mod_dashboard_coord_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title="Sélectionner un interne", width=4, status="primary", solidHeader=TRUE,
          selectInput(ns("sel_promotion"), "Promotion", choices=NULL),
          checkboxInput(ns("include_termine"), "Inclure les internes ayant terminé", FALSE),
          selectInput(ns("sel_interne"), "Interne", choices=NULL),
          actionButton(ns("btn_voir"), "Voir le portfolio",
                       class="btn-primary btn-block", icon=icon("eye"))),
      valueBoxOutput(ns("vb_n"),   width=4),
      valueBoxOutput(ns("vb_moy"), width=4)
    ),
    fluidRow(
      box(title="Suivi de la promotion", width=12, status="primary", solidHeader=TRUE,
          DTOutput(ns("tbl_promotion")))
    ),
    fluidRow(
      box(title="Distribution connaissances", width=6, status="info",
          plotOutput(ns("plot_dist_conn"), height="280px")),
      box(title="Distribution compétences",  width=6, status="info",
          plotOutput(ns("plot_dist_comp"), height="280px"))
    )
  )
}

mod_dashboard_coord_server <- function(id, user, interne_cible) {
  moduleServer(id, function(input, output, session) {

    observe({
      promos <- db_query(
        "SELECT DISTINCT promotion FROM users WHERE role='interne' AND active=1 ORDER BY promotion")$promotion
      updateSelectInput(session, "sel_promotion",
                        choices=c("Toutes"="toutes", setNames(promos,promos)))
    })

    observe({
      d <- get_all_internes(include_termine=isTRUE(input$include_termine))
      if (!is.null(input$sel_promotion) && input$sel_promotion != "toutes")
        d <- d[d$promotion==input$sel_promotion,]
      ch <- setNames(as.character(d$id),
                     paste0(d$nom," ",d$prenom,
                            ifelse(d$statut=="internat_termine"," [T]","")))
      updateSelectInput(session, "sel_interne", choices=c("— Choisir —"="",ch))
    })

    observeEvent(input$btn_voir, {
      req(input$sel_interne, nchar(input$sel_interne)>0)
      interne_cible(as.integer(input$sel_interne))
    })

    internes_data <- reactive({
      d <- get_all_internes(include_termine=isTRUE(input$include_termine))
      if (!is.null(input$sel_promotion) && input$sel_promotion != "toutes")
        d <- d[d$promotion==input$sel_promotion,]
      d
    })

    output$vb_n <- renderValueBox(
      valueBox(nrow(internes_data()), "Internes suivis",
               icon=icon("users"), color="blue"))

    output$vb_moy <- renderValueBox({
      internes <- internes_data()
      if (nrow(internes)==0)
        return(valueBox("—","Moy. conn. acquises",icon=icon("book"),color="yellow"))
      pcts <- sapply(internes$id, function(uid) {
        d <- get_eval_connaissances(uid)
        if (nrow(d)==0) 0 else round(100*sum(d$autoeval=="acquis",na.rm=TRUE)/nrow(d),1)
      })
      valueBox(paste0(round(mean(pcts,na.rm=TRUE),1),"%"),
               "Moy. connaissances acquises", icon=icon("percent"), color="green")
    })

    output$tbl_promotion <- renderDT({
      internes <- internes_data()
      if (nrow(internes)==0)
        return(datatable(data.frame(Message="Aucun interne"), rownames=FALSE))
      rows <- lapply(seq_len(nrow(internes)), function(i) {
        uid <- internes$id[i]
        dc  <- get_eval_connaissances(uid)
        dco <- get_eval_competences(uid)
        ds  <- get_stages(uid)
        dp  <- get_phases(uid)
        data.frame(
          Interne   = paste(internes$nom[i], internes$prenom[i]),
          Statut    = ifelse(internes$statut[i]=="internat_termine","Terminé","Actif"),
          Promotion = internes$promotion[i],
          `% Conn.` = if(nrow(dc)==0)"0%" else
            paste0(round(100*sum(dc$autoeval=="acquis",na.rm=TRUE)/nrow(dc),1),"%"),
          `% Comp.` = if(nrow(dco)==0)"0%" else
            paste0(round(100*sum(dco$autoeval%in%c("acquis","en_cours"),na.rm=TRUE)/nrow(dco),1),"%"),
          Stages    = paste0(sum(!is.na(ds$lieu)&nchar(ds$lieu)>0,na.rm=TRUE),"/",nrow(ds)),
          Phases    = paste0(sum(dp$avis_commission=="valide",na.rm=TRUE),"/3"),
          check.names=FALSE, stringsAsFactors=FALSE)
      })
      datatable(do.call(rbind,rows), rownames=FALSE,
        options=list(pageLength=20,
          language=list(url="//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"))) |>
        formatStyle("Statut",
          backgroundColor=styleEqual(c("Terminé","Actif"),c("#e9ecef","#d4edda")))
    }, server=TRUE)

    .dist <- function(evals, col, lvls, lbls, cols, titre) {
      if (is.null(evals) || nrow(evals)==0) return(NULL)
      cnt <- as.data.frame(table(statut=evals[[col]]))
      cnt$statut <- factor(cnt$statut, levels=lvls, labels=lbls)
      ggplot(cnt, aes(statut, Freq, fill=statut)) +
        geom_col(width=.55) +
        geom_text(aes(label=Freq), vjust=-0.3, size=4.5) +
        scale_fill_manual(values=cols) +
        labs(x=NULL, y=NULL, title=titre) +
        theme_minimal(base_size=14) +
        theme(legend.position="none",
              plot.title=element_text(face="bold", size=14))
    }

    output$plot_dist_conn <- renderPlot({
      d <- do.call(rbind, lapply(internes_data()$id, get_eval_connaissances))
      .dist(d, "autoeval",
            c("non_evalue","non_acquis","acquis"),
            c("Non évalué","Non acquis","Acquis"),
            c("Non évalué"="#adb5bd","Non acquis"="#e74c3c","Acquis"="#27ae60"),
            "Connaissances — promotion")
    }, bg="transparent")

    output$plot_dist_comp <- renderPlot({
      d <- do.call(rbind, lapply(internes_data()$id, get_eval_competences))
      .dist(d, "autoeval",
            c("non_evalue","non_acquis","en_cours","acquis"),
            c("Non évalué","Non acquis","En cours","Acquis"),
            c("Non évalué"="#adb5bd","Non acquis"="#e74c3c","En cours"="#f39c12","Acquis"="#27ae60"),
            "Compétences — promotion")
    }, bg="transparent")
  })
}
