# server.R
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Auth ────────────────────────────────────────────────────────────────────
  user <- mod_auth_server("auth")

  output$login_ui <- renderUI({
    if (is.null(user())) mod_auth_ui("auth") else NULL
  })

  # ── Interne cible (coord/admin consultent le portfolio d'un interne) ────────
  interne_cible <- reactiveVal(NULL)

  observe({
    req(user())
    if (user()$role == "interne") interne_cible(user()$id)
  })

  user_id_r <- reactive({
    req(user())
    if (user()$role == "interne") user()$id else interne_cible()
  })

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  output$sidebar_menu <- renderUI({
    req(user())
    role <- user()$role

    # ── Sélecteur interne (coord/admin) ────────────────────────────────────────
    selector_interne <- if (role %in% c("coordinateur","admin")) {
      div(style = paste0(
            "background:rgba(0,0,0,0.2); border-radius:6px;",
            "margin:6px 10px 10px; padding:10px;"),
        div(style = "color:#b8c7ce; font-size:0.8em; margin-bottom:6px;",
            icon("user-graduate"), " Interne sélectionné"),
        uiOutput("sidebar_interne_selector"),
        div(style = "margin-top:6px;",
          actionButton("btn_voir_portfolio", "Voir le portfolio",
                       class = "btn btn-block btn-sm",
                       style = "background:#27ae60; color:#fff; border:none; font-weight:600;",
                       icon = icon("eye"))
        )
      )
    } else NULL

    items_interne <- list(
      menuItem("Tableau de bord",       tabName = "dashboard",     icon = icon("gauge")),
      menuItem("Mon identité",          tabName = "identite",      icon = icon("id-card")),
      menuItem("Contrat de formation",  tabName = "contrat",       icon = icon("file-contract")),
      menuItem("Connaissances",         tabName = "connaissances", icon = icon("book-open")),
      menuItem("Compétences",           tabName = "competences",   icon = icon("star")),
      menuItem("Carnet de stages",      tabName = "stages",        icon = icon("hospital")),
      menuItem("Validation des phases", tabName = "phases",        icon = icon("graduation-cap")),
      menuItem("Exporter",              tabName = "export",        icon = icon("download"))
    )

    items_coord <- list(
      menuItem("Suivi de la promotion", tabName = "promotion",     icon = icon("users")),
      menuItem("Portfolio interne",     tabName = "portfolio_int", icon = icon("folder-open")),
      menuItem("Exporter",              tabName = "export",        icon = icon("download")),
      menuItem("Gestion internes",      tabName = "admin",         icon = icon("users-gear"))
    )

    items_admin <- list(
      menuItem("Suivi de la promotion", tabName = "promotion",     icon = icon("users")),
      menuItem("Portfolio interne",     tabName = "portfolio_int", icon = icon("folder-open")),
      menuItem("Exporter",              tabName = "export",        icon = icon("download")),
      menuItem("Administration",        tabName = "admin",         icon = icon("gears"),
               badgeLabel = "admin", badgeColor = "red")
    )

    all_items <- switch(role,
      "admin"        = items_admin,
      "coordinateur" = items_coord,
      items_interne
    )

    tagList(
      # ── Infos utilisateur + déconnexion en haut ────────────────────────────
      div(style = paste0(
            "padding:12px 15px 10px;",
            "border-bottom:1px solid rgba(255,255,255,0.08);",
            "margin-bottom:6px;"),
        div(style = "color:#fff; font-size:0.95em; font-weight:600; margin-bottom:2px;",
            uiOutput("sidebar_username")),
        div(style = "color:#7fb3c8; font-size:0.8em; margin-bottom:8px;",
            toupper(role)),
        actionButton("btn_logout",
                     label = span(icon("right-from-bracket"), " Déconnexion"),
                     style = paste0(
                       "display:block; width:100%; box-sizing:border-box;",
                       "background:#c0392b; color:#fff;",
                       "border:none; border-radius:4px;",
                       "font-size:0.82em; padding:5px 8px;",
                       "cursor:pointer; text-align:center;"))
      ),

      # Sélecteur interne
      selector_interne,

      # Menu principal
      do.call(sidebarMenu, c(list(id = "active_tab"), all_items))
    )
  })

  observeEvent(input$btn_logout, { user(NULL); session$reload() })

  # Nom affiché dans la sidebar — réactif après save_identite (qui sync users)
  output$sidebar_username <- renderUI({
    req(user())
    row <- db_query("SELECT nom, prenom FROM users WHERE id = ?", list(user()$id))
    if (nrow(row) > 0)
      paste(row$prenom[1] %||% "", row$nom[1] %||% "")
    else
      paste(user()$prenom, user()$nom)
  })

  # ── Sélecteur interne (sidebar) ──────────────────────────────────────────────
  output$sidebar_interne_selector <- renderUI({
    req(user())
    if (!user()$role %in% c("coordinateur","admin")) return(NULL)

    actifs   <- get_all_internes(include_termine = FALSE)
    termines <- get_all_internes(include_termine = TRUE)
    termines <- termines[termines$statut == "internat_termine", ]
    all_int  <- rbind(actifs, termines)

    if (nrow(all_int) == 0)
      return(div(style = "color:#888; font-size:0.8em;", "Aucun interne"))

    choices <- setNames(
      as.character(all_int$id),
      paste0(all_int$nom, " ", all_int$prenom,
             ifelse(all_int$statut == "internat_termine", " [T]", ""))
    )
    selectInput("sidebar_interne_id", label = NULL,
                choices  = c("— Choisir —" = "", choices),
                selected = as.character(interne_cible() %||% ""))
  })

  observeEvent(input$btn_voir_portfolio, {
    req(input$sidebar_interne_id, nchar(input$sidebar_interne_id) > 0)
    interne_cible(as.integer(input$sidebar_interne_id))
    updateTabItems(session, "active_tab", "portfolio_int")
  })

  # ── UI principale ────────────────────────────────────────────────────────────
  output$main_ui <- renderUI({
    if (is.null(user())) return(NULL)
    role <- user()$role

    tabs_interne <- list(
      tabItem("dashboard",     h2(icon("gauge"), " Tableau de bord"),
                               mod_dashboard_interne_ui("dash_interne")),
      tabItem("identite",      h2(icon("id-card"), " Fiche d'identité"),
                               mod_identite_ui("identite")),
      tabItem("contrat",       h2(icon("file-contract"), " Contrat de formation"),
                               mod_contrat_ui("contrat")),
      tabItem("connaissances", h2(icon("book-open"), " Connaissances"),
                               mod_connaissances_ui("connaissances")),
      tabItem("competences",   h2(icon("star"), " Compétences"),
                               mod_competences_ui("competences")),
      tabItem("stages",        h2(icon("hospital"), " Carnet de stages"),
                               mod_stages_ui("stages")),
      tabItem("phases",        h2(icon("graduation-cap"), " Validation des phases"),
                               mod_phases_ui("phases")),
      tabItem("export",        h2(icon("download"), " Exports"),
                               mod_export_ui("export"))
    )

    portfolio_int_tabs <- tabsetPanel(
      tabPanel("Tableau de bord",   mod_dashboard_interne_ui("dash_int_coord")),
      tabPanel("Contrat",           mod_contrat_ui("contrat_coord")),
      tabPanel("Connaissances",     mod_connaissances_ui("conn_coord")),
      tabPanel("Compétences",       mod_competences_ui("comp_coord")),
      tabPanel("Carnet de stages",  mod_stages_ui("stages_coord")),
      tabPanel("Validation phases", mod_phases_ui("phases_coord"))
    )

    tabs_coord <- list(
      tabItem("promotion",     h2(icon("users"), " Suivi de la promotion"),
                               mod_dashboard_coord_ui("dash_coord")),
      tabItem("portfolio_int",
        h2(icon("folder-open"), " Portfolio de l'interne sélectionné"),
        uiOutput("interne_info_banner"),
        portfolio_int_tabs
      ),
      tabItem("export",        h2(icon("download"), " Exports"),
                               mod_export_ui("export_coord")),
      tabItem("admin",         h2(icon("users-gear"), " Gestion des internes"),
                               mod_admin_ui("admin_coord"))
    )

    tabs_admin <- list(
      tabItem("promotion",     h2(icon("users"), " Suivi de la promotion"),
                               mod_dashboard_coord_ui("dash_coord")),
      tabItem("portfolio_int",
        h2(icon("folder-open"), " Portfolio de l'interne sélectionné"),
        uiOutput("interne_info_banner"),
        portfolio_int_tabs
      ),
      tabItem("export",        h2(icon("download"), " Exports"),
                               mod_export_ui("export_coord")),
      tabItem("admin",         h2(icon("gears"), " Administration"),
                               mod_admin_ui("admin_full"))
    )

    all_tabs <- switch(role,
      "admin"        = tabs_admin,
      "coordinateur" = tabs_coord,
      tabs_interne
    )

    do.call(tabItems, all_tabs)
  })

  # ── Bannière interne sélectionné ─────────────────────────────────────────────
  output$interne_info_banner <- renderUI({
    cible <- interne_cible()
    if (is.null(cible))
      return(div(class = "alert alert-info",
                 icon("info-circle"),
                 " Sélectionnez un interne dans la barre latérale gauche."))
    row <- db_query("SELECT nom, prenom, promotion, statut FROM users WHERE id=?",
                    list(cible))
    if (nrow(row) == 0) return(NULL)
    div(class = "alert alert-success", style = "margin-bottom:16px;",
        icon("user"), " ", strong(paste(row$prenom, row$nom)),
        " — Promotion : ", row$promotion,
        if (row$statut == "internat_termine")
          span(class = "badge",
               style = "background:#6c757d; margin-left:8px;", "Internat terminé"))
  })

  # ── Initialisation des modules ────────────────────────────────────────────────
  observe({
    req(user())
    role <- user()$role

    if (role == "interne") {
      mod_dashboard_interne_server("dash_interne", user_id_r)
      mod_identite_server("identite",              user_id_r)
      mod_contrat_server("contrat",                user_id_r)
      mod_connaissances_server("connaissances",    user_id_r)
      mod_competences_server("competences",        user_id_r)
      mod_stages_server("stages",                  user_id_r)
      mod_phases_server("phases",                  user_id_r)
      mod_export_server("export", user,            user_id_r)
    }

    if (role %in% c("coordinateur","admin")) {
      mod_dashboard_coord_server("dash_coord", user, interne_cible)
      mod_dashboard_interne_server("dash_int_coord", user_id_r)
      mod_contrat_server("contrat_coord",            user_id_r)
      mod_connaissances_server("conn_coord",          user_id_r)
      mod_competences_server("comp_coord",            user_id_r)
      mod_stages_server("stages_coord",               user_id_r)
      mod_phases_server("phases_coord",               user_id_r)
      mod_export_server("export_coord", user,         user_id_r)
    }

    if (role == "coordinateur") mod_admin_server("admin_coord", user)
    if (role == "admin")        mod_admin_server("admin_full",  user)
  })
}
