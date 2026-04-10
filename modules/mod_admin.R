# modules/mod_admin.R — Administration (admin complet + coord restreint)
# ─────────────────────────────────────────────────────────────────────────────

mod_admin_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("admin_content"))
}

mod_admin_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_admin <- reactive({ !is.null(user()) && user()$role == "admin" })
    is_coord <- reactive({ !is.null(user()) && user()$role %in% c("admin","coordinateur") })

    output$admin_content <- renderUI({
      req(user())
      role <- user()$role

      # Rôles disponibles selon profil
      role_choices <- if (role == "admin")
        c("Interne"="interne","Coordinateur"="coordinateur","Admin"="admin")
      else
        c("Interne"="interne","Coordinateur"="coordinateur")

      tagList(
        # ── Créer un compte ──────────────────────────────────────────────────
        fluidRow(
          box(title = "Créer un compte", width = 12,
              status = if (role=="admin") "danger" else "primary",
              solidHeader = TRUE,
            fluidRow(
              column(3, textInput(ns("f_username"), "Identifiant *")),
              column(3, textInput(ns("f_nom"),      "Nom *")),
              column(3, textInput(ns("f_prenom"),   "Prénom *")),
              column(3, textInput(ns("f_email"),    "Email"))
            ),
            fluidRow(
              column(3, selectInput(ns("f_role"), "Rôle", choices = role_choices)),
              column(3, selectInput(ns("f_statut"), "Statut",
                         choices = c("Actif"="actif","Internat terminé"="internat_termine"))),
              column(3, textInput(ns("f_faculte"),   "Faculté")),
              column(3, textInput(ns("f_promotion"), "Promotion"))
            ),
            fluidRow(
              column(4, passwordInput(ns("f_pw"), "Mot de passe initial *")),
              column(8, style = "padding-top:25px;",
                actionButton(ns("btn_create"), "Créer l'utilisateur",
                             class = "btn-success", icon = icon("user-plus")))
            ),
            uiOutput(ns("msg_create"))
          )
        ),

        # ── Liste et gestion des comptes ──────────────────────────────────────
        fluidRow(
          box(title = "Gestion des comptes", width = 12,
              status = "primary", solidHeader = TRUE,
            # Le coord voit uniquement les internes
            if (role == "coordinateur")
              p(class = "text-muted", icon("info-circle"),
                " En tant que coordinateur, vous voyez uniquement les internes.")
            else NULL,
            DTOutput(ns("tbl_users")),
            br(),
            fluidRow(
              column(4, passwordInput(ns("new_pw"), "Nouveau mot de passe")),
              column(8, style = "padding-top:25px;",
                actionButton(ns("btn_toggle_statut"), "Actif ↔ Internat terminé",
                             class = "btn-default", icon = icon("graduation-cap")),
                if (role == "admin") tagList(
                  actionButton(ns("btn_toggle"), "Activer / Désactiver",
                               class = "btn-info", icon = icon("power-off")),
                  actionButton(ns("btn_reset_pw"), "Réinitialiser MDP",
                               class = "btn-warning", icon = icon("key"))
                ) else NULL
              )
            ),
            uiOutput(ns("msg_action"))
          )
        ),

        # ── Stats et maintenance (admin uniquement) ───────────────────────────
        if (role == "admin") fluidRow(
          box(title = "Statistiques base", width = 6, status = "info", solidHeader = TRUE,
              tableOutput(ns("tbl_stats"))),
          box(title = "Maintenance", width = 6, status = "warning", solidHeader = TRUE,
            actionButton(ns("btn_vacuum"), "VACUUM", class = "btn-default",
                         icon = icon("database")),
            br(), br(),
            downloadButton(ns("dl_backup"), "Backup SQLite",
                           class = "btn-info btn-block")
          )
        ) else NULL
      )
    })

    # ── Données utilisateurs ────────────────────────────────────────────────────
    load_users <- function() {
      req(user())
      if (user()$role == "coordinateur") {
        db_query("SELECT id,username,nom,prenom,email,role,statut,promotion,faculte,active
                  FROM users WHERE role='interne' AND active=1 ORDER BY nom,prenom")
      } else {
        db_query("SELECT id,username,nom,prenom,email,role,statut,faculte,promotion,active,created_at
                  FROM users ORDER BY role,nom,prenom")
      }
    }

    users_rv <- reactiveVal(NULL)
    observe({ req(user()); users_rv(load_users()) })

    output$tbl_users <- renderDT({
      req(users_rv())
      d <- users_rv()
      d$active_lbl <- ifelse(d$active == 1, "Actif", "Désactivé")
      d$statut_lbl <- ifelse(d$statut == "internat_termine", "Internat terminé", "En cours")
      cols <- intersect(c("username","nom","prenom","email","role","statut_lbl",
                          "promotion","faculte","active_lbl"), names(d))
      datatable(d[, cols], rownames = FALSE, selection = "single",
        colnames = c("Login","Nom","Prénom","Email","Rôle","Statut","Promotion","Faculté","Compte")[seq_along(cols)],
        options = list(pageLength = 20, scrollX = TRUE,
          language = list(url="//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json"))
      ) |>
        formatStyle("statut_lbl",
          backgroundColor = styleEqual(
            c("En cours","Internat terminé"), c("#d4edda","#e9ecef"))) |>
        formatStyle("active_lbl",
          backgroundColor = styleEqual(
            c("Actif","Désactivé"), c(NA_character_,"#fde2e2")),
          fontWeight = styleEqual(c("Désactivé"), c("bold")),
          color = styleEqual(c("Désactivé"), c("#c0392b")))
    }, server = TRUE)

    sel_user <- reactive({
      sel <- input$tbl_users_rows_selected
      if (is.null(sel)) return(NULL)
      users_rv()[sel, ]
    })

    # ── Créer ──────────────────────────────────────────────────────────────────
    observeEvent(input$btn_create, {
      req(input$f_username, input$f_nom, input$f_prenom, input$f_pw)
      # Coord ne peut créer que interne/coord
      if (user()$role == "coordinateur" && input$f_role == "admin") {
        output$msg_create <- renderUI(
          div(class="alert alert-danger", "Un coordinateur ne peut pas créer un administrateur."))
        return()
      }
      username <- trimws(input$f_username)
      if (nchar(username) == 0) {
        output$msg_create <- renderUI(div(class="alert alert-danger","L'identifiant est vide."))
        return()
      }
      exists <- db_query("SELECT COUNT(*) AS n FROM users WHERE username=?", list(username))$n
      if (exists > 0) {
        output$msg_create <- renderUI(div(class="alert alert-danger",
          icon("exclamation-triangle"),
          sprintf(" L'identifiant '%s' est déjà utilisé.", username)))
        return()
      }
      tryCatch({
        db_execute(
          "INSERT INTO users(username,nom,prenom,email,role,statut,hashed_pw,faculte,promotion)
           VALUES(?,?,?,?,?,?,?,?,?)",
          list(username, trimws(input$f_nom), trimws(input$f_prenom),
               trimws(input$f_email), input$f_role, input$f_statut,
               bcrypt::hashpw(input$f_pw),
               trimws(input$f_faculte), trimws(input$f_promotion))
        )
        users_rv(load_users())
        output$msg_create <- renderUI(
          div(class="alert alert-success", icon("check"),
              sprintf(" Utilisateur '%s' créé.", username)))
        lapply(c("f_username","f_nom","f_prenom","f_email","f_faculte","f_promotion","f_pw"),
               function(x) updateTextInput(session, x, value = ""))
      }, error = function(e)
        output$msg_create <- renderUI(
          div(class="alert alert-danger", " Erreur : ", e$message)))
    })

    # ── Toggle statut actif ↔ internat_termine ─────────────────────────────────
    observeEvent(input$btn_toggle_statut, {
      row <- sel_user(); req(row)
      if (row$role != "interne") {
        output$msg_action <- renderUI(
          div(class="alert alert-warning", "Ce statut s'applique uniquement aux internes."))
        return()
      }
      new_statut <- if (row$statut == "actif") "internat_termine" else "actif"
      db_execute("UPDATE users SET statut=? WHERE id=?", list(new_statut, row$id))
      users_rv(load_users())
      lbl <- if (new_statut == "internat_termine") "marqué 'Internat terminé'" else "remis à 'Actif'"
      output$msg_action <- renderUI(
        div(class="alert alert-info", sprintf("Compte '%s' %s.", row$username, lbl)))
    })

    # ── Admin seulement : toggle actif/désactivé ───────────────────────────────
    observeEvent(input$btn_toggle, {
      row <- sel_user(); req(row)
      if (row$username == user()$username) {
        output$msg_action <- renderUI(
          div(class="alert alert-warning", "Impossible de modifier votre propre compte."))
        return()
      }
      new_active <- if (row$active == 1) 0L else 1L
      db_execute("UPDATE users SET active=? WHERE id=?", list(new_active, row$id))
      users_rv(load_users())
      output$msg_action <- renderUI(
        div(class="alert alert-info",
            sprintf("Compte '%s' %s.", row$username,
                    if (new_active==1) "activé" else "désactivé")))
    })

    # ── Admin seulement : reset MDP ────────────────────────────────────────────
    observeEvent(input$btn_reset_pw, {
      row <- sel_user(); req(row)
      if (nchar(trimws(input$new_pw %||% "")) < 6) {
        output$msg_action <- renderUI(
          div(class="alert alert-warning", "Le mot de passe doit faire au moins 6 caractères."))
        return()
      }
      tryCatch({
        db_execute("UPDATE users SET hashed_pw=? WHERE id=?",
                   list(bcrypt::hashpw(input$new_pw), row$id))
        users_rv(load_users())
        email_info <- if (nchar(row$email %||% "") > 0)
          paste0(" Email à notifier : ", row$email)
        else " Aucun email renseigné pour cet utilisateur."
        output$msg_action <- renderUI(
          div(class="alert alert-success", icon("check"),
              sprintf(" MDP réinitialisé pour '%s'.", row$username),
              email_info))
        updateTextInput(session, "new_pw", value = "")
      }, error = function(e)
        output$msg_action <- renderUI(div(class="alert alert-danger", e$message)))
    })

    # ── Stats et maintenance (admin) ───────────────────────────────────────────
    output$tbl_stats <- renderTable({
      tables <- c("users","eval_connaissances","eval_competences","stages",
                  "phases_validation","identite","diplomes","contrat_formation")
      data.frame(
        Table  = tables,
        Lignes = sapply(tables, function(t)
          db_query(paste0("SELECT COUNT(*) AS n FROM ", t))$n),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE)

    observeEvent(input$btn_vacuum, {
      db_execute("VACUUM;")
      showNotification("VACUUM exécuté ✓", type = "message", duration = 3)
    })

    output$dl_backup <- downloadHandler(
      filename = function() paste0("backup_", format(Sys.time(),"%Y%m%d_%H%M"), ".sqlite"),
      content  = function(file) file.copy(DB_PATH, file)
    )
  })
}
