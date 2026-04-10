# modules/mod_auth.R — Authentification locale
# ─────────────────────────────────────────────────────────────────────────────

mod_auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    div(id = ns("login_panel"),
      div(class = "login-container",
        div(class = "login-box",
          tags$img(src = "logo_sp.png", height = "60px",
                   style = "margin-bottom:12px;",
                   onerror = "this.style.display='none'"),
          h3(APP_TITLE, style = "margin-bottom:24px; color:#2c3e50; font-size:1.3em;"),
          div(class = "form-group",
            tags$label("Identifiant", `for` = ns("username")),
            textInput(ns("username"), label = NULL, placeholder = "username")
          ),
          div(class = "form-group",
            tags$label("Mot de passe", `for` = ns("password")),
            passwordInput(ns("password"), label = NULL, placeholder = "••••••••")
          ),
          actionButton(ns("btn_login"), "Se connecter",
                       class = "btn btn-primary btn-block",
                       style = "margin-top:10px;"),
          uiOutput(ns("login_msg"))
        )
      )
    )
  )
}

mod_auth_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    user <- reactiveVal(NULL)

    observeEvent(input$btn_login, {
      req(input$username, input$password)
      info <- verify_user(trimws(input$username), input$password)
      if (is.null(info)) {
        output$login_msg <- renderUI(
          div(class = "alert alert-danger", style = "margin-top:10px;",
              icon("exclamation-triangle"),
              " Identifiant ou mot de passe incorrect."))
      } else {
        user(info)
        shinyjs::hide("login_panel")
      }
    })

    return(user)
  })
}
