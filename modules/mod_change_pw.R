# modules/mod_change_pw.R — Changement de mot de passe (tous rôles)
# ─────────────────────────────────────────────────────────────────────────────

mod_change_pw_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4, offset = 4,
      box(title = "Changer mon mot de passe", width = NULL,
          status = "warning", solidHeader = TRUE,
          p(class = "text-muted", style = "margin-bottom:16px;",
            icon("info-circle"),
            " Le mot de passe doit faire au moins 8 caractères."),
          passwordInput(ns("pw_current"),  "Mot de passe actuel"),
          passwordInput(ns("pw_new"),      "Nouveau mot de passe"),
          passwordInput(ns("pw_confirm"),  "Confirmer le nouveau mot de passe"),
          br(),
          actionButton(ns("btn_save_pw"), "Enregistrer le nouveau mot de passe",
                       class = "btn-success btn-block",
                       icon = icon("lock")),
          uiOutput(ns("msg"))
      )
    )
  )
}

mod_change_pw_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$btn_save_pw, {
      req(user())

      pw_cur  <- input$pw_current
      pw_new  <- input$pw_new
      pw_conf <- input$pw_confirm

      # Validations
      if (nchar(trimws(pw_cur)) == 0) {
        output$msg <- renderUI(div(class="alert alert-danger",
          icon("exclamation-triangle"), " Entrez votre mot de passe actuel."))
        return()
      }
      if (nchar(pw_new) < 8) {
        output$msg <- renderUI(div(class="alert alert-danger",
          icon("exclamation-triangle"), " Le nouveau mot de passe doit faire au moins 8 caractères."))
        return()
      }
      if (!identical(pw_new, pw_conf)) {
        output$msg <- renderUI(div(class="alert alert-danger",
          icon("exclamation-triangle"), " Les deux mots de passe ne correspondent pas."))
        return()
      }

      # Vérifier le mot de passe actuel
      row <- db_query("SELECT hashed_pw FROM users WHERE id=?", list(user()$id))
      if (nrow(row) == 0) {
        output$msg <- renderUI(div(class="alert alert-danger", " Utilisateur introuvable."))
        return()
      }

      if (!bcrypt::checkpw(pw_cur, row$hashed_pw[1])) {
        output$msg <- renderUI(div(class="alert alert-danger",
          icon("exclamation-triangle"), " Mot de passe actuel incorrect."))
        return()
      }

      # Enregistrement
      tryCatch({
        db_execute("UPDATE users SET hashed_pw=? WHERE id=?",
                   list(bcrypt::hashpw(pw_new), user()$id))

        # Vider les champs
        updateTextInput(session, "pw_current", value = "")
        updateTextInput(session, "pw_new",     value = "")
        updateTextInput(session, "pw_confirm", value = "")

        output$msg <- renderUI(div(class="alert alert-success",
          icon("check"), " Mot de passe modifié avec succès."))
      }, error = function(e) {
        output$msg <- renderUI(div(class="alert alert-danger",
          " Erreur : ", e$message))
      })
    })
  })
}
