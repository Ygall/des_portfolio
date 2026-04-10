# ui.R — Interface utilisateur shinydashboard
# ─────────────────────────────────────────────────────────────────────────────

ui <- function(request) {
  dashboardPage(
    skin = "blue",

    # ── Header ────────────────────────────────────────────────────────────────
    dashboardHeader(
      title = span(
        tags$img(src = "logo_sp.png", height = "28px",
                 style = "margin-right:8px; vertical-align:middle;",
                 onerror = "this.style.display='none'"),
        "e-Portfolio DES SP"
      ),
      titleWidth = 280
    ),

    # ── Sidebar ───────────────────────────────────────────────────────────────
    dashboardSidebar(
      width = 280,
      uiOutput("sidebar_menu")
    ),

    # ── Body ──────────────────────────────────────────────────────────────────
    dashboardBody(

      # CSS + JS
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
        shinyjs::useShinyjs()
      ),

      # Panneau de connexion (affiché si non authentifié)
      uiOutput("login_ui"),

      # Contenu principal (affiché si authentifié)
      uiOutput("main_ui")
    )
  )
}
