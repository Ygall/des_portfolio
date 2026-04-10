# app.R — Point d'entrée
# ─────────────────────────────────────────────────────────────────────────────
# Lancement :
#   shiny::runApp(".", port = 3838, launch.browser = TRUE)
# Production :
#   Rscript -e "shiny::runApp('.', port=3838, host='0.0.0.0')"
# Docker / Shiny Server : pointer le working directory sur ce dossier
# ─────────────────────────────────────────────────────────────────────────────

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
