#!/usr/bin/env Rscript
# deploy.R — Script de déploiement sur shinyapps.io
# ─────────────────────────────────────────────────────────────────────────────
# Usage :
#   Rscript deploy.R
# Ou depuis RStudio :
#   source("deploy.R")
# ─────────────────────────────────────────────────────────────────────────────

cat("\n========================================\n")
cat("  Déploiement e-Portfolio DES SP\n")
cat("========================================\n\n")

# ── 1. Packages de déploiement ────────────────────────────────────────────────
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  cat("Installation de rsconnect...\n")
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}
library(rsconnect)

# ── 2. Packages applicatifs ───────────────────────────────────────────────────
app_pkgs <- c(
  "shiny", "shinydashboard", "shinyjs",
  "DBI", "RSQLite", "RPostgres",
  "dplyr", "ggplot2", "DT", "bcrypt",
  "rmarkdown", "knitr", "openxlsx",
  "lubridate", "glue", "scales",
  "tidyr", "purrr", "stringr", "tinytex"
)

missing_pkgs <- app_pkgs[!sapply(app_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  cat(sprintf("Installation de : %s\n", paste(missing_pkgs, collapse = ", ")))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

# ── 3. LaTeX pour PDF ─────────────────────────────────────────────────────────
if (!tinytex::is_tinytex()) {
  cat("Installation de tinytex (LaTeX pour PDF)...\n")
  tinytex::install_tinytex()
} else {
  cat("tinytex : OK\n")
}

# ── 4. Configuration rsconnect ────────────────────────────────────────────────
# Remplir vos identifiants depuis https://www.shinyapps.io/admin/#/tokens
# Cliquer "Show" > "Show secret" > copier les 3 valeurs ci-dessous

SHINYAPPS_NAME   <- Sys.getenv("SHINYAPPS_NAME",   "")   # votre nom de compte
SHINYAPPS_TOKEN  <- Sys.getenv("SHINYAPPS_TOKEN",  "")   # token
SHINYAPPS_SECRET <- Sys.getenv("SHINYAPPS_SECRET", "")   # secret

# Si les env vars ne sont pas définies, demander interactivement
if (nchar(SHINYAPPS_NAME) == 0) {
  SHINYAPPS_NAME   <- readline("Nom de compte shinyapps.io : ")
  SHINYAPPS_TOKEN  <- readline("Token : ")
  SHINYAPPS_SECRET <- readline("Secret : ")
}

rsconnect::setAccountInfo(
  name   = SHINYAPPS_NAME,
  token  = SHINYAPPS_TOKEN,
  secret = SHINYAPPS_SECRET
)
cat(sprintf("Compte configuré : %s\n", SHINYAPPS_NAME))

# ── 5. Variables d'environnement PostgreSQL ───────────────────────────────────
# À configurer sur shinyapps.io : Application > Settings > Environment Variables
# Ou via rsconnect avant le déploiement :

PG_HOST     <- Sys.getenv("PG_HOST",     "")
PG_PORT     <- Sys.getenv("PG_PORT",     "5432")
PG_DB       <- Sys.getenv("PG_DB",       "des_portfolio")
PG_USER     <- Sys.getenv("PG_USER",     "")
PG_PASSWORD <- Sys.getenv("PG_PASSWORD", "")

if (nchar(PG_HOST) == 0) {
  cat(paste0(
    "\n⚠️  Variables PostgreSQL non définies.\n",
    "   L'app utilisera SQLite local (données éphémères sur shinyapps.io).\n",
    "   Pour PostgreSQL, définissez ces env vars :\n",
    "     PG_HOST, PG_PORT, PG_DB, PG_USER, PG_PASSWORD\n",
    "   Ou utilisez : rsconnect::configureApp() après déploiement.\n\n"
  ))
}

# ── 6. Déploiement ────────────────────────────────────────────────────────────
APP_NAME  <- "portfolio-des-sp"          # identifiant URL : compte.shinyapps.io/portfolio-des-sp
APP_TITLE <- "e-Portfolio DES SP"

cat(sprintf("\nDéploiement vers : %s.shinyapps.io/%s\n\n", SHINYAPPS_NAME, APP_NAME))

# Fichiers à inclure
app_files <- c(
  "app.R", "global.R", "ui.R", "server.R",
  "install_packages.R",
  "data/referentiel.R",
  "data/init_db.R",
  # Ne PAS inclure data/portfolio_des.sqlite (DB locale non pertinente en prod)
  "modules/mod_auth.R",
  "modules/mod_admin.R",
  "modules/mod_competences.R",
  "modules/mod_connaissances.R",
  "modules/mod_contrat.R",
  "modules/mod_dashboard.R",
  "modules/mod_export.R",
  "modules/mod_identite.R",
  "modules/mod_phases.R",
  "modules/mod_stages.R",
  "templates/portfolio_pdf.Rmd",
  "www/style.css"
)

tryCatch({
  rsconnect::deployApp(
    appDir      = ".",
    appFiles    = app_files,
    appName     = APP_NAME,
    appTitle    = APP_TITLE,
    account     = SHINYAPPS_NAME,
    forceUpdate = TRUE,
    launch.browser = FALSE
  )
  cat(sprintf("\n✓ Déploiement réussi !\n  URL : https://%s.shinyapps.io/%s\n",
              SHINYAPPS_NAME, APP_NAME))
}, error = function(e) {
  cat(sprintf("\n✗ Erreur de déploiement :\n  %s\n", conditionMessage(e)))
})

# ── 7. Configuration post-déploiement (PostgreSQL) ────────────────────────────
if (nchar(PG_HOST) > 0) {
  cat("\nConfiguration des variables d'environnement PostgreSQL...\n")
  tryCatch({
    rsconnect::configureApp(
      appName = APP_NAME,
      account = SHINYAPPS_NAME,
      envVars = list(
        PG_HOST     = PG_HOST,
        PG_PORT     = PG_PORT,
        PG_DB       = PG_DB,
        PG_USER     = PG_USER,
        PG_PASSWORD = PG_PASSWORD
      )
    )
    cat("✓ Variables PostgreSQL configurées.\n")
  }, error = function(e) {
    cat(sprintf("⚠️  Configurer manuellement sur shinyapps.io :\n  %s\n",
                conditionMessage(e)))
  })
}
