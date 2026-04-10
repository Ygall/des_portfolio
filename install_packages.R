#!/usr/bin/env Rscript
# install_packages.R — Installation de toutes les dépendances
# Exécuter une seule fois : Rscript install_packages.R
# ─────────────────────────────────────────────────────────────────────────────

pkgs_cran <- c(
  # Shiny & UI
  "shiny",
  "shinydashboard",
  "shinyjs",
  "DT",

  # Base de données
  "DBI",
  "RSQLite",
  "RPostgres",       # PostgreSQL (production)

  # Manipulation de données
  "dplyr",
  "tidyr",
  "purrr",
  "lubridate",
  "scales",
  "stringr",
  "glue",

  # Visualisation
  "ggplot2",
  "fmsb",          # radarchart

  # Authentification
  "bcrypt",

  # Export
  "rmarkdown",
  "knitr",
  "kableExtra",
  "openxlsx",

  # HTTP (OAuth2 en production)
)

cat("=== Installation des packages R pour e-Portfolio DES SP ===\n\n")

installed <- rownames(installed.packages())
to_install <- pkgs_cran[!pkgs_cran %in% installed]

if (length(to_install) == 0) {
  cat("Tous les packages sont déjà installés.\n")
} else {
  cat(sprintf("Installation de %d package(s) : %s\n\n",
              length(to_install), paste(to_install, collapse = ", ")))
  install.packages(to_install, repos = "https://cloud.r-project.org", dependencies = TRUE)
}

# Vérification
cat("\n=== Vérification ===\n")
missing <- pkgs_cran[!pkgs_cran %in% rownames(installed.packages())]
if (length(missing) > 0) {
  cat(sprintf("ATTENTION — Packages manquants : %s\n", paste(missing, collapse = ", ")))
} else {
  cat("OK — Tous les packages sont disponibles.\n")
}

# Note LaTeX pour PDF
cat("\n=== Note PDF ===\n")
cat("Pour l'export PDF, XeLaTeX doit être installé sur le serveur.\n")
cat("Ubuntu/Debian : sudo apt-get install texlive-xetex texlive-fonts-recommended texlive-lang-french\n")
cat("macOS         : brew install mactex\n")
cat("Ou utiliser tinytex : install.packages('tinytex'); tinytex::install_tinytex()\n\n")

if (!requireNamespace("tinytex", quietly = TRUE)) {
  cat("tinytex non installé — installation...\n")
  install.packages("tinytex", repos = "https://cloud.r-project.org")
  tinytex::install_tinytex()
}
