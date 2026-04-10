# modules/mod_phases.R — Validation des phases
# ─────────────────────────────────────────────────────────────────────────────

EXIGENCES_PHASES <- list(
  socle = list(
    connaissances = "Connaissances de base en biostatistiques, épidémiologie, économie de la santé, promotion de la santé + compétences transversales inter-DES",
    stages = "1 stage principal en Santé Publique (méthodes quantitatives) + 1 stage libre"
  ),
  approfondissement = list(
    connaissances = "Connaissances transversales art.3 + base informatique biomédicale, qualité-risques, SHS, environnement-santé + 4 modules avancés",
    stages = "3 stages principaux en Santé Publique + 1 stage libre"
  ),
  consolidation = list(
    connaissances = "Compétences spécifiques conformément à la maquette de Santé Publique",
    stages = "1 stage de 1 an ou 2 stages d'un semestre dans un lieu agréé à titre principal"
  )
)

mod_phases_ui <- function(id) {
  ns <- NS(id)
  tagList(
    lapply(PHASES, function(ph) {
      ex <- EXIGENCES_PHASES[[ph]]
      fluidRow(
        box(
          title = span(icon("graduation-cap"), " ", PHASE_LABELS[[ph]]),
          width = 12, status = if (ph=="socle") "primary" else if (ph=="approfondissement") "info" else "success",
          solidHeader = TRUE, collapsible = TRUE,
          fluidRow(
            column(6,
              div(class = "well well-sm",
                strong("Exigences pédagogiques :"),
                tags$ul(
                  tags$li(strong("Connaissances/Compétences : "), ex$connaissances),
                  tags$li(strong("Stages : "), ex$stages)
                )
              )
            ),
            column(6,
              selectInput(ns(paste0("avis_", ph)), "Avis de la commission",
                          choices = c("En attente" = "", "Validé" = "valide",
                                      "Non validé" = "non_valide", "Ajourné" = "ajourne"),
                          selected = ""),
              textAreaInput(ns(paste0("comment_", ph)),
                            "Commentaire / Justification", rows = 3),
              fluidRow(
                column(6, textInput(ns(paste0("date_", ph)), "Date de validation (JJ/MM/AAAA)")),
                column(6, textInput(ns(paste0("valid_", ph)), "Signataire"))
              ),
              actionButton(ns(paste0("btn_", ph)), "Enregistrer cette phase",
                           class = "btn-primary", icon = icon("save"))
            )
          )
        )
      )
    })
  )
}

mod_phases_server <- function(id, user_id_r) {
  moduleServer(id, function(input, output, session) {

    load_data <- function() {
      req(user_id_r())
      d <- get_phases(user_id_r())
      for (ph in PHASES) {
        row <- d[d$phase == ph, ]
        if (nrow(row) > 0) {
          updateSelectInput(session, paste0("avis_", ph),
                            selected = row$avis_commission[1] %||% "")
          updateTextAreaInput(session, paste0("comment_", ph),
                              value = row$commentaire[1] %||% "")
          updateTextInput(session, paste0("date_", ph),
                          value = row$date_validation[1] %||% "")
          updateTextInput(session, paste0("valid_", ph),
                          value = row$validateur[1] %||% "")
        }
      }
    }

    observe({ req(user_id_r()); load_data() })

    lapply(PHASES, function(ph) {
      observeEvent(input[[paste0("btn_", ph)]], {
        req(user_id_r())
        tryCatch({
          upsert_phase(
            user_id_r(), ph,
            input[[paste0("avis_", ph)]],
            input[[paste0("comment_", ph)]],
            input[[paste0("date_", ph)]],
            input[[paste0("valid_", ph)]]
          )
          showNotification(
            paste(PHASE_LABELS[[ph]], "enregistrée ✓"), type = "message", duration = 2)
        }, error = function(e) showNotification(e$message, type = "error"))
      })
    })
  })
}
