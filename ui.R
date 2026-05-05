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
        shinyjs::useShinyjs(),
        tags$script(HTML('
          document.addEventListener("DOMContentLoaded", function() {
            var resizer = document.createElement("div");
            resizer.id = "sidebar-resizer";
            document.body.appendChild(resizer);

            var sidebar = document.querySelector(".main-sidebar");
            var content = document.querySelector(".content-wrapper");
            var nav     = document.querySelector(".main-header .navbar");
            var isDragging = false;

            function setSidebarWidth(w) {
              w = Math.max(180, Math.min(420, w));
              sidebar.style.width = w + "px";
              if (content) content.style.marginLeft = w + "px";
              if (nav)     nav.style.marginLeft     = w + "px";
              resizer.style.left = w + "px";
            }

            resizer.addEventListener("mousedown", function(e) {
              isDragging = true;
              resizer.classList.add("dragging");
              e.preventDefault();
            });
            document.addEventListener("mousemove", function(e) {
              if (!isDragging) return;
              setSidebarWidth(e.clientX);
            });
            document.addEventListener("mouseup", function() {
              isDragging = false;
              resizer.classList.remove("dragging");
            });
          });
        '))
      ),

      # Panneau de connexion (affiché si non authentifié)
      uiOutput("login_ui"),

      # Contenu principal (affiché si authentifié)
      uiOutput("main_ui")
    )
  )
}
