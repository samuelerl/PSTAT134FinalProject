# Shiny server logic. Runs after global.R and ui.R.
# Keep reactive work here; call functions defined under R/ for clarity.

server <- function(input, output, session) {
  observeEvent(input$send, {
    showNotification(
      "Recommender model not wired yet — this is a UI-only preview.",
      type = "message",
      duration = 4
    )
  }, ignoreInit = TRUE)
}
