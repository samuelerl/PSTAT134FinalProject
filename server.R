# Shiny server logic. Runs after global.R and ui.R.
# Keep reactive work here; call functions defined under R/ for clarity.

server <- function(input, output, session) {
  output$app_status <- renderText({
    paste0(
      "Shiny ", packageVersion("shiny"), " | ",
      APP_TITLE, " | v", APP_VERSION, "\n",
      "global.R has run; add outputs and reactives as the project matures."
    )
  })
}
