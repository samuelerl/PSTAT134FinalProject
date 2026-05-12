# Shiny user interface. Runs after global.R.
# Replace this placeholder layout as the recommender UI takes shape.

ui <- fluidPage(
  titlePanel(APP_TITLE),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText(
        "Boilerplate shell: wire inputs here (movie search, filters, chat, etc.)",
        "once the recommender and NLP pieces live in R/05_recommender.R (or similar)."
      )
    ),
    mainPanel(
      width = 9,
      h4("Status"),
      verbatimTextOutput("app_status")
    )
  )
)
