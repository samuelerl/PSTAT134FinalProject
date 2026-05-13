# Shiny user interface. Runs after global.R.

ui <- page_fillable(
  title = APP_TITLE,
  theme = bs_theme(version = 5),
  padding = 0,
  gap = 0,
  fillable_mobile = TRUE,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "chat.css")
  ),
  chat_ui()
)
