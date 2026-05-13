# Runs once when the Shiny app process starts (before ui.R and server.R).
# Put one-time data loads and heavy precomputation here, not in server.R.

source("R/01_packages.R", local = FALSE)
source("R/ui_chat.R", local = FALSE)

# App metadata (use in ui.R / server.R if helpful).
APP_TITLE <- "PSTAT 134 — Movie recommender (Shiny shell)"
APP_VERSION <- "0.0.0"

# When your data and pipeline scripts are ready, uncomment and order them
# to match your R Markdown workflow (load → clean → features / model).
#
# source("R/02_load_data.R", local = FALSE)
# source("R/03_clean_and_merge.R", local = FALSE)
# source("R/04_eda_plots.R", local = FALSE)
# source("R/05_recommender.R", local = FALSE)
