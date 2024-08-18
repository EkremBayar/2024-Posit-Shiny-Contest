# Packages ----------------------------------------------------------------
source("library.R")

# Functions ---------------------------------------------------------------
source("function.R")

# Global ------------------------------------------------------------------
source("global.R")


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  
  # Shiny JS
  shinyjs::useShinyjs(),
  
  # CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
    tags$link(rel = "preload", href = "images/euro2024.jpg", as = "image"),
    tags$link(rel = "preload", href = "images/ardaguler.jpg", as = "image"),
    tags$link(rel = "preload", href = "images/spain.jpg", as = "image")
  ),
  
  # Pages
  source(file.path("ui.R"),  local = TRUE, encoding = "UTF-8")$value

  
  
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {

  source(file.path("server", "reactive_values.R"),  local = TRUE, encoding = "UTF-8")$value
  source(file.path("server", "pages.R"),  local = TRUE, encoding = "UTF-8")$value
  source(file.path("server", "select_players.R"),  local = TRUE, encoding = "UTF-8")$value
  source(file.path("server", "draw_pitch.R"),  local = TRUE, encoding = "UTF-8")$value
  source(file.path("server", "sort_players.R"),  local = TRUE, encoding = "UTF-8")$value
  source(file.path("server", "shadow_team.R"),  local = TRUE, encoding = "UTF-8")$value
  source(file.path("server", "creators_xi.R"),  local = TRUE, encoding = "UTF-8")$value
  
}

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)

