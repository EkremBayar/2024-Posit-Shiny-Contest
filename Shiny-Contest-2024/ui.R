fluidRow(
  # Homeland 
  div(
    id = "homeland_page",
    div(
      style = "bottom: 50%;text-align: center;",
      br(),
      actionBttn(
        inputId = "next_page",
        size = "lg",
        label = "Create your own team!",
        style = "gradient",
        color = "primary"
      )
    )
  ),
  # Team Builder 
  shinyjs::hidden(div(
    id = "team_builder_page",
    
    br(),
    
    column(
      width = 4,
      wellPanel(
        style = "background-color:#143cda; color:white",
        
        div(
          div(style="float:left",  h4(tags$img(src= "images/logo.png", height=25), "Team Builder")),
          div(style="float:right",  
              prettyCheckbox(
                inputId = "creator_xi",
                label = "Creator's Team", 
                value = FALSE,
                status = "warning",
                shape = "curve",
                outline = TRUE
              )
          )
        ),
        div(style="clear:both;"),
        
        
        pickerInput(
          inputId = "position_filter",
          label = "Select Position",
          choices = position_choices,
          multiple = FALSE,
        ),
        
        pickerInput(
          inputId = "player_filter",
          label = "Select Player", 
          choices = NULL,
          multiple = TRUE,
          options = pickerOptions(
            container = "body", 
            liveSearch = TRUE,
            selectedTextFormat = "count > 0",
            showContent = FALSE,
            size = 10,
            maxOptions = 3,
            maxOptionsText = "Maximum 3 players can be selected for each position!",
            liveSearchPlaceholder = "Arda Güler",
            liveSearchNormalize = TRUE,
            header = "Team Builder"
          )
        ),
        hr(),
        actionButton("team_builder_submit", "Sort Players", icon = icon("sort"), 
                     style = "width:100%; background-color:#02b95d;color:white;"),
        
        br(),br(),
        plotOutput("pitch_plot")
      )
    )
  )),
  # Final Page 
  shinyjs::hidden(div(
    id = "final_page",
    div(class = "centered-plot", 
        wellPanel(
          fluidRow(
            column(width = 12,
                   div(style="float:left;", 
                       actionButton("download_shadowlist", "Download", icon = icon("download"), style = "background-color:#002749; color:white;"),
                       shinyjs::disabled(downloadButton("hidden_download_shadowlist", "D", style = "color:#f5f5f5;background-color:#f5f5f5;border-color:#f5f5f5;"))),
                   div(style="float:right;", 
                       actionButton("return_teambuilder_page", "Team Builder Page", icon = icon("angle-left"), style= "width:155px;background-color:#002749; color:white;"), 
                       actionButton("return_homepage", "Home Page", icon = icon("angles-left"),style="margin-left:10px;width:155px;background-color:#002749; color:white;")),
                   br(),br(),
                   shinycssloaders::withSpinner(plotOutput("shadow_team", width = "1000px", height = "562.5px")))
          )
        ))
  ))
)