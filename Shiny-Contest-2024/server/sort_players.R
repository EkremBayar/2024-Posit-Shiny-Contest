observeEvent(input$team_builder_submit, {
  
  # Check
  check_positions <- sapply(names(rvList)[names(rvList) %in% position_abv], function(i){length(rvList[[i]])})
  check1 <- check_positions[check_positions > 0 & check_positions < 3]
  check2 <- check_positions[check_positions == 0]
  
  
  if(length(check1) > 0 | length(check2) > 0){
    
    # Render Reactable
    check_table <- data.frame(Count = check_positions) %>% mutate(`Max. 3 Players` = as.character(check_positions == 3)) %>% t() %>% as.data.frame() %>% rownames_to_column(var = " ")
    check_table <- reactable(
      check_table,
      sortable = FALSE,
      filterable = FALSE,
      showPagination = FALSE,
      pagination = FALSE,
      compact = TRUE,
      bordered = TRUE,
      style = "z-index: 0; width:100%; font-size:65%;  font-family: Repo, sans-serif;",
      # Default Col Def
      defaultColDef = colDef(
        headerVAlign = "center",
        align = "center",vAlign = "center",
        maxWidth = 73,
        minWidth = 73,
        html = TRUE,
        headerStyle = list(color = "white", backgroundColor="#1a3151"),
        cell = JS("function(cellInfo) {return cellInfo.value === 'FALSE' ? '\u274c' : cellInfo.value === 'TRUE' ? '\u2714\ufe0f' : cellInfo.value}"),
        style = function(value) {
          if (value == "3") {
            color <- "#02b95d"
          } else if (value == "2") {
            color <- "yellow"
          } else if (value == "1") {
            color <- "orange"
          } else {
            color <- NULL
          }
          list(background = color, fontWeight = "bold")
        }
      )
    )
    output$check_table <- renderReactable({check_table})
    
    # Show Modal
    showModal(
      modalDialog(size = "l",
                  title = tagList(icon("circle-exclamation", style="color:red"), "Team Builder Warning!"),
                  h5("Please, select max. 3 players for each position!", style="color:red;"),
                  div(style="text-align:center", reactableOutput("check_table")),
                  footer = tagList(div(style="text-align:center", modalButton("Cancel")))
      )
    )
    
    
  }else{
    
    # Create Tab Panels
    navs <- do.call(
      navlistPanel, c(id = "sort_position_navlist", lapply(position_abv, function(x){tabPanel(x)}))
    )
    
    
    # Show Modal
    showModal(
      modalDialog(size = "l",
                  title = tagList(icon("sort", style="color:red"), "Team Builder - Sortable"),
                  fluidRow(
                    column(
                      width = 12,
                      column(
                        width = 4,
                        navs
                      ),
                      column(
                        width = 8,
                        h4("Sort Players", style="color:red;"),
                        uiOutput("sortable_columns")
                      )
                    )
                  ),
                  footer = tagList(div(style="text-align:center", actionButton("sort_submit", "Submit", style = "background-color:#02b95d;color:white;"), modalButton("Cancel")))
      )
    )
    
  }
  
})





# Sort Players ------------------------------------------------------------
# Sortable
# Assign sorted players
observeEvent(input$sortable_order_cols,{
  rvList[[input$sort_position_navlist]] <- input$sortable_order_cols
  
})

output$sortable_columns <- renderUI({
  req(input$sort_position_navlist)
  temp <- rvList[[input$sort_position_navlist]]
  rank_list(input_id = "sortable_order_cols", text = "Drag list names to change order",temp, "sortable")
})

