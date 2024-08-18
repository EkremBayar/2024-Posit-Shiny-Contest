# Creator's Team
observeEvent(input$creator_xi, {
  
  if(input$creator_xi == TRUE){
    
    # Assign Players
    for (i in position_abv) {
      rvList[[i]] <- xi %>% filter(Position == i) %>% pull(Player)
      if(i == "GK"){rvList$CheckGK <- rvList$GK}
    }
    
    # Update Picker
    selected_pos <- position_selection(input$position_filter)
    updatePickerInput(
      session, "player_filter",selected = isolate(rvList[[selected_pos]])
    )
   
  }else{
    
    # Assign NULL
    for (i in position_abv) {
      rvList[[i]] <- NULL
    }
    
    # Update Picker
    selected_pos <- position_selection(input$position_filter)
    updatePickerInput(
      session, "player_filter", selected = character()
      )
    
  }
  
})


