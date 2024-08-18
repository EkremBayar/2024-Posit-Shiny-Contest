# Picker Input
observe({
  req(input$position_filter)
  input$position_filter
  
  selected_pos <- position_selection(input$position_filter)
  
  filtered_pos <- position_filter(input$position_filter)
  
  temp <- df %>% filter(MainPosition %in% filtered_pos)
  
  updatePickerInput(
    session, "player_filter", choices = temp$Name, selected = isolate(rvList[[selected_pos]]),
    choicesOpt = list(
      content = paste0(
        "<img style='margin-left:3px' src='https://tmssl.akamaized.net/images/flagge/tiny/",temp$TeamId,".png' width='15px'> ",
        "<img src='", temp$image_url, "' width='25px'> ", temp$Name,"</img>"
        
      ))
  )
  
})


# Selected Players to Reactive
observeEvent(input$player_filter, {
  selected_pos <- position_selection(input$position_filter)
  rvList[[selected_pos]] <- input$player_filter

})


