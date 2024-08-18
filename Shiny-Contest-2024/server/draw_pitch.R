# Team Builder Page Draw XI
output$pitch_plot <- renderPlot({
  
  # Plot Data
  temp <- data.frame(
    Player = sapply(position_choices, function(i){
      selected_pos <- position_selection(i)
      length(rvList[[selected_pos]])
    })
  ) %>%
    rownames_to_column(var = "Position") %>%
    mutate(Position = sapply(Position, function(i){position_selection(i)}))
  
  # Number of Players
  if(is.null(temp)){
    plot_df <- plot_data %>% mutate(Player = 0)
  }else{
    plot_df <- left_join(plot_data, temp, by = "Position")
  }
  plot_df$Player <- factor(plot_df$Player)

  # Plot
  ggplot(plot_df, aes(X, Y, fill = if_else(Player == 0, "orangered", if_else(Player == 1, "orange", if_else(Player == 2, "goldenrod1", "palegreen4")))))+
    background_image(img)+
    geom_point(show.legend = FALSE, shape = 21, size = 20, color = "darkslategray",position = "identity")+
    geom_text(aes(label = Position), vjust= -0.5, color = "white", size = 4.5, fontface = "bold")+
    geom_text(aes(label = Player), vjust = 1.5, fontface = "bold", color = "white")+
    theme_minimal()+
    theme_void() +
    theme(
      panel.background = element_rect(fill = NULL, colour = NULL),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      line = element_blank(),
      axis.line = element_blank()
    )+
    xlim(0,160)+
    ylim(0,140)+
    scale_fill_identity()

})