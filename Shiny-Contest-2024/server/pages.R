# From Home Page to Team Builder Page
observeEvent(input$next_page, {
  shinyjs::hide("homeland_page")
  shinyjs::show("team_builder_page")
})

# From Team Builder Page to Final Page 
observeEvent(input$sort_submit,{
  removeModal()
  
  # Create Reactive Plot
  pl <- rep(position_abv, 3)
  pl <- pl[order(factor(pl,levels = position_abv))]
  sp <- as.vector(unlist(sapply(position_abv, function(i){rvList[[i]]})))
  
  temp <- data.frame(
    Position = pl,
    Player = sp
  ) %>% 
    left_join(
      df %>% select(Name, Team, date_of_birth, height, foot),
      by = c("Player" = "Name")
    ) %>% 
    mutate(Player = paste0(Player," | ", Team, " | ",  str_sub(date_of_birth, 3,4), "' | ", as.integer(height*100), " | ", str_sub(foot, 1, 1)))
  
  pos_gk <- temp %>% filter(Position == "GK") %>% pull(Player)
  pos_lb <- temp %>% filter(Position == "LB") %>% pull(Player)
  pos_rb <- temp %>% filter(Position == "RB") %>% pull(Player)
  pos_lcb <- temp %>% filter(Position == "LCB") %>% pull(Player)
  pos_rcb <- temp %>% filter(Position == "RCB") %>% pull(Player)
  pos_dm <- temp %>% filter(Position == "DM") %>% pull(Player)
  pos_cm <- temp %>% filter(Position == "CM") %>% pull(Player)
  pos_am <- temp %>% filter(Position == "AM") %>% pull(Player)
  pos_lw <- temp %>% filter(Position == "LW") %>% pull(Player)
  pos_rw <- temp %>% filter(Position == "RW") %>% pull(Player)
  pos_cf <- temp %>% filter(Position == "CF") %>% pull(Player)
  
  rvList$Plot <- ggplot() +
    # Background
    background_image(img) +
    # Logo
    geom_rect(aes(xmin = 3.5, xmax = 16.5, ymin = 105, ymax = 120), color = "darkslategray", fill = "white")+
    geom_image(aes(x = 10, y = 115, image = paste0(root_path, "/www/images/logo.png")), size = 0.06) +
    geom_text(aes(x = 10, y = 108, label = "Euro 24\nTeam Builder"),  lineheight = .8)+
    # Title
    labs(x = NULL, y = NULL) +
    # Limits
    xlim(0, 160) +
    ylim(0, 120) +
    # Annotate Positions
    annotate_pos(10, 60, data.frame("GK" = pos_gk)) +
    annotate_pos(45, 5, data.frame("RB" = pos_rb)) +
    annotate_pos(35, 85, data.frame("LCB" = pos_lcb)) +
    annotate_pos(35, 35, data.frame("RCB" = pos_rcb)) +
    annotate_pos(45, 115, data.frame("LB" = pos_lb)) +
    annotate_pos(80, 35, data.frame("DM" = pos_dm)) +
    annotate_pos(80, 85, data.frame("CM" = pos_cm)) +
    annotate_pos(100, 60, data.frame("AM" = pos_am)) +
    annotate_pos(120, 5, data.frame("RW" = pos_rw)) +
    annotate_pos(120, 115, data.frame("LW" = pos_lw)) +
    annotate_pos(145, 60, data.frame("CF" = pos_cf)) +
    # Theme
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.margin = margin(l = -5, r = -5, b = -5, t = -5)
    ) +
    coord_cartesian()
  
  # Pages
  shinyjs::hide("team_builder_page")
  shinyjs::show("final_page")
})

# From Final Page to Team Builder Page
observeEvent(input$return_teambuilder_page, {
  shinyjs::hide("final_page")
  shinyjs::show("team_builder_page")
})

# Return to Home Page
observeEvent(input$return_homepage, {
  session$reload()
})