remove_all_files_in_folder <- function(path){
  
  if(stringr::str_sub(path, -2) != "/*"){
    if(stringr::str_sub(path, -1) == "/"){
      path <- paste0(path, "*")
    }else{
      path <- paste0(path, "/*")
    }
  }
  
  unlink(path, recursive = TRUE, force = TRUE)
}


position_selection <- function(pos){
  selected_pos <- switch (pos,
                          "Goalkeeper" = "GK",
                          "Right Back" = "RB",
                          "Right Centre Back" = "RCB",
                          "Left Centre Back" = "LCB",
                          "Left Back" = "LB",
                          "Defensive Midfield" = "DM",
                          "Central Midfield" = "CM",
                          "Attacking Midfield" = "AM",
                          "Right Winger" = "RW",
                          "Left Winger" = "LW",
                          "Centre Forward" = "CF"
  )
  return(selected_pos)
}


position_filter <- function(pos){
  filtered_pos <- switch (pos,
                          "Goalkeeper" = "Goalkeeper",
                          "Right Back" = "Right-Back" ,
                          "Right Centre Back" = ,
                          "Left Centre Back" = "Centre-Back",
                          "Left Back" = "Left-Back",
                          "Defensive Midfield" = "Defensive Midfield",
                          "Central Midfield" = "Central Midfield" ,
                          "Attacking Midfield" = "Attacking Midfield",
                          "Right Winger" = c("Right Winger", "Right Midfield"),
                          "Left Winger" = c("Left Winger", "Left Midfield"),
                          "Centre Forward" = c("Centre-Forward", "Second Striker")
  )
  return(filtered_pos)
}



# Annotate Pos function
annotate_pos <- function(x, y, data){
  grob <- tableGrob(data,
                    theme = ttheme_gtlight(
                      base_size = 10,
                      colhead = list(bg_params = list(fill = "#002749"),
                                     fg_params = list(col = "white")
                      )
                    ))
  annotation_custom(grob, x = x, xmax = x, ymin = y, ymax = y)
}