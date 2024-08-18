# Root Path
root_path <- getwd()

# Image Path & Read
img_path <- paste0(root_path,"/www/images/pitch.jpg")
img <- image_read(img_path)

# Remove Output
remove_all_files_in_folder(paste0(root_path,"/www/output"))

# Positions
position_choices <- c(
  "Goalkeeper", 
  "Right Back", "Right Centre Back", "Left Centre Back", "Left Back", 
  "Defensive Midfield", "Central Midfield", "Attacking Midfield", 
  "Right Winger", "Left Winger", "Centre Forward"
  )
position_abv <- c("GK", "LB", "LCB", "RCB", "RB", "DM", "CM", "AM", "LW", "RW", "CF")

# Euro 2024 Data
df <- readRDS(paste0(root_path, "/data/euro24_squads.rds")) %>% arrange(Name)

# Creator's Data
xi <- openxlsx::read.xlsx(paste0(root_path, "/data/creatorsxi.xlsx"))

# Plot Raw Data
plot_data <- data.frame(
  Position = position_abv,
  X = c(10, 
        45 ,32 ,32 ,45,
        80, 80,
        110,
        125,125,
        145
  ),
  Y = c(
    70, 
    125, 95, 45, 15,
    90, 50,
    70,
    125, 15, 
    70
  )
)