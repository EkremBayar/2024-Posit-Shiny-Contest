# Render Plot
output$shadow_team <- renderPlot({
  rvList$Plot
})

# Download Your Squad with Action button
observeEvent(input$download_shadowlist, {
  disable("download_shadowlist")
  ggsave(paste0(root_path, "/www/output/euro24_your_team.png"), 
         plot = rvList$Plot, width = 20, height = 11.25, units = "in", dpi = 300)
  
  Sys.sleep(0.5)
  runjs("$('#hidden_download_shadowlist')[0].click();") 
  enable("download_shadowlist")
  
})

output$hidden_download_shadowlist <- downloadHandler(
  
  
  filename = function() {
    "euro24_your_team.png"
  },
  content = function(file) {
    file.copy(paste0(root_path, "/www/output/euro24_your_team.png"), file)
  }
  
)