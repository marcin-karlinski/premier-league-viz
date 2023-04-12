library(shiny)
library(worldfootballR)
library(tidyverse)
library(highcharter)

stats_shooting <- fb_big5_advanced_season_stats(season_end_year= "2023", stat_type= "shooting", team_or_player= "team") %>% 
  filter(Comp == "Premier League" & Team_or_Opponent == "team")

stats <- fb_big5_advanced_season_stats(season_end_year= "2023", stat_type= "standard", team_or_player= "team") %>% 
  filter(Comp == "Premier League")

ui <- fluidPage(
  fluidRow(highchartOutput("hc1"))
)

server <- function(input, output, session) {
  output$hc1 <- renderHighchart({
    stats %>% 
      arrange(-`np:G_minus_xG_Expected`) %>% 
      hchart(
      'bar', hcaes(x = Squad, y = `np:G_minus_xG_Expected`),
      color = "#0073C2FF"
    )
      
  })
}

shinyApp(ui, server)