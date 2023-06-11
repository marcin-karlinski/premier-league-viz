library(shiny)
library(worldfootballR)
library(tidyverse)
library(highcharter)
library(tablerDash)

stats_shooting <- fb_big5_advanced_season_stats(season_end_year = "2023", stat_type= "shooting", team_or_player= "team") %>% 
  filter(Comp == "Premier League" & Team_or_Opponent == "team")

stats <- fb_big5_advanced_season_stats(season_end_year= "2023", stat_type= "standard", team_or_player = "team") %>% 
  filter(Comp == "Premier League")

shiny::shinyApp(
  ui = tablerDashPage(
    navbar = tablerDashNav(
      id = "my_menu",
      src = "https://supersport-cms-prod.azureedge.net/media/icxfej42/premier-league.png?width=500",
      navMenu = tablerNavMenu(
        tablerNavMenuItem(
          "Teams",
          tabName = "Teams",
          icon = "flag"
          ),
        tablerNavMenuItem(
          "Players",
          tabName = "Players",
          icon = "users"
        )
      )
    ),
    footer = tablerDashFooter(),
    title = "test",
    enable_preloader = TRUE,
    body = tablerDashBody(
      tablerTabItems(
        tablerTabItem(
          tabName = "Teams",
          fluidRow(highchartOutput("hc1"))
          ),
        tablerTabItem(
          tabName = "Players",
          textOutput("Test2")
        )
      )
    )
  ),
  server = function(input, output) {
    
    output$hc1 <- renderHighchart({
      stats %>% 
        arrange(-`npxG_Per`) %>% 
        hchart(
          'bar', hcaes(x = Squad, y = `npxG_Per`),
          color = "#0073C2FF"
        )
    })
    
    output$Test2 <- renderText("sdasd")
  }
  
)
