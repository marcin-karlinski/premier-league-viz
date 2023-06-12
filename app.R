library(shiny)
library(worldfootballR)
library(tidyverse)
library(highcharter)
library(tablerDash)
library(reactable)
library(sparkline)

stats_shooting <- readRDS("./data/stats_shooting.rds")
stats <- readRDS("./data/stats.rds")
premier_league_table <- readRDS("./data/premier_league_table.rds")
epl_matchday_1to38_table <- readRDS("epl_matchday_1to38_table.rds")
pl_2022 <- readRDS("eng_matchweek_detailed.rds")

premier_league_table <- premier_league_table %>% 
  select(Rk, Squad, W, D, L, GF, GA, GD, xG, xGA, xGD, Pts.MP, Pts)

shiny::shinyApp(
  ui = tablerDashPage(
    navbar = tablerDashNav(
      id = "my_menu",
      src = "https://supersport-cms-prod.azureedge.net/media/icxfej42/premier-league.png?width=500",
      navMenu = tablerNavMenu(
        tablerNavMenuItem(
          "Overview",
          tabName = "Overview",
          icon = "flag"
        ),
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
          tabName = "Overview",
          tablerCard(
            width = 12,
            title = "Standings",
            reactableOutput("PL_main_table"),
            status = "success",
            statusSide = "left",
            collapsible = FALSE,
            zoomable = FALSE,
            closable = FALSE
          ),
          tablerCard(
            width = 12,
            title = "Card",
            highchartOutput("hc_test"),
            status = "success",
            statusSide = "left",
            collapsible = FALSE,
            zoomable = FALSE,
            closable = FALSE
          )
        ),
        tablerTabItem(
          tabName = "Teams",
          tablerCard(
            width = 12,
            title = "Card",
            highchartOutput("hc_test2"),
            status = "success",
            statusSide = "left",
            collapsible = FALSE,
            zoomable = FALSE,
            closable = FALSE
            )
          ),
        tablerTabItem(
          tabName = "Players",
          textOutput("Test2")
        )
      )
    )
  ),
  server = function(input, output) {
  
    
    output$PL_main_table <- renderReactable({
      
      reactable(
        premier_league_table,
        showPageInfo = FALSE,
        pagination = FALSE,
        showPageSizeOptions = FALSE,
        defaultPageSize = 20,
        showSortable = TRUE,
        sortable = TRUE,
        details = function(index) {
          
          epl_matchday_1to38_table <- epl_matchday_1to38_table[order(-epl_matchday_1to38_table$p, epl_matchday_1to38_table$rk), ]
          team <- epl_matchday_1to38_table[index,]$squad
          my_data <- epl_matchday_1to38_table[epl_matchday_1to38_table$squad == team,]
          
          results <- pl_2022[pl_2022$Home == premier_league_table[index,]$Squad | pl_2022$Away == premier_league_table[index,]$Squad, ]
          
          my_data <- my_data %>%
            left_join(results, by = c("matchday" = "Wk"))
          
          hchart(my_data, "line", hcaes(x = p, y = rk), tooltip = list(pointFormat = "{point.Home} {point.HomeGoals} : {point.AwayGoals} {point.Away} ")) %>% 
            hc_size(height = 250) %>% 
            hc_yAxis(reversed = TRUE, max = 20, min = 1)
            # hc_add_series(
              # type="point", 
              # hcaes(x = p, y = -rk)
              # tooltip = list(pointFormat = "tooltip with 2 values {point.animal}: {point.hours}")
            # )
        },
        defaultColDef = colDef(
          maxWidth = 73,
          class = JS("function(rowInfo, column, state) {
                    // Highlight sorted columns
                    for (let i = 0; i < state.sorted.length; i++) {
                      if (state.sorted[i].id === column.id) {
                        return 'sorted'
                      }
                    }
                  }")
        ),
        columns = list(
          Rk = colDef(
            name = "P.",
            maxWidth = 40,
            align = "center",
            style = function(value) {
              value <- as.numeric(value)
              if(value < 5){
                color = "#4285F4"
                font_color = "#fff"
              }else if (value < 7){
                color = "#dc8136"
                font_color = "#fff"
              }else if (value == 7){
                color = "#66a55c"
                font_color = "#fff"
              } else if (value >=18){
                color = "#ca503f"
                font_color = "#fff"
              } else {
                color = "#fff"
                font_color = "#000000"
              }
              list(background = color,
                   color = font_color,
                   fontWeight = "bold")
            }
          ),
          Squad = colDef(
            name = "Team",
            maxWidth = 250,
            cell = function(value) {
              img_src <- knitr::image_uri(sprintf("./www/images/%s.svg", value))
              image <- img(src = img_src, style = "height: 24px;", alt = value)
              tagList(
                div(style = "display: inline-block; 
                             margin-left: 0.5rem;
                             font-size: 1.125rem;
                             font-weight: 700;
                             width: 45px", image),
                value
              )
            }
          ),
          Pts = colDef(
            style = list(fontWeight = "bold")
          )
        )
      )
      
    })
    
    output$hc_test <- renderHighchart({
      stats %>% 
        arrange(-`npxG_Per`) %>% 
        hchart(
          'bar', hcaes(x = Squad, y = `npxG_Per`),
          color = "#0073C2FF"
        ) %>% 
        hc_size(height = 250)
    })
    
    output$Test2 <- renderText("sdasd")
  }
  
)
