library(shiny)
library(worldfootballR)
library(tidyverse)
library(highcharter)
library(tablerDash)
library(reactable)
library(sparkline)
library(bslib)
library(shinythemes)

premier_league_table <- readRDS("./data/premier_league_table.rds") %>% 
  select(Rk, Squad, W, D, L, GF, GA, GD, xG, xGA, xGD, Pts.MP, Pts)

epl_matchday_1to38_table <- readRDS("./data/epl_matchday_1to38_table.rds")

# pl_2022 <- readRDS("./data/eng_matchweek_detailed.rds")

prem_2023_player_shooting <- readRDS("./data/prem_2023_player_shooting.rds") %>% 
  select(Player, Squad, xG = xG_Expected, npxG = npxG_Expected, Goals = Gls_Standard)

prem_2023_player_passing <- readRDS("./data/prem_2023_player_passing.rds") %>% 
  select(Player, Squad, `Key passes` = KP, xA, Assists = Ast)

cumulative_goals <- readRDS("./data/cumulative_goals.rds")
cumulative_xG <- readRDS("./data/cumulative_xG.rds")
cumulative_npxG <- readRDS("./data/cumulative_npxG.rds")

shiny::shinyApp(
  ui = page_navbar(
    title = "Premier League Viz",
    theme = bs_theme(version = 5, bootswatch = "pulse"),
    # theme = shinytheme("spacelab"),
    tabPanel("panel 1", 
             fluidRow(
              column(12,
               card(
                 max_height = 200,
                 # full_screen = TRUE,
                 card_header("Standings"),
                 reactableOutput("rct_main_table")
               )
              )
                          ),
              fluidRow(
                column(6,
                       card(
                         max_height = 200,
                         # full_screen = TRUE,
                         card_header("A dynamically rendered plot"),
                         reactableOutput("rct_top_scorers")
                       )
                       ),
                column(6,
                       card(
                         max_height = 200,
                         # full_screen = TRUE,
                         card_header("A dynamically rendered plot"),
                         reactableOutput("rct_top_assisters")
                          )
                        )
                      )
             ),
    tabPanel("panel 2", 
             highchartOutput("hc_goals_by_gw")),
    tabPanel("panel 3", "three")
      # src = "https://supersport-cms-prod.azureedge.net/media/icxfej42/premier-league.png?width=500",
  ),
  server = function(input, output) {
  
    
    output$rct_main_table <- renderReactable({
      
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
          
          hchart(my_data, "line", hcaes(x = p, y = rk), 
                 shadow = TRUE) %>% 
            hc_size(height = 250) %>% 
            hc_yAxis(title = list(text = "Position"),
                     reversed = TRUE, max = 20, min = 1,
                     tickPositions = c(1, 5, 10, 15, 20)) %>% 
            hc_xAxis(title = list(text = "Matchweek"),
                     tickPositions = c(1:38)) %>% 
            hc_add_series(
            my_data,
            type="point",
            hcaes(x = p, y = rk, group = Result), color = c("#76766f", "#d81920", "#13cf00"),
            tooltip = list(pointFormat = "{point.Round}<br>{point.squad} {point.GF} : {point.GA} {point.Opponent} ")
            )
        },
        defaultColDef = colDef(
          # maxWidth = 73,
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
            # maxWidth = 40,
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
            # maxWidth = 250,
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
    
    output$rct_top_scorers <- renderReactable({
      
      top_scorers_data <- prem_2023_player_shooting %>% arrange(-Goals) %>% slice(1:8)
      
      reactable(
        top_scorers_data,
        showPageInfo = FALSE,
        pagination = FALSE,
        showPageSizeOptions = FALSE,
        defaultPageSize = 20,
        showSortable = TRUE,
        sortable = TRUE,
        defaultColDef = colDef(
          # maxWidth = 80,
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
          Player = colDef(
            # maxWidth = 250,
            cell = function(value, index) {
              team <- top_scorers_data[index,]$Squad
              img_src <- knitr::image_uri(sprintf("./www/images/%s.svg", team))
              image <- img(src = img_src, style = "height: 24px;", alt = team)
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
          Squad = colDef(show = FALSE),
          Goals = colDef(
            style = list(fontWeight = "bold")
          )
        )
      )
      
    })
    
    output$rct_top_assisters <- renderReactable({
      
      top_assisters_data <- prem_2023_player_passing %>% arrange(-Assists) %>% slice(1:8)
      
      reactable(
        top_assisters_data,
        showPageInfo = FALSE,
        pagination = FALSE,
        showPageSizeOptions = FALSE,
        defaultPageSize = 20,
        showSortable = TRUE,
        sortable = TRUE,
        defaultColDef = colDef(
          # maxWidth = 80,
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
          Player = colDef(
            # maxWidth = 250,
            cell = function(value, index) {
              team <- top_assisters_data[index,]$Squad
              img_src <- knitr::image_uri(sprintf("./www/images/%s.svg", team))
              image <- img(src = img_src, style = "height: 24px;", alt = team)
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
          Squad = colDef(show = FALSE),
          # `Key passes` = colDef(maxWidth = 100),
          Assists = colDef(
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
    
    output$hc_goals_by_gw <- renderHighchart({
      hchart(cumulative_goals, 
             "line", 
             hcaes(x = Round, y = cumulative_goals, group = Scorer)) %>% 
             hc_xAxis(title = list(text = "Matchweek"),
                      tickPositions = c(1:39)) 
    
  })
  
})
