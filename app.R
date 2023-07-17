library(shiny)
library(worldfootballR)
library(tidyverse)
library(highcharter)
library(reactable)
library(sparkline)
library(bslib)
library(shinythemes)
library(shinyWidgets)
library(htmltools)
library(shinycssloaders)
library(ggplot2)
library(ggsoccer)
library(plotly)

options(highcharter.rjson = FALSE)

premier_league_table <- readRDS("./data/premier_league_table.rds") %>% 
  select(Rk, Squad, W, D, L, GF, GA, GD, xG, xGA, xGD, Pts.MP, Pts)

epl_matchday_1to38_table <- readRDS("./data/epl_matchday_1to38_table.rds")

prem_2023_player_shooting <- readRDS("./data/prem_2023_player_shooting.rds") %>% 
  select(Player, Squad, Goals = Gls_Standard, xG = xG_Expected, npxG = npxG_Expected)

prem_2023_player_passing <- readRDS("./data/prem_2023_player_passing.rds") %>% 
  select(Player, Squad, Assists = Ast, `Key passes` = KP, xA)

prem_2023_player_standard <- readRDS("./data/prem_2023_player_standard.rds")
standard_over_1000_minutes <- readRDS("./data/standard_over_1000_minutes.rds")

prem_2023_player_defense <- readRDS("./data/prem_2023_player_defense.rds")

cumulative_goals <- readRDS("./data/cumulative_goals.rds")
cumulative_xG <- readRDS("./data/cumulative_xG.rds")
cumulative_npxG <- readRDS("./data/cumulative_npxG.rds")
cumulative_assists <- readRDS("./data/cumulative_assists.rds")
cumulative_xA <- readRDS("./data/cumulative_xA.rds")

xGxA_vs_possesions_fotmob <- readRDS("xGxA_vs_possesions_fotmob.rds")

match_details <- readRDS("./data/match_details.rds") %>% 
  select(player_name, x, y, shot_type, on_goal_shot_x, on_goal_shot_y)

bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center", fontWeight = "bold"), label, chart)
}

shiny::shinyApp(
  ui = page_navbar(
    title = "Premier League Stats",
    includeCSS("./www/styling.css"),
    theme = bs_theme(version = 5, bootswatch = "pulse"),
    nav_panel(title = "Summary", 
              fluidRow(
      column(12,
             card(
               full_screen = TRUE,
               card_header("Standings", class = "bg-primary"),
               reactableOutput("rct_main_table")
             )
      )
    ),
    fluidRow(
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Top scorers", class = "bg-primary"),
          reactableOutput("rct_top_scorers")
        ),
        card(
          card_header("Top assisters", class = "bg-primary"),
          reactableOutput("rct_top_assisters")
        )
      )
    ),
    hr(),
    p("This application is intended solely for demonstration and showcase purposes. It is not intended for commercial use. Source of the data is Opta (via Fbref.com).")
    ),
    nav_panel(title = "League statistics",
              fluidRow(
                layout_column_wrap(
                  width = 1/2,
                  card(
                    full_screen = TRUE,
                    min_height = "80vh",
                    card_header(div("Scoring output per 90 minutes",
                                    div(
                                      selectizeInput(
                                        inputId = "goals_metrics_per90_select",
                                        label = "",
                                        choices = c("Goals" = "Gls_Per 90 Minutes", 
                                                    "xG" = "xG_Per 90 Minutes", 
                                                    "npxG" = "npxG_Per 90 Minutes", 
                                                    "Assists" = "Ast_Per 90 Minutes",  
                                                    "xA" = "xAG_Per 90 Minutes", 
                                                    "npxG+xA" = "npxG+xAG_Per 90 Minutes"),
                                        selected = "Gls_Per 90 Minutes"), 
                                      style = "float:right;margin-top:-25px;"), style = "display:inline-block;width:100%;")),
                    highchartOutput("hc_scoring_per90")),
                  card(
                    full_screen = TRUE,
                    min_height = "80vh",
                    card_header("Threat vs regains per 90 minutes"),
                    highchartOutput("hc_threat_vs_regains_per90")),
                )
              ),
             fluidRow(
               layout_column_wrap(
                 width = 1/2,
                      card(
                        full_screen = TRUE,
                        min_height = "80vh",
                        card_header(div("Cumulative goalscoring output by point of season",
                                    div(radioGroupButtons(
                                      inputId = "goals_metrics_by_GW_select",
                                      label = "",
                                      choices = c("Goals", "xG", "npxG"),
                                      selected = "Goals",
                                      individual = TRUE,
                                      status = "secondary"
                                        
                                    ), style = "float:right;margin-top:-25px;"), style = "display:inline-block;width:100%;")),
                        highchartOutput("hc_goals_by_gw")),
                 card(
                   full_screen = TRUE,
                   min_height = "80vh",
                   card_header(div("Cumulative creative output by point of season",
                                   div(radioGroupButtons(
                                     inputId = "assists_metrics_by_GW_select",
                                     label = "",
                                     choices = c("Assists", "xA"),
                                     selected = "Assists",
                                     individual = TRUE,
                                     status = "secondary"
                                   ), style = "float:right;margin-top:-25px;"), style = "display:inline-block;width:100%;")),
                   highchartOutput("hc_assists_by_gw"))
                      )
                  ),
             fluidRow(
               layout_column_wrap(
                 width = 1/2,
                 card(
                   card_header(div("Player shot locations",
                                   div(pickerInput(
                                     inputId = "shot_location_player_select",
                                     label = "",
                                     choices = unique(match_details$player_name),
                                     selected = "Erling Braut Haaland",
                                     options = list( `live-search` = TRUE),
                                   ), style = "float:right;margin-top:-25px;"), style = "display:inline-block;width:100%;")),
                   full_screen = TRUE,
                   min_height = "80vh",
                  plotlyOutput("shot_location_plot")
                 ),
                 card(
                   card_header("Player shot locations"),
                   full_screen = TRUE,
                   min_height = "80vh",
                   plotlyOutput("xGOT_plot")
                 )
              )
             ),
             hr(),
             p("This application is intended solely for demonstration and showcase purposes. It is not intended for commercial use. Source of the data is Opta (via Fbref.com).")
               ),
    nav_panel(title = "Player statistics", p("Second page content."),
              hr(),
              p("This application is intended solely for demonstration and showcase purposes. It is not intended for commercial use. Source of the data is Opta (via Fbref.com).")),
    nav_spacer(),
    nav_item(tags$a(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank"))
  ),
  server = function(input, output) {
  
    
    output$rct_main_table <- renderReactable({
      
      reactable(
        premier_league_table,
        pagination = FALSE,
        showSortable = TRUE,
        # height = "80vh",
        sortable = TRUE,
        rowStyle = function(index) {
          if (index > 0 & index <= 4) list(background = "#c8dbfa",  borderLeft = "solid #4285F4 3px")
          else if (index >= 5 & index < 7) list(background = "#ffe2c9",  borderLeft = "solid #dc8136 3px")
          else if (index == 7) list(background = "#b8fcc1", borderLeft = "solid #66a55c 3px")
          else if (index >= 18) list(background = "#ffaca1", borderLeft = "solid #ca503f 3px")
        },
        details = function(index) {
          
          epl_matchday_1to38_table <- epl_matchday_1to38_table[order(-epl_matchday_1to38_table$p, epl_matchday_1to38_table$rk), ]
          team <- epl_matchday_1to38_table[index,]$squad
          my_data <- epl_matchday_1to38_table[epl_matchday_1to38_table$squad == team,]
          
          tagList(
          br(),
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
          )
        },
        defaultColDef = colDef(
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
            align = "center"
          ),
          Squad = colDef(
            name = "Team",
            minWidth = 700,
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
      
      top_scorers_data <- prem_2023_player_shooting %>% arrange(-Goals)
      
      reactable(
        top_scorers_data,
        showPagination = FALSE,
        pagination = TRUE,
        showSortable = TRUE,
        defaultPageSize = 7,
        sortable = TRUE,
        height = "100%",
        defaultSortOrder = "desc",
        defaultColDef = colDef(
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
            minWidth = 350,
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
            # style = list(fontWeight = "bold"),
            cell = function(value) {
              width <- paste0(value / max(top_scorers_data$Goals) * 100, "%")
              bar_chart(value, width = width)
            }
          )
        )
      )
      
    })
    
    output$rct_top_assisters <- renderReactable({
      
      top_assisters_data <- prem_2023_player_passing %>% arrange(-Assists)
      
      reactable(
        showPagination = FALSE,
        top_assisters_data,
        showPageInfo = FALSE,
        pagination = TRUE,
        showPageSizeOptions = FALSE,
        defaultPageSize = 7,
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
            minWidth = 350,
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
            cell = function(value) {
              width <- paste0(value / max(top_assisters_data$Assists) * 100, "%")
              bar_chart(value, width = width)
            }
          )
        )
      )
      
    })
    
    output$hc_goals_by_gw <- renderHighchart({
      
      if(input$goals_metrics_by_GW_select == "Goals"){
        plot_data <- cumulative_goals %>% 
          mutate(Goals = as.numeric(Goals)) %>% 
          group_by(Player, time_value) %>% 
          summarize(Goals = max(Goals)) %>% 
          ungroup()
      } else if(input$goals_metrics_by_GW_select == "xG"){
        plot_data <- cumulative_xG %>% 
          mutate(xG = as.numeric(xG)) %>% 
          group_by(Player, time_value) %>% 
          summarize(xG = max(xG)) %>% 
          ungroup()
      } else if(input$goals_metrics_by_GW_select == "npxG") {
        plot_data <- cumulative_npxG %>% 
          mutate(npxG = as.numeric(npxG)) %>% 
          group_by(Player, time_value) %>% 
          summarize(npxG = max(npxG)) %>% 
          ungroup() 
      }
      
      xseries <- plot_data %>% 
        select(-time_value) %>%
        # use `name` to name  series according the value of `cat` avariable
        rename(name = Player) %>%
        group_by(name) %>% 
        do(data = list_parse2(.)) %>%
        # add type of series
        mutate(type = "line")
      
      highchart2() %>% 
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_add_series_list(xseries)
    
  })
    
    output$hc_assists_by_gw <- renderHighchart({
      
      if(input$assists_metrics_by_GW_select == "Assists"){
        plot_data <- cumulative_assists %>% 
          mutate(Assists = as.numeric(Assists)) %>% 
          group_by(Player, time_value) %>% 
          summarize(Assists = max(Assists)) %>% 
          ungroup()
      } else if(input$assists_metrics_by_GW_select == "xA"){
        plot_data <- cumulative_xA %>% 
          mutate(xA = as.numeric(xA)) %>% 
          group_by(Player, time_value) %>% 
          summarize(xA = max(xA)) %>% 
          ungroup()
      }
      
      xseries <- plot_data %>% 
        select(-time_value) %>%
        # use `name` to name  series according the value of `cat` avariable
        rename(name = Player) %>%
        group_by(name) %>% 
        do(data = list_parse2(.)) %>%
        # add type of series
        mutate(type = "line")
      
      highchart2() %>% 
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_add_series_list(xseries)
      
    })
    
    output$hc_scoring_per90 <- renderHighchart({
      
      if(input$goals_metrics_per90_select == "npxG+xAG_Per 90 Minutes"){
        
        hc_data <- standard_over_1000_minutes %>%
          arrange(-.data[[input$goals_metrics_per90_select]]) %>% 
          slice(1:10)
        
        hc <- highchart() %>% 
          hc_chart(type = "bar") %>%
          hc_plotOptions(bar = list(stacking = "normal")) %>%
          hc_xAxis(categories = hc_data$Player) %>%
          hc_add_series(name="xA",
                        data = hc_data$`xAG_Per 90 Minutes`) %>%
          hc_add_series(name="npxG",
                        data = hc_data$`npxG_Per 90 Minutes`)
        
      } else{
        
        hc <- standard_over_1000_minutes %>%
          arrange(-.data[[input$goals_metrics_per90_select]]) %>% 
          slice(1:10) %>% 
          hchart('bar', hcaes(x = Player, y = .data[[input$goals_metrics_per90_select]]))
        
      }

      hc
      
    })
    
    
    output$hc_threat_vs_regains_per90 <- renderHighchart({
      
      hc <- xGxA_vs_possesions_fotmob %>% 
        hchart('scatter', hcaes(y = `Possession won final 3rd per 90`, x = `xG + xA per 90`, group = team_name, color = team_color)) %>% 
        hc_tooltip(formatter = JS("function(){
                            return ('Player: ' + this.point.participant_name + '<br> xG + xA per 90: ' + this.x + '<br> Possessions won final 3rd per 90: ' + this.y)
                            }")) %>% 
        hc_legend(enabled = FALSE)
      
      hc
      
    })
    
    output$shot_location_plot <- renderPlotly({
      
      shot_location_data <- match_details %>% 
        filter(player_name == input$shot_location_player_select)
      
      shot_location_chart <- ggplot(shot_location_data) +
        annotate_pitch(dimensions = pitch_international,
                       colour = "white",
                       fill = "#3ab54a") +
        geom_point(aes(x = x, y = y, fill = shot_type),
                   shape = 21,
                   size = 4) +
        coord_flip(xlim = c(49, 110))+ 
        scale_y_reverse() +
        theme_pitch() +
        theme(panel.background = element_rect(fill = "#3ab54a"))
      
      ggplotly(shot_location_chart)
      
    })
  
    output$xGOT_plot <- renderPlotly({
      
      shot_location_data <- match_details %>% 
        filter(player_name == input$shot_location_player_select)
    
      xGOT_chart <- shot_location_data %>% 
        ggplot() +
          geom_point(aes(x = on_goal_shot_x, y = on_goal_shot_y), size = 4) + 
          xlim(c(-2, 4)) +
          ylim(c(-1, 1)) + 
          theme_void() + 
          geom_segment(aes(x = 0, y = 0, xend = 0, yend = 0.67), colour = "gray", linewidth = 3) +
          geom_segment(aes(x = -0.02, y = 0.67, xend = 2.02, yend = 0.67), colour = "gray", linewidth = 2.5) +
          geom_segment(aes(x = 2, y = 0, xend = 2, yend = 0.67), colour = "gray", linewidth = 3) +
          geom_segment(aes(x = -2, y = 0, xend = 4, yend = 0))
      
      ggplotly(xGOT_chart)
      
    })
    
})
