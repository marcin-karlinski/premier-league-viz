stats_shooting <- fb_big5_advanced_season_stats(season_end_year = "2023", stat_type= "shooting", team_or_player= "team") %>% 
  filter(Comp == "Premier League" & Team_or_Opponent == "team")

saveRDS(stats_shooting, "./data/stats_shooting.rds")

stats <- fb_big5_advanced_season_stats(season_end_year= "2023", stat_type= "standard", team_or_player = "team") %>% 
  filter(Comp == "Premier League")

saveRDS(stats, "./data/stats.rds")

premier_league_table <- fb_season_team_stats(country = "ENG", gender = "M", season_end_year = "2023", tier = "1st", stat_type = "league_table") %>% 
  arrange(Rk)

saveRDS(premier_league_table, "./data/premier_league_table.rds")

pl_2022 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "1st")
pl_2022$Wk <- as.numeric(pl_2022$Wk)
saveRDS(pl_2022, "eng_matchweek_detailed.rds")

eng_urls <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
match_logs_2023 <- data.frame()
for(i in eng_urls){
  match_logs_2023 <- match_logs_2023 %>% 
    bind_rows(fb_team_match_results(i))
}
match_logs_2023 <- match_logs_2023 %>% 
  filter(Comp == "Premier League") %>% 
  mutate(matchday = as.numeric(gsub("\\D", "", Round)))

dplyr::glimpse(match_logs_2023)

saveRDS(match_logs_2023, "match_logs_2023.rds")

epl_matchday_1to38_table <- tm_matchday_table(country_name="England", start_year="2022", matchday=c(1:38))

epl_matchday_1to38_table <- epl_matchday_1to38_table %>% 
  distinct(league, matchday, rk, squad, p, pts) %>% 
  mutate(squad = case_when(
    squad == "Man City" ~ "Manchester City",
    squad == "Man Utd" ~ "Manchester United",
    squad == "Newcastle" ~ "Newcastle United",
    squad == "Nottm Forest" ~ "Nottingham Forest",
    squad == "Brighton" ~ "Brighton and Hove Albion",
    squad == "Leeds" ~ "Leeds United",
    squad == "Leicester" ~ "Leicester City",
    squad == "Tottenham" ~ "Tottenham Hotspur",
    squad == "West Ham" ~ "West Ham United",
    squad == "Wolves" ~ "Wolverhampton Wanderers",
    TRUE ~ squad
  ))

epl_matchday_1to38_table <- epl_matchday_1to38_table %>% 
  left_join(match_logs_2023, by = c("matchday", "squad" = "Team"))

epl_matchday_1to38_table <- epl_matchday_1to38_table %>% 
  select(Round, Result, Opponent, Venue, GF, GA, matchday, squad, p, pts, rk)

saveRDS(epl_matchday_1to38_table, "epl_matchday_1to38_table.rds")
