stats_shooting <- fb_big5_advanced_season_stats(season_end_year = "2023", stat_type= "shooting", team_or_player= "team") %>% 
  filter(Comp == "Premier League" & Team_or_Opponent == "team")

saveRDS(stats_shooting, "./data/stats_shooting.rds")

stats <- fb_big5_advanced_season_stats(season_end_year= "2023", stat_type= "standard", team_or_player = "team") %>% 
  filter(Comp == "Premier League")

saveRDS(stats, "./data/stats.rds")

premier_league_table <- fb_season_team_stats(country = "ENG", gender = "M", season_end_year = "2023", tier = "1st", stat_type = "league_table") %>% 
  arrange(Rk)

saveRDS(premier_league_table, "./data/premier_league_table.rds")

epl_matchday_1to38_table <- tm_matchday_table(country_name="England", start_year="2022", matchday=c(1:38)) %>% 
  distinct(league, matchday, rk, squad, p, pts)
saveRDS(epl_matchday_1to38_table, "epl_matchday_1to38_table.rds")

pl_2022 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "1st")
pl_2022$Wk <- as.numeric(pl_2022$Wk)
saveRDS(pl_2022, "eng_matchweek_detailed.rds")
