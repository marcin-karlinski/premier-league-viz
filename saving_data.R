library(tidyverse)
library(worldfootballR)

#Extracting the final Premier League table
premier_league_table <- fb_season_team_stats(country = "ENG", 
                                             gender = "M", 
                                             season_end_year = "2023", 
                                             tier = "1st", 
                                             stat_type = "league_table") %>% 
  arrange(Rk)

saveRDS(premier_league_table, "./data/premier_league_table.rds")

#Extracting match results for the 2022/2023 season
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

saveRDS(match_logs_2023, "./data/match_logs_2023.rds")
readRDS("./data/match_logs_2023.rds")

#Extracting from Transfermarkt the state of PL table at the end of every GW
epl_matchday_1to38_table <- tm_matchday_table(country_name="England", 
                                              start_year="2022", 
                                              matchday=c(1:38))

#Changing club names to match the names from Fbref
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

#Joining match logs by GW to also show the result of the team for a given GW on the tooltip
epl_matchday_1to38_table <- epl_matchday_1to38_table %>% 
  left_join(match_logs_2023, by = c("matchday", "squad" = "Team"))

epl_matchday_1to38_table <- epl_matchday_1to38_table %>% 
  select(Round, Result, Opponent, Venue, GF, GA, matchday, squad, p, pts, rk)

#saving the full table
saveRDS(epl_matchday_1to38_table, "./data/epl_matchday_1to38_table.rds")


#Shooting stats of PL players - for xG and npxG stats
prem_2023_player_shooting <- fb_league_stats(
  country = "ENG",
  gender = "M",
  season_end_year = 2023,
  tier = "1st",
  stat_type = "shooting",
  team_or_player = "player"
)

saveRDS(prem_2023_player_shooting, "./data/prem_2023_player_shooting.rds")

#Shooting stats of PL players - for xA
prem_2023_player_passing <- fb_league_stats(
  country = "ENG",
  gender = "M",
  season_end_year = 2023,
  tier = "1st",
  non_dom_league_url = NA,
  stat_type = "passing",
  team_or_player = "player"
)

saveRDS(prem_2023_player_passing, "./data/prem_2023_player_passing.rds")

#Extracting goal logs for cumulative goals by GW chart
eng_2023_urls <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
eng_2023_goal_logs <- fb_team_goal_logs(team_urls = eng_2023_urls, for_or_against="for")

saveRDS(eng_2023_goal_logs, "./data/eng_2023_goal_logs.rds")
eng_2023_goal_logs <- readRDS("./data/eng_2023_goal_logs.rds")

eng_2023_goal_logs <- eng_2023_goal_logs %>% 
  filter(Comp == "Premier League" & Notes != "Own Goal") %>% 
  select(Round, Scorer, xG) %>% 
  mutate(Round = as.numeric(gsub("\\D", "", Round))) %>% #getting just GW by number
  mutate(goals = 1)

cumulative_goals <- eng_2023_goal_logs
cumulative_goals$goals <- 1 #adding inofrmation about goal that will be summed later 

#expanding the data frame so that GW where player didnt score have values of 0
players <- unique(cumulative_goals$Scorer)  
matchweeks <- unique(cumulative_goals$Round)
expanded <- expand.grid(Scorer = players, Round = matchweeks)=
cumulative_goals <- merge(cumulative_goals, expanded, all = TRUE)
cumulative_goals$goals[is.na(cumulative_goals$goals)] <- 0

#first order df by Scorer and Round to cumsum
cumulative_goals <- cumulative_goals[order(cumulative_goals$Scorer, cumulative_goals$Round), ]
cumulative_goals$cumulative_goals <- ave(cumulative_goals$goals, cumulative_goals$Scorer, FUN = cumsum, na.rm = TRUE)

#Only keep the top scorers to make the chart more readable
players_to_keep <- cumulative_goals %>% filter(Round == 38 & cumulative_goals >= 15) %>% pull(Scorer)
cumulative_goals <- cumulative_goals %>% 
  filter(Scorer %in% players_to_keep)

saveRDS(cumulative_goals, "./data/cumulative_goals.rds")

##Now prepare data for cumulative xG by minute of the season
#First get detailed match logs from fbref
epl_2023_urls <- fb_match_urls(country = "ENG", gender = "M", season_end_year = 2023, tier="1st")
all_matches_logs_eng_2023 <- fb_match_shooting(epl_2023_urls)

saveRDS(all_matches_logs_eng_2023, "./data/all_matches_logs_eng_2023.rds")
all_matches_logs_eng_2023 <- readRDS("./data/all_matches_logs_eng_2023.rds")

#create df with dates of games for a given team. Will be used to assign GW to a game
gameweeks <- all_matches_logs_eng_2023 %>%
                distinct(Date, Squad)

#Create column with GW based on date of the game
gameweeks <- gameweeks %>% 
  arrange(Squad, Date) %>% 
  group_by(Squad) %>%
  mutate(Matchweek = 1:n())

all_matches_logs_eng_2023_with_GW <- all_matches_logs_eng_2023 %>% 
                                left_join(gameweeks, by = c("Date", "Squad"))

#Create a column with a minute of the season. 
all_matches_logs_eng_2023_with_GW <- all_matches_logs_eng_2023_with_GW %>% 
                                        mutate(Minute = as.numeric(substr(Minute, 1, 2))) %>% #extract just a minute (some values are with stoppage time eg. 45+2)
                                        mutate(cumulative_minute = (Matchweek-1)*90 + Minute) #Cumulative minute of the season

all_matches_logs_eng_2023_with_GW <- all_matches_logs_eng_2023_with_GW %>% 
                                        select(Player, xG, cumulative_minute) %>% 
                                        mutate(Player = trimws(gsub("\\(.*\\)", "", Player))) #Remove '(pen)' from a player name

#Only select 10 players for xG - mainly to be able to merge the expanded df (which is large)
top_players_xG <- readRDS("./data/prem_2023_player_shooting.rds") %>% 
  arrange(-as.numeric(xG_Expected)) %>% 
  slice(1:10) %>% 
  pull(Player)

cumulative_xG <- all_matches_logs_eng_2023_with_GW %>% 
  filter(Player %in% top_players_xG)

#Expand grid with value for every player for every minute - 0 if no xG was recorded
players <- unique(cumulative_xG$Player)
cum_minutes <- 1:3420
expanded <- expand.grid(Player = players, cumulative_minute = cum_minutes)
cumulative_xG <- merge(cumulative_xG, expanded, all = TRUE)
cumulative_xG$xG[is.na(cumulative_xG$xG)] <- 0

cumulative_xG <- cumulative_xG[order(cumulative_xG$Player, cumulative_xG$cumulative_minute), ]
cumulative_xG$cumulative_xG <- ave(cumulative_xG$xG, cumulative_xG$Player, FUN = cumsum, na.rm = TRUE)

saveRDS(cumulative_xG, "cumulative_xG.rds")


#The same but for non-pen xG
all_matches_logs_eng_2023 <- readRDS("./data/all_matches_logs_eng_2023.rds")

gameweeks <- all_matches_logs_eng_2023 %>%
  distinct(Date, Squad)

gameweeks <- gameweeks %>% 
  arrange(Squad, Date) %>% 
  group_by(Squad) %>%
  mutate(Matchweek = 1:n())

all_matches_logs_eng_2023_with_GW <- all_matches_logs_eng_2023 %>% 
  left_join(gameweeks, by = c("Date", "Squad"))

all_matches_logs_eng_2023_with_GW <- all_matches_logs_eng_2023_with_GW %>% 
  mutate(Minute = as.numeric(substr(Minute, 1, 2))) %>% 
  mutate(cumulative_minute = (Matchweek-1)*90 + Minute)

all_matches_logs_eng_2023_with_GW <- all_matches_logs_eng_2023_with_GW %>% 
  select(Player, xG, cumulative_minute) %>% 
  filter(!grepl("\\(.*\\)", Player))

top_players_npxG <- readRDS("./data/prem_2023_player_shooting.rds") %>% 
  arrange(-as.numeric(npxG_Expected)) %>% 
  slice(1:10) %>% 
  pull(Player)

cumulative_npxG <- all_matches_logs_eng_2023_with_GW %>% 
  filter(Player %in% top_players_npxG)

players <- unique(cumulative_npxG$Player)
cum_minutes <- 1:3420
expanded <- expand.grid(Player = players, cumulative_minute = cum_minutes)

cumulative_npxG <- merge(cumulative_npxG, expanded, all = TRUE)
cumulative_npxG$xG[is.na(cumulative_npxG$xG)] <- 0

cumulative_npxG <- cumulative_npxG[order(cumulative_npxG$Player, cumulative_npxG$cumulative_minute), ]
cumulative_npxG$cumulative_npxG <- ave(cumulative_npxG$xG, cumulative_npxG$Player, FUN = cumsum, na.rm = TRUE)
print(cumulative_npxG)

saveRDS(cumulative_npxG, "cumulative_npxG.rds")