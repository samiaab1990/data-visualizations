# libraries 
library(httr)
library(tidyverse)
library(tictoc)

# Script to get all data points for specific player and league data 

# The following function and basketball court is based off the ballR package
# https://github.com/toddwschneider/ballr

# Upfront stats- as of 2/7, Lebron scored 38,390 points
# Lebron ID- 2544

# create default lengths for basketball court (from ballR package)
# location of y is based off hoop_center
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75

hoop_center_y = backboard_offset + neck_length + hoop_radius

# create function for season, seasontype and dat_type
# player_id is player_id associated with each player from stats.nba.com 
# season is a character with "yyyy-yy" format (ie: 2003-04)
# season type can be: preseason, regular season, playoffs or play-in
# dat-type is either player (which gets every single shot made by player in question) 
# or league averages (which is the average of the leage per the season in question)
get_stats<-function(player_id = 2544, season, season_type = "Regular Season", dat_type = "player"){

# standard request headers for API call
# used at end of GET 
# explanation - https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers
request_headers = c(
  "Accept" = "application/json, text/plain, */*",
  "Accept-Language" = "en-US,en;q=0.8",
  "Cache-Control" = "no-cache",
  "Connection" = "keep-alive",
  "Host" = "stats.nba.com",
  "Pragma" = "no-cache",
  "Referer" = "https://www.nba.com/",
  "Upgrade-Insecure-Requests" = "1",
  # using windows - change if different operating system 
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36"
)

request = GET(
  "https://stats.nba.com/stats/shotchartdetail",
  query = list(
    PlayerID = player_id,
    Season = season,
    SeasonType = season_type,
    PlayerPosition = "",
    ContextMeasure = "FGA",
    DateFrom = "",
    DateTo = "",
    GameID = "",
    GameSegment = "",
    LastNGames = 0,
    LeagueID = "00",
    Location = "",
    Month = 0,
    OpponentTeamID = 0,
    Outcome = "",
    Period = 0,
    Position = "",
    RookieYear = "",
    SeasonSegment = "",
    TeamID = 0,
    VsConference = "",
    VsDivision = ""
  ),
  add_headers(request_headers)
)

stop_for_status(request)

data = content(request)

raw_shots_data = data$resultSets[[1]]$rowSet
col_names = tolower(as.character(data$resultSets[[1]]$headers))

if (length(raw_shots_data) == 0) {
  shots = data.frame(
    matrix(nrow = 0, ncol = length(col_names))
  )
} else {
  shots = data.frame(
    matrix(
      unlist(raw_shots_data),
      ncol = length(col_names),
      byrow = TRUE
    )
  )
}

shots = as_tibble(shots)
names(shots) = col_names

shots = mutate(shots,
               loc_x = -as.numeric(as.character(loc_x)) / 10,
               loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
               shot_distance = as.numeric(as.character(shot_distance)),
               shot_made_numeric = as.numeric(as.character(shot_made_flag)),
               shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
               shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
               shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2),
               game_date = as.Date(game_date, format = "%Y%m%d"),
               
               # added season and season type to dataset
               season = season,
               season_type = season_type
)

raw_league_avg_data = data$resultSets[[2]]$rowSet
  league_avg_names = tolower(as.character(data$resultSets[[2]]$headers))
  league_averages = as_tibble(data.frame(
    matrix(unlist(raw_league_avg_data), ncol = length(league_avg_names), byrow = TRUE)
  ))
  names(league_averages) = league_avg_names
  league_averages = mutate(league_averages,
    fga = as.numeric(as.character(fga)),
    fgm = as.numeric(as.character(fgm)),
    fg_pct = as.numeric(as.character(fg_pct)),
    shot_value = ifelse(shot_zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2),
    season = season,
    season_type = season_type
  )
  
  if(dat_type == "player")
  {
    if(nrow(shots) > 0)
    {
      return(shots)
    }
  }
    
  
  if(dat_type=="league_averages")
  {
    if(nrow(league_averages) > 0)
    {
    return(league_averages)
    }
  }
}

# Player stats
## Get all years Lebron was in NBA-starts in 2003 an goes up to 2023

year_prefix<-seq(3,22,1)

seasons<-paste0("20",str_pad(string=year_prefix,pad="0",side="left", width =2),"-",str_pad(string=year_prefix + 1,pad="0",side="left", width =2))

## get player stats from 2003-2023
tic("get regular seasons-player")
for(season in seasons)
{
  index = which(seasons==season)
  dat = get_stats(season = season)
  if (index == 1)
  {
    write.csv(dat,"lebron-full-regularR.csv", row.names = FALSE)
  }
  else
  {
    write.table(dat, "lebron-full-regularR.csv", sep = ",", col.names = !file.exists("lebron-regular-season.csv"), append = T)
  }
}
toc()


# League stats 
## get league stats from 2003-2023
tic("get regular seasons-league")
for(season in seasons)
{
  index = which(seasons==season)
  dat = get_stats(season = season, dat_type = "leage_averages")
  if (index == 1)
  {
    write.csv(dat,"league-full-regularR.csv", row.names = FALSE)
  }
  else
  {
    write.table(dat, "league-full-regularR.csv", sep = ",", col.names = !file.exists("lebron-regular-season.csv"), append = T)
  }
}
toc()