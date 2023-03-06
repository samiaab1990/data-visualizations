from nba_api.stats.endpoints import shotchartdetail
import pandas as pd

def get_stats(player, season, season_type, dataset):
    api_call = shotchartdetail.ShotChartDetail(
    team_id = 0, 
    player_id = player, 
    context_measure_simple = 'FGA', 
    season_type_all_star = [season_type],
    season_nullable = season)

    if dataset == "player":
        dat = api_call.get_data_frames()[0]
        dat['Season'] = season
        dat['Season Type'] = season_type

        return dat

    if dataset == "league":
        dat = api_call.get_data_frames()[1]
        dat['Season'] = season
        dat['Season Type'] = season_type
        return dat

seasons = []
for i in range(3,23,1):
    seasons.append("20" + str(i).rjust(2,"0") + "-" + str(i + 1).rjust(2,"0"))

# get player regular season stats
for season in seasons:
    dat = get_stats(player = 2544, season = season, season_type = "Regular Season", dataset = "player")
    
    if seasons.index(season) == 0:
        dat.to_csv("lebron-full-regular.csv", index = False)
    else:
        dat.to_csv("lebron-full-regular.csv", mode = "a", index = False, header = False)

# get league regular season stats
for season in seasons:
    dat = get_stats(player= 2544, season = season, season_type = "Regular Season", dataset = "league")
    
    if seasons.index(season) == 0:
       
        dat.to_csv("league-full-regular.csv", index = False)
    else:
        dat.to_csv("league-full-regular.csv", mode = "a", index = False, header = False)

