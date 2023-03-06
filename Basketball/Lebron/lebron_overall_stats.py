from nba_api.stats.endpoints import playerprofilev2
import pandas as pd

api_call = playerprofilev2.PlayerProfileV2(
player_id = 2544,
per_mode36="Totals")

dat = api_call.get_data_frames()[0]

dat.to_csv("lebron-profile.csv", index = False)


    
