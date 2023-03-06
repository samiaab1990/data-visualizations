# get lebron CSV files 
dir<-"~/GitHub/Data-Visualizations/Basketball/"

lebron_regular<-read.csv(paste0(dir,"Lebron/CSV/lebron-full-regular.csv"))
league_regular<-read.csv(paste0(dir, "Lebron/CSV/league-full-regular.csv"))
lebron_profile<-read.csv(paste0(dir, "Lebron/CSV/lebron-profile.csv"))


# data cleanup
lebron_regular<-lebron_regular %>%
  mutate(shot_value = as.numeric(str_extract(SHOT_TYPE,"[:digit:]")),
         # scale down by 10, convert to negative to flip to court direction
         LOC_X = -LOC_X/10,
         #scale down to 10, add 5.25 for the hoop distance 
         LOC_Y =  (LOC_Y/10) +  5.25,
         GAME_DATE = as.Date(paste0(str_sub(GAME_DATE,1,4),"-",str_sub(GAME_DATE,5,6),"-",str_sub(GAME_DATE,7,8))))

two_pt_fg<-lebron_regular %>%
  filter(shot_value == 2, SHOT_MADE_FLAG == 1) %>%
  nrow()

three_pt_fg<-lebron_regular %>%
  filter(shot_value == 3, SHOT_MADE_FLAG == 1) %>%
  nrow()

lebron_overall<-tibble(
  totalpt = 38390,
  freethrowsmade = 8047,
  fieldgoal2made = two_pt_fg * 2,
  fieldgoal3made = three_pt_fg * 3)
