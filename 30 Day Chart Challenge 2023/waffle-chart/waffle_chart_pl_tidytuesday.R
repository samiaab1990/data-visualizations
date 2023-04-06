library(tidyverse)
library(waffle)
library(ggimage)
library(ggtext)
library(cowplot)

# fonts
sysfonts::font_add_google("Rajdhani","Rajdhani")
sysfonts::font_add_google("Roboto","Roboto")
sysfonts::font_add_google("Rubik","Rubik")
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# get tidy tuesday data

tuesdata <- tidytuesdayR::tt_load('2023-04-04')
tuesdata <- tidytuesdayR::tt_load(2023, week = 14)

soccer <- tuesdata$soccer

# reshape data
home_away<-soccer %>%
mutate(month = lubridate::month(Date)) %>%
select(Date, HomeTeam, AwayTeam, FTHG, FTAG) %>%
pivot_longer(cols=c("HomeTeam","AwayTeam"), names_to="home_or_away", values_to="team_name") %>%
pivot_longer(cols=c("FTHG","FTAG"), names_to="goal_type", values_to="goals") %>%
filter(case_when(
  home_or_away == "HomeTeam" ~ goal_type =="FTHG",
  TRUE ~ goal_type == "FTAG"
))

# summarize goals 
goals_summary<-home_away %>% 
group_by(team_name, home_or_away) %>%
summarize(goals = sum(goals)) %>%
mutate(percent = goals/sum(goals))

# get logo and home and away colors
logo_and_color<-tibble(
  team_name = c("Arsenal", 
                "Aston Villa",
                "Brentford",
                "Brighton",
                "Burnley",
                "Chelsea",
                "Crystal Palace",
                "Everton",
                "Leeds",
                "Leicester",
                "Liverpool",
                "Man City",
                "Man United",
                "Newcastle",
                "Norwich",
                "Southampton",
                "Tottenham",
                "Watford",
                "West Ham",
                "Wolves"),
                
  team_logo = c("https://upload.wikimedia.org/wikipedia/en/5/53/Arsenal_FC.svg",
                "https://upload.wikimedia.org/wikipedia/en/f/f9/Aston_Villa_FC_crest_%282016%29.svg",
                "https://upload.wikimedia.org/wikipedia/en/2/2a/Brentford_FC_crest.svg",
                "https://upload.wikimedia.org/wikipedia/en/f/fd/Brighton_%26_Hove_Albion_logo.svg",
                "https://upload.wikimedia.org/wikipedia/en/6/62/Burnley_F.C._Logo.svg",
                "https://upload.wikimedia.org/wikipedia/en/c/cc/Chelsea_FC.svg",
                "https://upload.wikimedia.org/wikipedia/en/a/a2/Crystal_Palace_FC_logo_%282022%29.svg",
                "https://upload.wikimedia.org/wikipedia/en/7/7c/Everton_FC_logo.svg",
                "https://upload.wikimedia.org/wikipedia/en/5/54/Leeds_United_F.C._logo.svg",
                "https://upload.wikimedia.org/wikipedia/en/2/2d/Leicester_City_crest.svg",
                "https://upload.wikimedia.org/wikipedia/en/0/0c/Liverpool_FC.svg",
                "https://upload.wikimedia.org/wikipedia/en/e/eb/Manchester_City_FC_badge.svg",
                "https://upload.wikimedia.org/wikipedia/en/7/7a/Manchester_United_FC_crest.svg",
                "https://upload.wikimedia.org/wikipedia/en/5/56/Newcastle_United_Logo.svg",
                "https://upload.wikimedia.org/wikipedia/en/1/17/Norwich_City_FC_logo.svg",
                "https://upload.wikimedia.org/wikipedia/en/c/c9/FC_Southampton.svg",
                "https://upload.wikimedia.org/wikipedia/en/b/b4/Tottenham_Hotspur.svg",
                "https://upload.wikimedia.org/wikipedia/en/e/e2/Watford.svg",
                "https://upload.wikimedia.org/wikipedia/en/c/c2/West_Ham_United_FC_logo.svg",
                "https://upload.wikimedia.org/wikipedia/en/f/fc/Wolverhampton_Wanderers.svg"
                ),
  HomeTeam = c("#EF0107","#670E36","#e30613","#0057B8","#6C1D45","#034694","#C4122E","#003399","#FFFFFF","#003090","#C8102E","#6CABDD","#DA291C","#FFFFFF","#FFF200","#D71920","#FFFFFF","#FBEE23","#7A263A","#FDB913"),
  AwayTeam = c("#E9EA8D", "#000000","#DFC623","#5DDAD0","#FFFFFF","#E8DB4E","#E6CB02","#000000","#394285","#58C1C4","#E7E3D8","#FFFFFF","#546FAB","#000000", "#000000","#FAEC0F","#000000","#ed2127","#FFFFFF","#FFFFFF")
) %>%
pivot_longer(cols = c("HomeTeam","AwayTeam"), names_to="home_or_away", values_to="hex")

# join to goals_summary data
goals_summary<-left_join(goals_summary,logo_and_color)

# make waffle function
make_waffle<-function(team)
{
  
dat<- goals_summary %>% filter(team_name == team)

p<-ggplot()+
geom_waffle(data = dat,aes(fill=home_or_away, values=goals), make_proportional=TRUE, color="#340040", flip=TRUE, size=3, radius = unit(8,"pt"))+
geom_textbox(data = dat %>% filter(home_or_away == "HomeTeam"), aes(x=11, y=8,label=paste0("<b>Home</b><br>",round(percent*100,1),"%")), box.color=NA, family="Roboto", fill=NA, size=7, color="#FFFFFF", width=unit(.1,"npc"))+
geom_textbox(data = dat %>% filter(home_or_away == "AwayTeam"), aes(x=11, y=2,label=paste0("<b>Away</b><br>",round(percent*100,1),"%")), box.color=NA, family="Roboto", fill=NA, size=7, color="#FFFFFF", width=unit(.1,"npc"))+
geom_text(data = dat, aes(x=5, y=-.03, label=team_name), hjust=.5, size=8, fontface="bold", family="Rajdhani", color="#FFFFFF")+
geom_image(data = dat %>% slice(1), aes(x=1, y=-.03, image=team_logo), size=.08)+
theme_void()+
scale_fill_manual(values = c(dat %>% filter(home_or_away=="AwayTeam") %>% pull(hex), dat %>% filter(home_or_away=="HomeTeam") %>% pull(hex)))+
theme(legend.position="none")+
coord_fixed(clip="off")

return(p)
}

# create separate grids for home, away and equal proportions
home_grids<-goals_summary %>% filter(home_or_away == "HomeTeam" & percent >.5) %>% pull(team_name) %>% map(make_waffle)
away_grids<-goals_summary %>% filter(home_or_away == "AwayTeam" & percent >.5) %>% pull(team_name) %>% map(make_waffle)
equal_grids<-goals_summary %>% filter(home_or_away == "AwayTeam" & percent ==.5) %>% pull(team_name) %>% map(make_waffle)

# base chart 
main<-
ggplot()+
geom_textbox(aes(x=16.5, y=23.5, label="Home or Away: Where Most Goals were Scored in the 2021-22 Premier League Season"), family='Rubik', color="#FFFFFF", fontface="bold", width=unit(.8,"npc"), box.color=NA, fill=NA, size=25)+
geom_textbox(aes(x=16.5, y=21, label="Each chart shows the proportion of all season goals made at home or away for each team in the 2021-22 premier league season. The colors corresponding to <b>home</b> and <b>away</b> reflect the respective home and away kit colors in the season."), family="Rubik", size=15, box.color=NA, width=unit(.8,"npc"), fill=NA, color="#FFFFFF")+
geom_textbox(aes(x=16.5, y=19.5, label="<b>Source</b>: premierleague.com via Kaggle, footyheadlines.com, logo: pngkit <b><b>Viz By:</b> Samia B (<span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span> samiaab1990)</span> "), width=unit(.8,"npc"), box.color=NA, fill=NA, size=8, color="#FFFFFF", family="Rubik")+
xlim(0,28.5)+
ylim(0,24)+
theme_void()+
theme(plot.background = element_rect(fill="#37003C", color=NA),
      panel.background = element_rect(fill="#37003C", color=NA))+
coord_fixed(clip="off")

# first header
header1<-
ggplot()+
geom_textbox(aes(x=7, y=0, label="More Goals Made at Home"), color="#FFFFFF", box.color=NA, fill=NA, family="Rubik", fontface="bold", size=13, width=unit(.5,"npc"))+
geom_segment(aes(x=5, y=0, xend=28, yend=0), linetype="dotted", color="#FFFFFF")+
xlim(0,28.5)+
ylim(0,5)+
theme_void()+
coord_fixed(clip="off")

# second header
header2<-
  ggplot()+
  geom_textbox(aes(x=7, y=0, label="More Goals Made Away"), color="#FFFFFF", box.color=NA, fill=NA, family="Rubik", fontface="bold", size=13, width=unit(.5,"npc"))+
  geom_segment(aes(x=5, y=0, xend=22, yend=0), linetype="dotted", color="#FFFFFF")+
  xlim(0,28.5)+
  ylim(0,5)+
  theme_void()+
  coord_fixed(clip="off")

# third header
header3<-
  ggplot()+
  geom_textbox(aes(x=7, y=0, label="Equal"), color="#FFFFFF", box.color=NA, fill=NA, family="Rubik", fontface="bold", size=13, width=unit(.5,"npc"))+
  geom_segment(aes(x=5, y=0, xend=12, yend=0), linetype="dotted", color="#FFFFFF")+
  xlim(0,15)+
  ylim(0,5)+
  theme_void()+
  coord_fixed(clip="off")

# check lengths to see how to organize nrow 
length(home_grids)

# grids 
home_grids_all<-plot_grid(plotlist=home_grids, nrow=3)

away_grids_all<-plot_grid(plotlist=away_grids, nrow=1)

equal_grids_all<-plot_grid(plotlist=equal_grids, nrow=1)

# arrange with cowplot
pl_plot<-ggdraw()+
draw_plot(main)+
draw_image("~/GitHub/Data-Visualizations/30 Day Chart Challenge 2023/waffle-chart/pl_logo.png", width=.2, height=.2, x=-.015, y=.78)+
draw_plot(header1, y=.32)+
draw_plot(home_grids_all, height=.5, y=.22)+
draw_plot(header2, y=-.22)+
draw_plot(away_grids_all, x=-.085, height=.2, scale=.83, width=.97, y=-.01)+
draw_plot(header3, x=.75, width=.3, y=-.26)+
draw_plot(equal_grids_all, height=.22, x=.8, y=-.02, width=.2, scale=.75)

# save
ggsave(plot = pl_plot, filename="pl_waffle.png",width=(5*5)+3.5, height=(5*4)+5, units='in', dpi=300, bg="#37003C")