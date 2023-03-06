library(ggimage)
library(rsvg)
library(ggtext)
library(ggstar)
library(jcolors)
library(grid)
library(waffle)
library(cowplot)
library(png)
library(ggnewscale)
library(geomtextpath)

dir<-"~/GitHub/Data-Visualizations/Basketball/"

source(paste0(dir,"ggcourt.R"))
source(paste0(dir,"calc_hexbins.R"))
source(paste0(dir,"Lebron/data_cleaning.R"))

# get fonts
sysfonts::font_add_google("Playfair Display", "Playfair Display", bold.wt=800)
sysfonts::font_add_google("Open Sans", "Open Sans", bold.wt=800)
#sysfonts::font_add_google("Oswald", "Oswald", bold.wt=700)
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# main plot bg 
main_plot<-ggplot()+
  theme_void()+
  xlim(0,14)+
  ylim(0,14)+
  theme(plot.background = element_rect(color="#FFFCEF"))


# lebron icon
lebron_pic<-paste0(dir,"/Lebron/images/lebron_circle_halftone.webp")

# title 
title<-ggplot()+
  geom_textbox(aes(x=0,y=0,label="Lebron James:<i>A Making<br>Of NBA History</i>"), size=17, family="Playfair Display", fontface="bold", color="#303030", box.color=NA, fill=NA, lineheight=.8, width=unit(1,"npc"))+
  theme_void()+
  theme(plot.background = element_rect(fill="transparent", color="transparent"))

# subtitle
subtitle<-ggplot()+
  geom_textbox(aes(x=0,y=0,label="<span style='font-family:\"Open Sans\"'>A visual summary of an NBA all-time scoring record.</span><br><span style='font-family:\"Playfair Display\"'><i><b>By:</b></i></span><span style='font-family:\"Open Sans\"'> Samia B</span>"), size=6, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"))+
  theme_void()


# heading one 
heading_one<-ggplot()+
  geom_textbox(aes(x=1.5,y=3.5,label="<span style='font-family:\"Playfair Display\"'><i>True Shooting Percentage</span>"), size=10, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"),alpha=.5, fontface="bold")+
  theme_void()

# explanation
explanation<-ggplot()+
  geom_textbox(aes(x=1.5,y=3.5,label="<span style='font-family:\"Open Sans\"'>The <b> true shooting percentage</b> measures efficiency, accounting for point differences between field goals and free throws, determined by the following formula: <b>Total Points / 2 x (Field Goal Attempts + (.44 x Free Throw Attempts))</b></span>"), size=5, box.color=NA, fill=NA, color="#303030", width=unit(.7,"npc"),alpha=.5)+
  theme_void()

# caption
caption<-ggplot()+
  geom_textbox(aes(x=1.5,y=3.6,label="<span style='font-family:\"Open Sans\"'><b>Source:</b> stat.nba.com</span>"), size=4, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"),alpha=.5)+
  theme_void()

lebron_profile_dat<-lebron_profile %>%
mutate(TSP = PTS/(2*(FGA + (.44* FTA))))

lebron_metrics<-lebron_profile_dat %>%
left_join(lebron_regular %>% select(Season, TEAM_NAME) %>% distinct(.keep_all=TRUE), by =c("SEASON_ID" = "Season")) %>%
  mutate(TEAM_NAME = case_when(
    SEASON_ID %in% c("2014-15","2015-16","2016-17","2017-18") ~ paste0(TEAM_NAME,"-2"),
    TRUE ~ TEAM_NAME
  ),
  colors = case_when(
    str_detect(TEAM_NAME,"Cleveland") ~ "#6F212F",
    str_detect(TEAM_NAME,"Miami") ~ "#000000",
    str_detect(TEAM_NAME,"Lakers") ~ "#552583")) %>% 
  select(SEASON_ID, TSP, FT_PCT, FG_PCT, TEAM_NAME, colors) 
 
tsp_plot<-ggplot(data = lebron_metrics, aes(x = SEASON_ID, y=TSP, group=1))+
geom_line(aes(color=colors), linewidth=3)+
geom_point(aes(color=colors), fill = "#FFFCEF", shape = 21, size=5, stroke=3)+
geom_line(aes(y=FT_PCT), linewidth=3, color="#e4e4e4")+
geom_point(aes(y=FT_PCT), fill = "#FFFCEF",color="#e4e4e4",shape = 21, size=5, stroke=3,)+
geom_line(aes(y=FG_PCT), linewidth=3, color="#e4e4e4")+
geom_text(data = lebron_metrics %>% filter(SEASON_ID == "2013-14"), aes(x=SEASON_ID, y=TSP+.03, label="Best Season"), color="#C6C6C6", fontface="bold", family="Open Sans")+
geom_point(aes(y=FG_PCT), fill = "#FFFCEF",color="#e4e4e4",shape = 21, size=5, stroke=3,)+
geom_segment(data = lebron_metrics %>% filter(SEASON_ID == "2003-04"), aes(x=SEASON_ID, xend=SEASON_ID, y=.5, yend=.61), linetype="dotted", color="#6F212F")+
geom_textbox(data = lebron_metrics %>% filter(SEASON_ID == "2003-04"), aes(x=3.5, y = TSP + .15, label = "Cleveland<br>Cavaliers<br><b>2003-10</b>"), box.color=NA, fill=NA, hjust=.5, halign=.5, size=4, family="Open Sans", color="#6F212F")+
geom_segment(data = lebron_metrics, aes(x=1, xend=6.89, y=.61, yend=.61), linetype="dotted", color="#6F212F")+
geom_segment(data = lebron_metrics %>% filter(SEASON_ID == "2010-11"), aes(x=SEASON_ID, xend=SEASON_ID, y=.61, yend=.66), linetype="dotted", color="#000000")+
geom_textbox(data = lebron_metrics %>% filter(SEASON_ID == "2010-11"), aes(x=9.5, y = .689, label = "Miami<br>Heat<br><b>2010-14</b>"), box.color=NA, fill=NA, hjust=.5, halign=.5, size=4, family="Open Sans", color="#000000")+
geom_segment(data = lebron_metrics %>% filter(SEASON_ID == "2010-11"), aes(x=8, xend=11, y=.66, yend=.66), linetype="dotted", color="#000000")+
geom_segment(data = lebron_metrics %>% filter(SEASON_ID == "2014-15"), aes(x=SEASON_ID, xend=SEASON_ID, y=.6, yend=.63), linetype="dotted", color="#6F212F")+
geom_textbox(data = lebron_metrics %>% filter(SEASON_ID == "2014-15"), aes(x=13, y = .66, label = "Cleveland<br>Cavaliers<br><b>2014-18</b>"), box.color=NA, fill=NA, hjust=.5, halign=.5, size=4, family="Open Sans", color="#6F212F")+
geom_segment(data = lebron_metrics %>% filter(SEASON_ID == "2014-15"), aes(x=12, xend=15, y=.63, yend=.63), linetype="dotted", color="#6F212F")+
geom_segment(data = lebron_metrics %>% filter(SEASON_ID == "2018-19"), aes(x=SEASON_ID, xend=SEASON_ID, y=.6, yend=.63), linetype="dotted", color="#552583")+
geom_textbox(data = lebron_metrics %>% filter(SEASON_ID == "2018-19"), aes(x=19, y = .66, label = "Los Angeles<br>Lakers<br><b>2018-Current</b>"), box.color=NA, fill=NA, hjust=.5, halign=.5, size=4, family="Open Sans", color="#552583")+
geom_segment(data = lebron_metrics %>% filter(SEASON_ID == "2018-19"), aes(x=16, xend=20, y=.63, yend=.63), linetype="dotted", color="#552583")+
geom_segment(data = lebron_metrics %>% filter(SEASON_ID == "2018-19"), aes(x=20, xend=20, y=.63, yend=.6), linetype="dotted", color="#552583")+
geom_textbox(data = lebron_metrics %>% filter(SEASON_ID == "2022-23"), aes(x=20.8, y = FT_PCT, label = "<b>FT PCT</b>"), box.color=NA, fill=NA, hjust=.5, halign=.5, size=4, family="Open Sans", color="#e4e4e4")+
geom_textbox(data = lebron_metrics %>% filter(SEASON_ID == "2022-23"), aes(x=20.8, y = FG_PCT, label = "<b>FG PCT</b>"), box.color=NA, fill=NA, hjust=.5, halign=.5, size=4, family="Open Sans", color="#e4e4e4")+
scale_color_identity()+
theme_minimal()+
xlab("")+
ylab("")+
theme(axis.text.x = element_blank(),
      axis.text.y = element_text(family="Open Sans", color="#303030", size=8, face="bold"))+
coord_cartesian(clip="off")


league_fg_pct<-league_regular %>% 
group_by(GRID_TYPE,Season) %>% 
summarise(FG_PCT = sum(FGM)/sum(FGA))

lebron_diff_dat<-lebron_profile %>%
left_join(league_fg_pct, by=c("SEASON_ID" = "Season")) %>%
left_join(lebron_regular %>% select(Season, TEAM_NAME) %>% distinct(.keep_all=TRUE), by =c("SEASON_ID" = "Season")) %>% 
mutate(fg_diff = FG_PCT.x - FG_PCT.y,
       colors = case_when(
         str_detect(TEAM_NAME,"Cleveland") ~ "#6F212F",
         str_detect(TEAM_NAME,"Miami") ~ "#000000",
         str_detect(TEAM_NAME,"Lakers") ~ "#552583"),
       direction = case_when(
         fg_diff < 0 ~ "#ef233c",
         TRUE~ "#06d6a0"
       )) %>%
select(SEASON_ID, fg_diff, colors, TEAM_NAME, direction)

fg_diff_plot<-ggplot(data = lebron_diff_dat, aes(x=SEASON_ID, xend=SEASON_ID, y=0, yend=fg_diff))+
geom_segment(aes(color=direction), linewidth=3)+
geom_point(aes(x=SEASON_ID, y=fg_diff, color=direction), size=6)+
scale_color_identity()+
scale_fill_identity()+
geom_text(aes(x=SEASON_ID, y=ifelse(fg_diff>0,-.02,.02), label=SEASON_ID, color=direction), family="Open Sans", size=5, angle=90, fontface="bold")+
theme_minimal()+
theme(axis.text.x = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(family="Open Sans", color="#303030", size=8, face="bold"))+
  coord_cartesian(clip="off")

# heading two
heading_two<-ggplot()+
  geom_textbox(aes(x=1.5,y=3.5,label="<span style='font-family:\"Playfair Display\"'><i>Field Goal Percent vs League Average, By Season</span>"), size=10, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"),alpha=.5, fontface="bold")+
  theme_void()

# explanation two
explanation_two<-ggplot()+
  geom_textbox(aes(x=1.5,y=3.5,label="<span style='font-family:\"Open Sans\"'>The difference between individual field goal percent versus the league average for each season.</span>"), size=5, box.color=NA, fill=NA, color="#303030", width=unit(.7,"npc"),alpha=.5)+
  theme_void()


full_plot<-ggdraw()+
  draw_plot(main_plot)+
  draw_image(lebron_pic, scale = .23, x=-.43, y=.4)+
  draw_plot(title, x=.18, y=.42)+
  draw_plot(subtitle, x=.18,y=.35)+
  draw_plot(heading_one,x=.05,y=.28)+
  draw_plot(explanation,x=-.1,y=.23)+
  draw_plot(tsp_plot, height=.3, width=.95,y=.38)+
  draw_plot(heading_two, x=.05, y=-.13)+
  draw_plot(explanation_two, x=-.1, y=-.165)+
  draw_plot(fg_diff_plot, height=.3, width=.93,y=.02,x=.01)+
  draw_plot(caption,x=.83,y=-.495)


save_dir<-"~/GitHub/Data-Visualizations/Basketball/Lebron/plots/"

ggsave(plot = full_plot, filename=paste0(save_dir,"lebron_fg_stats_summary.png"), width=13, height=13, units='in', dpi=300, bg="#FFFCEF")
