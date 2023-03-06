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

# caption
caption<-ggplot()+
  geom_textbox(aes(x=1.5,y=3.5,label="<span style='font-family:\"Open Sans\"'><b>Source:</b> stats.nba.com</span>"), size=4, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"),alpha=.5)+
  theme_void()

# heading one 
heading_one<-ggplot()+
  geom_textbox(aes(x=1.5,y=3.5,label="<span style='font-family:\"Playfair Display\"'><i>Where the Most Points Were Scored</span>"), size=10, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"),alpha=.5, fontface="bold")+
  theme_void()

# heading two
heading_two<-ggplot()+
  geom_textbox(aes(x=1.5,y=3.5,label="<span style='font-family:\"Playfair Display\"'><i>When the Most Points Were Scored</span>"), size=10, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"),alpha=.5, fontface="bold")+
  theme_void()

## summary stats
summary_stats<-ggplot()+
  geom_textbox(data = lebron_overall, 
               aes(x=0,y=0,
                   label=paste0("<span style='font-family:\"Playfair Display\";font-size:65px;'><b>",scales::comma(totalpt),"<span style='font-size:30px;font-family:\"Open Sans\"'> <b>TOTAL POINTS</b><br></b><span style='font-family:\"Playfair Display\";font-size:50px;'><b>",scales::comma(fieldgoal2made),"</b></span><span style='font-size:20px;font-family:\"Open Sans\"'><b> POINTS 2 PT FG</b></span></span><span style='font-family:\"Playfair Display\";font-size:50px;'><b> ",scales::comma(freethrowsmade),"</b></span><span style='font-size:20px;font-family:\"Open Sans\"'><b> POINTS FTM</b></span><span style='font-family:\"Playfair Display\";font-size:50px;'><b> ",scales::comma(fieldgoal3made),"</b></span><span style='font-size:20px;font-family:\"Open Sans\"'><b> POINTS 3 PT FG</b> </span>")), box.color=NA, fill=NA, width=unit(.9,"npc"), halign=.5, hjust=.5, color="#303030")+
  theme_void()


## waffle chart
lebron_profile_dat<-lebron_profile %>%
  left_join(lebron_regular %>% select(Season, TEAM_NAME) %>% distinct(.keep_all=TRUE), by =c("SEASON_ID" = "Season")) %>%
  mutate(TEAM_NAME = case_when(
    SEASON_ID %in% c("2014-15","2015-16","2016-17","2017-18") ~ paste0(TEAM_NAME,"-2"),
    TRUE ~ TEAM_NAME
  ))

points_summary<-lebron_profile_dat %>%
  group_by(TEAM_NAME) %>%
  summarise(TOT_PTS = sum(PTS)) %>%
  ungroup() %>%
  mutate(TOT_PCT = TOT_PTS/sum(TOT_PTS),
         TEAM_NAME = factor(TEAM_NAME,levels=c("Cleveland Cavaliers","Miami Heat","Cleveland Cavaliers-2","Los Angeles Lakers"))) %>%
  arrange(TEAM_NAME)

team_colors<-c("#6F212F","#000000", "#6F212F", "#552583")

labs<- points_summary %>%
  select(TEAM_NAME, TOT_PTS) %>%
  mutate(
    seasons = case_when(
      TEAM_NAME == "Cleveland Cavaliers" ~ "2003-2010",
      TEAM_NAME == "Miami Heat" ~ "2010-2014",
      TEAM_NAME == "Cleveland Cavaliers-2" ~ "2014-2018",
      TEAM_NAME == "Los Angeles Lakers" ~ "2018-Current"
    ),
    colors = case_when(
      str_detect(TEAM_NAME,"Cleveland") ~ team_colors[1],
      str_detect(TEAM_NAME,"Miami") ~ team_colors[2],
      str_detect(TEAM_NAME,"Lakers") ~ team_colors[4]
    ),
    labs = paste0("<b><span style='font-family:\"Playfair Display\";font-size:30px;color:",colors,"'>",str_trim(str_remove_all(TEAM_NAME,"Cleveland|Miami|Los Angeles|-2")),"</b><br><span style='font-family:\"Open Sans\"; font-size:15px; color:#303030'><b>", seasons,"</b></span><br><span style='font-family:\"Playfair Display\";font-size:25px; color:#303030'><b>",scales::comma(TOT_PTS),"</b></span><span style='font-family:\"Open Sans\"; font-size:15px;color:#303030'><b>PTS</b></span>"),
    x = c(5,11.5,16,19.5),
    y = rep(-.2,4)
  )
waffle<-ggplot()+
  geom_waffle(data = points_summary,aes(fill=TEAM_NAME, values=TOT_PTS),
              n_rows = 5, size = 0.33, colour = "#FFFCEF", make_proportional=TRUE,radius = unit(4, "pt"))+
  coord_equal(clip="off")+
  geom_textbox(data = labs, aes(x=x-.5,y=y-.1,label=labs), fill=NA, box.color=NA, width=unit(.5,"npc"), halign=.5)+
  theme_void()+
  scale_fill_manual(values = team_colors)+
  theme(legend.position = "none")

#ggsave(plot = waffle, filename=paste0(save_dir,"waffle.png"), width=10, height=5, units='in', dpi=300, bg="transparent")

## area plot 

lebron_season_sum<-lebron_profile_dat %>%
  mutate(FG2M = FGM - FG3M) %>%
  select(TEAM_NAME, SEASON_ID, FG2M, FTM, FG3M,PTS) %>%
  mutate(
    FG2PTs = FG2M * 2,
    FG3PTs = FG3M * 3,
    FTPTs = FTM
  ) %>%
  select(TEAM_NAME, SEASON_ID, FG2PTs, FG3PTs, FTPTs, PTS) %>% 
  pivot_longer(c("FG2PTs","FTPTs","FG3PTs"), names_to="shot_type", values_to="shot_points") %>%
  mutate(shot_type = factor(shot_type, levels=c("FG2PTs","FTPTs","FG3PTs")),
         colors = case_when(
           str_detect(TEAM_NAME,"Cleveland") ~ "#6F212F",
           str_detect(TEAM_NAME,"Miami") ~ "#000000",
           str_detect(TEAM_NAME,"Lakers") ~ "#552583"
         ),
         team_abbr = case_when(
           str_detect(TEAM_NAME,"Cleveland") ~ "CLE",
           str_detect(TEAM_NAME, "Miami") ~ "MIA",
           str_detect(TEAM_NAME, "Lakers") ~ "LAL"
         ),
         
         shot_lab = case_when(
           shot_type == "FG2PTs" ~ "Points From 2 PT Field Goal",
           shot_type == "FG3PTs" ~ "Points From 3 PT Field Goal",
           shot_type == "FTPTs" ~  "Points From Free Throw"))


area_plot<-ggplot(data = lebron_season_sum, aes(x=SEASON_ID, y=shot_points, group=shot_type))+
  geom_segment(aes(x=SEASON_ID, xend=SEASON_ID, y=0, yend=max(shot_points)+10, color=colors), linewidth=.7, linetype="dotted", alpha=.3)+
  geom_textbox(aes(y=max(shot_points)+150,label=paste0("<span style='font-family:\"Playfair Display\";font-size:15px'><b>", team_abbr,"</b></span><br><span style='font-name:\"Open Sans\"; font-size:10px'><b>",SEASON_ID,"<br>",scales::comma(PTS)," PTS</b></span>"), color=colors), box.color=NA, fill=NA, hjust=.5, halign=.5, lineheight=.8, alpha=.2)+
  geom_textbox(data = lebron_season_sum %>% filter(shot_points == max(shot_points)), aes(y=max(shot_points)+150,label=paste0("<span style='font-family:\"Playfair Display\";font-size:15px'><b>", team_abbr,"</b></span><br><span style='font-name:\"Open Sans\"; font-size:10px'><b>",SEASON_ID,"<br>",scales::comma(PTS)," PTS</b></span>"), color=colors), box.color=NA, fill=NA, hjust=.5, halign=.5, lineheight=.8)+
  geom_area(aes(fill=shot_type),linewidth=1.5,alpha=.1, position="identity")+
  scale_fill_manual(values=c("#e63946","#ee9b00","#00afb9"))+
  theme_minimal()+
  scale_color_identity()+
  new_scale_color()+
  geom_textline(aes(label=shot_lab, color=shot_type), family="Open Sans", fontface=2, linewidth=1.5, hjust=.2,text_smoothing = 50, size=5)+
  scale_color_manual(values=c("#e63946","#ee9b00","#00afb9"))+
  theme(legend.position="none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(color="#A2A2A2", face="bold"))+
  coord_cartesian(clip="off")

# annotation1
annotation_one<-ggplot()+
  geom_curve(aes(x=2.5, y=2, xend=1, yend=1), arrow=arrow(length=unit(.1,"npc"), type="closed"), color="#303030", alpha=.5)+
  geom_text(aes(x=3,y=1.6,label="Best season"), family="Open Sans", fontface="bold", size=3.5, color="#303030", alpha=.5)+
  xlim(0,4)+
  ylim(0,4)+
  theme_void()

# annotation2
annotation_two<-ggplot()+
  geom_curve(aes(x=2.5, y=2, xend=5, yend=.5), curvature=-.5,arrow=arrow(length=unit(.03,"npc"), type="closed"), color="#303030", alpha=.5)+
  geom_textbox(aes(x=2.5,y=1.5,label="More points from 3PT FGs than FTs"), family="Open Sans", fontface="bold", size=3.5, color="#303030", alpha=.5, box.color=NA, fill=NA, width=unit(.5,"npc"))+
  xlim(0,5)+
  ylim(0,5)+
  theme_void()

full_plot<-ggdraw()+
  draw_plot(main_plot)+
  draw_image(lebron_pic, scale = .23, x=-.43, y=.4)+
  draw_plot(title, x=.18, y=.42)+
  draw_plot(subtitle, x=.18,y=.35)+
  draw_plot(summary_stats, y=.27)+
  draw_plot(heading_one,x=.04, y=.17)+
  draw_plot(waffle,y=.02)+
  draw_plot(heading_two, x=.05,y=-.2)+
  draw_plot(area_plot, height=.25)+
  draw_plot(annotation_one, height=.05, width=.2, x=.1, y=.24)+
  draw_plot(annotation_two, height=.2, width=.2, x=.49,y=.04)+
  draw_plot(caption,x=.83,y=-.49)

save_dir<-"~/GitHub/Data-Visualizations/Basketball/Lebron/plots/"

ggsave(plot = full_plot, filename=paste0(save_dir,"lebron_points_summary.png"), width=13, height=13, units='in', dpi=300, bg="#FFFCEF")
