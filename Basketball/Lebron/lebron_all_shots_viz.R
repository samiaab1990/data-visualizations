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
  geom_textbox(aes(x=1.5,y=3.5,label="<span style='font-family:\"Open Sans\"'><b>Source:</b> stat.nba.com, ballR</span>"), size=4, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"),alpha=.5)+
  theme_void()

# heading 
heading<-ggplot()+
  geom_textbox(aes(x=2.5,y=2.5,label="<span style='font-family:\"Playfair Display\";font-size:30px'><i>All Attempted Shots</span><br><span style='font-family:\"Open Sans\";font-size:14px'><i>From 2003 to 02-07-2023</span>"), box.color=NA, fill=NA, color="#303030", width=unit(.8,"npc"),alpha=.5, fontface="bold", lineheight=.3)+
  xlim(0,5)+
  ylim(0,5)+
  theme_void()

lebron_regular<-lebron_regular %>%
  mutate(colors = case_when(
    SHOT_MADE_FLAG == 0 ~ "#009590",
    TRUE ~ "#FDA100"
  ))

# shot chart 2
plot<-ggcourt("#303030","#F5E49F")+
  geom_point(data = lebron_regular, aes(x=LOC_X, y=LOC_Y, color=colors), size=.7)+
  geom_star(data = lebron_regular %>% filter(GAME_DATE=="2023-02-07" & GAME_EVENT_ID==501),
             aes(x=LOC_X, y=LOC_Y), fill="yellow", size=7, alpha=.5)
  #geom_image(data = $hex_data, aes(x=0,y=2,image=team_logo))+
  #geom_textbox(data = dataset$hex_data, aes(x=0, y=-2.5, label=paste0("<span style='font-size:45px;font-family:\"Playfair Display\";color:",color1,"'><b>",TEAM_NAME,"</b></span><br><span style='font-size:21px;font-family:\"Open Sans\"; color:",color1,"'><b>SEASON: ", season_lab,"</b></span>")), fill=NA, box.color=NA, color=color1, hjust=.5, halign=.5, width=unit(1,"npc"))+

# legend
legend<-tibble(
  x = c(1,2,3),
  y = rep(-.5,3),
  labs = c("Made", "Missed", "Record-Breaking Shot")
)

legend_plot<-ggplot()+
geom_point(data = legend %>% filter(x %in% c(1,2)), aes(x=x,y=y, color=as.factor(x)), size=4)+
geom_star(data = legend %>% filter(x == 3), aes(x=x,y=y), fill="yellow", size=4)+
geom_textbox(data = legend, aes(x=x,y=y-.1,label=labs), family="Open Sans", color="#303030", fontface="bold", size=4, box.color=NA, fill=NA, hjust=.5, halign=.5)+
scale_color_manual(values = c("#FDA100","#009590"))+
theme_void()+
ylim(-1,0)+
xlim(1,3.5)+
theme(legend.position="none")


full_plot<-ggdraw()+
  draw_plot(main_plot)+
  draw_image(lebron_pic, scale = .23, x=-.43, y=.4)+
  draw_plot(title, x=.18, y=.42)+
  draw_plot(subtitle, x=.18,y=.35)+
  draw_plot(plot)+
  draw_plot(legend_plot, width=.4, height=.2,y=-.05, x=.335)+
  draw_plot(heading,height=.5, x=-.03,y=-.21)+
  draw_plot(caption,x=.83,y=-.49)

save_dir<-"~/GitHub/Data-Visualizations/Basketball/Lebron/plots/"

ggsave(plot = full_plot, filename=paste0(save_dir,"lebron_shot_chart.png"), width=13, height=13, units='in', dpi=300, bg="#FFFCEF")

