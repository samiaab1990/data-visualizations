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

year_prefix<-seq(3,22,1)
seasons<-paste0("20",str_pad(string=year_prefix,pad="0",side="left", width =2),"-",str_pad(string=year_prefix + 1,pad="0",side="left", width =2))

# get the career hex_attempts so that it's standardized across all charts
all_seasons<-seasons %>%
  map(function(x) calculate_hexbins_from_shots(lebron_regular, league_regular, season = x)[1]) %>%
  bind_rows() 

hex_attempts_levels<-all_seasons$hex_data %>% 
  mutate(hex_attempts_level = ntile(all_seasons$hex_data$hex_attempts,5)) %>%
  select(hexbin_id, season, hex_attempts_level)


# find the default ggplot2 breaks for continuous scale
# creating a manual continuous hexbin scale and manually labeling using breaks below 
# size_breaks<-with(test$hex_data, labeling::extended(range(hex_attempts_level, na.rm=TRUE)[1], range(hex_attempts_level, na.rm=TRUE)[2], m = 5))

# colors from jcolors package
pal<-jcolors_contin("pal2", reverse=TRUE)
vals<-pal(5)

pal2<-jcolors_contin("pal3",bias=2.25)
vals2<-pal2(5)

pal_vals<-c(vals[1:2],rev(vals2[1:4]))

# generate plots function for each season
generate_plot<-function(color1, color2, season){
  
  dataset<-calculate_hexbins_from_shots(lebron_regular, league_regular, season=season)
  
  dataset$hex_data<-dataset$hex_data %>% left_join(hex_attempts_levels)
  
  season_lab = paste0(str_sub(season[1],1,4),"-",str_sub(season[length(season)],6,7))
  
 
    plot<-ggcourt(color1,color2)+
    geom_image(data = dataset$hex_data, aes(x=0,y=2,image=team_logo))+
    geom_star(data = dataset$hex_data, aes(x=center_x,y=center_y,group=hexbin_id, size=hex_attempts_level, fill=bounded_fg_diff), starshape=6, color=NA)+
    geom_textbox(data = dataset$hex_data, aes(x=0, y=-2.5, label=paste0("<span style='font-size:45px;font-family:\"Playfair Display\";color:",color1,"'><b>",TEAM_NAME,"</b></span><br><span style='font-size:21px;font-family:\"Open Sans\"; color:",color1,"'><b>SEASON: ", season_lab,"</b></span>")), fill=NA, box.color=NA, color=color1, hjust=.5, halign=.5, width=unit(1,"npc"))+
    scale_size(range=c(1,5))+
    scale_fill_stepsn(colors=pal_vals, limits=c(-.12,.12))

  return(plot)
}

# create tibble of all colors and seasons
shotchart_grids<-tibble(
  color1 = c(rep("#6F212F",7), rep("#000000",4), rep("#6F212F",4), rep("#552583",5)),
  color2 = c(rep("#FFB915",7), rep("#98002E", 4), rep("#FFB915",4), rep("#FDB927",5)),
  season = seasons
)

# generate plots
lebron_plots<-shotchart_grids %>%
  pmap(generate_plot)

save_dir<-"~/GitHub/Data-Visualizations/Basketball/Lebron/plots/"

# # save individual plots
# for(i in 1:length(lebron_plots))
# {
#   ggsave(plot = lebron_plots[[i]], filename= paste0(save_dir,"lebron",i,".png"), width=10, height=10, units='in', dpi=300, bg="transparent")
#   
# }

## title 
title<-ggplot()+
  geom_textbox(aes(x=0,y=0,label="Lebron James:<i>A Making<br>Of NBA History</i>"), size=17, family="Playfair Display", fontface="bold", color="#303030", box.color=NA, fill=NA, lineheight=.8, width=unit(1,"npc"))+
  theme_void()+
  theme(plot.background = element_rect(fill="transparent", color="transparent"))

#ggsave(plot = title, filename=paste0(save_dir,"title.png"), width=20, height=5, units='in', dpi=300, bg="transparent")

## subtitle
subtitle<-ggplot()+
  geom_textbox(aes(x=0,y=0,label="<span style='font-family:\"Open Sans\"'>A visual summary of an NBA all-time scoring record.</span><br><span style='font-family:\"Playfair Display\"'><i><b>By:</b></i></span><span style='font-family:\"Open Sans\"'> Samia B</span>"), size=6, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"))+
  theme_void()

#ggsave(plot = subtitle, filename=paste0(save_dir,"subtitle.png"), width=20, height=5, units='in', dpi=300, bg="transparent")

## caption
caption<-ggplot()+
  geom_textbox(aes(x=1.5,y=3.5,label="<span style='font-family:\"Open Sans\"'><b>Source:</b> stats.nba.com, ballR</span>"), size=4, box.color=NA, fill=NA, color="#303030", width=unit(1,"npc"),alpha=.5)+
  theme_void()
#ggsave(plot = caption, filename=paste0(save_dir,"caption.png"), width=7, height=3, units='in', dpi=300, bg="transparent")

## plot legends

### size legend
legend<-tibble(
  x = c(1,2,3,4,5),
  y = rep(-.5,5)
)


hex_size_legend<-ggplot()+
geom_star(data = legend, aes(x=x, y=y, size=x), starshape=6, color="#303030")+
geom_segment(aes(x=1,y=-.59,xend=5.2,yend=-.59), arrow = arrow(length = unit(0.05, "npc")), color="#303030", linewidth=.5)+
scale_size(range=c(1,5))+
geom_text(aes(x=3,y=-.65,label = "SHOT FREQUENCY"), size=4, family="Open Sans", color="#303030", fontface="bold")+
ylim(-1,0)+
xlim(1,5.5)+
theme_void()+
theme(legend.position="none")

#ggsave(plot = hex_size_legend, filename=paste0(save_dir,"hex_size_legend.png"), width=1.9, height=2.5, units='in', dpi=300, bg="transparent")

### fill legend
legend2<-tibble(
  x = c(1,2,3,4,5,6),
  y = rep(-.5,6),
  lab = c("-.10","-.5","0",".5",".10"," ")
)

hex_fill_legend<-ggplot()+
  geom_star(data = legend2, aes(x=as.factor(x), y=y, fill=as.factor(x)), starshape=6, size=5, color=NA)+
  geom_segment(aes(x=3.6,y=-.58,xend=6,yend=-.58), arrow = arrow(length = unit(0.05, "npc")), color="#303030")+
  geom_segment(aes(x=3.4,y=-.58,xend=1,yend=-.58), arrow = arrow(length = unit(0.05, "npc")), color="#303030")+
  scale_fill_manual(values = pal_vals)+
  geom_text(data = legend2, aes(x=x+.5,y=-.5,label = lab), size=4, family="Open Sans", color="#303030", fontface="bold")+
  geom_text(aes(x=4.8, y=-.64, label = "HIGHER"), size=4, family="Open Sans", color="#303030", fontface = "bold")+
  geom_text(aes(x=2.3, y=-.64, label = "LOWER"), size=4, family="Open Sans", color="#303030", fontface="bold")+
  geom_text(aes(x=3.5, y=-.38, label = "FG PCT VS LEAGUE AVG"), size=4, family="Open Sans", color="#303030", fontface="bold")+
  ylim(-1,0)+
  theme_void()+
  theme(legend.position="none")

#ggsave(plot = hex_fill_legend, filename=paste0(save_dir,"hex_fill_legend.png"), width=2.5, height=2.5, units='in', dpi=300, bg="transparent")


# lebron icon
lebron_pic<-paste0(dir,"/Lebron/images/lebron_circle_halftone.webp")

# main plot bg 
main_plot<-ggplot()+
  theme_void()+
  xlim(0,14)+
  ylim(0,14)+
  theme(plot.background = element_rect(color="#FFFCEF"))


#save individual plots
for(i in 1:length(lebron_plots))
{
  final_plot<-ggdraw()+
    draw_plot(main_plot)+
    draw_image(lebron_pic, scale = .23, x=-.43, y=.4)+
    draw_plot(title, x=.18, y=.42)+
    draw_plot(subtitle, x=.18,y=.35)+
    draw_plot(hex_size_legend, width=.15, height=.2, x=.3,y=.7)+
    draw_plot(hex_fill_legend, width=.25, height=.2, x=.47, y=.7)+
    draw_plot(lebron_plots[[i]], height=.9, y=.05)+
    draw_plot(caption,x=.83,y=-.49)
  
  ggsave(plot = final_plot, filename=paste0(save_dir,"/subplots_for_anim/lebron",i,".png"), width=13, height=13, units='in', dpi=300, bg="#FFFCEF")
  
}

img_files<-paste0(save_dir,"/subplots_for_anim") %>%
  list.files(full.names=TRUE) %>%
  file.info() %>%
  tibble::rownames_to_column("filename") %>%
  arrange(mtime) %>%
  pull(filename)

# create gif
gifski::gifski(img_files, gif_file = paste0(save_dir,"lebron_gif.gif"), width = 1248, height = 1248, delay = 1.5)


