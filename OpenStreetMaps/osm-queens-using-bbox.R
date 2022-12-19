library(osmdata)
library(sf)
library(tidyverse)
library(tibble)
library(ggpattern)
library(ggtext)
library(magick)
library(gradienttext)

sysfonts::font_add_google("Saira Condensed","Saira Condensed")
sysfonts::font_add_google("Roboto Condensed","Roboto Condensed")
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# lat, long manually collected from the internet
queens_locations<-tribble(
  ~cities,             ~lat,     ~long,     ~bbox_x_min, ~bbox_y_min, ~bbox_x_max, ~bbox_y_max,
  "Sunnyside",       40.74288,   -73.9188,   -73.9455,    40.7350,     -73.9082   , 40.7525,
  "Woodside",        40.74526,   -73.9048,   -73.9168,    40.7348,     -73.8915   , 40.7577, 
  "Jackson Heights", 40.755682,  -73.88307,  -73.9092,    40.7448,     -73.8624   , 40.7665,
  "Elmhurst"       , 40.74291,   -73.87998,  -73.9018,    40.7257,     -73.8550   , 40.7475, 
  "Corona"         , 40.73497,   -73.86497,  -73.8835,    40.7360,     -73.8367   , 40.7578,
  "Flushing"       , 40.768452,  -73.832764, -73.8408,    40.7545,     -73.7940   , 40.7763
)

# help from https://taraskaduk.com/posts/2021-01-18-print-street-maps/ tutorial 

highways<-c(
  'motorway',       
  'motorway_link',  
  'trunk',          
  'primary',        
  'primary_link',   
  'secondary',      
  'secondary_link', 
  'tertiary',       
  'tertiary_link',  
  'cycleway',       
  'residential',    
  'living_street',  
  'unclassified',   
  'pedestrian',     
  'service',        
  'footway',        
  'path')

# create a function for the map 
get_map<-function(city, nudge_long=0, nudge_lat=0, buffer, pal)
{
 
    
  bbox = queens_locations %>% 
         filter(cities == city) %>% 
         select(bbox_x_min, bbox_y_min, bbox_x_max, bbox_y_max) %>%
         unlist()
  
  highways_osm<-opq(bbox = bbox) %>% add_osm_feature(key = 'highway', value = highways) %>% osmdata_sf()
    
  
  buildings_osm<-opq(bbox = bbox) %>% add_osm_feature(key='building') %>% osmdata_sf()
  

  park_osm<-opq(bbox = bbox) %>% add_osm_feature(key = 'leisure', value='park') %>% osmdata_sf()
  
  
  water_osm<-opq(bbox = bbox) %>% add_osm_feature(key='natural', value='water') %>% osmdata_sf()
  

  railways_osm<-opq(bbox = bbox) %>% add_osm_feature(key = 'railway') %>% osmdata_sf()
  
  buildings_osm$osm_polygons<-buildings_osm$osm_polygons %>% mutate(color = sample(1:5, n(), replace = TRUE)) 
  
  circle <- tibble(lat= queens_locations %>% filter(cities==city) %>% pull(lat) + nudge_lat, long = queens_locations %>% filter(cities==city) %>% pull(long) + nudge_long) %>%
    st_as_sf(coords = c("long","lat"), crs=4326) %>% 
    st_transform(2263) %>% 
    st_buffer(dist = buffer) %>%
    st_transform(4326)
  
  circle_buff<-st_bbox(circle)
  
  
  p<-ggplot()+
    geom_sf_pattern(data=circle, pattern='circle', pattern_spacing=.005, fill="#060110", pattern_color='#2B2B2B', color=NA)
  
  if(!is.null(water_osm$osm_polygons))
  {
    p<-p+geom_sf_pattern(data=water_osm$osm_polygons %>% st_intersection(circle),  color=NA, pattern='circle', pattern_spacing=.005, pattern_color='#4cc9f0', fill='#060110', alpha=.8)
  }
  
  
  p<-p+
    geom_sf(data=buildings_osm$osm_polygons %>% st_intersection(circle), aes(fill=as.factor(color)), color=NA)+
    geom_sf_pattern(data=park_osm$osm_polygons %>% st_intersection(circle), pattern_spacing=.005, pattern='crosshatch', pattern_color='#009122', fill="#060110", color=NA)+
    geom_sf(data=railways_osm$osm_lines %>% st_intersection(circle), color="#F9F9F9")+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("motorway","motorway_link","trunk")) %>% st_intersection(circle),linewidth=.9, color="#F9F9F9")+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("primary","primary_link"))%>% st_intersection(circle), linewidth=.8, color="#F9F9F9")+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("secondary","secondary_link"))%>% st_intersection(circle), linewidth=.7, color="#F9F9F9")+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("tertiary","tertiary_link","cycleway"))%>% st_intersection(circle), linewidth=.6, color="#F9F9F9")+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("residential","living_street"))%>% st_intersection(circle), linewidth=.5, color="#F9F9F9")+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("pedestrian","unclassified","service","footway","path"))%>% st_intersection(circle), linewidth=.4, color="#F9F9F9")+
    scale_fill_manual(values=pal)+
    geom_sf(data=circle,fill=NA,color='#F9F9F9', linewidth=1.5)+
    theme_void()+
    labs(caption = paste0("<b><span style='font-size:110px'>", city,"</span></b><br><span style='font-size:50px'>",round(queens_locations %>% filter(cities==city) %>% pull(lat),4),"°N ",round(queens_locations %>% filter(cities==city) %>% pull(long),4),"°W</span>"))+
    theme(
      legend.position="none",
      plot.background = element_blank(),
      plot.caption=element_markdown(family="Roboto Condensed", color="#F9F9F9",hjust=.5, lineheight=2.5)
    )+
    coord_sf(xlim=c(circle_buff[1], circle_buff[3]), ylim=c(circle_buff[2], circle_buff[4]))
  
  return(p)
}

#palettes 
#colors<-c("#390099","#9e0099","#FF0037","#ff6f00","#fff200")
#colors<-c("#231942","#5e548e","#9f86c0","#be95c4","#f1e9f5")
#colors<-c("#770bf3","#9e0067","#ff0037","#fd801f","#F8E400")

# create map for each city/neighborhood
sunnyside<-get_map(city="Sunnyside", buffer=4000, pal=c("#5b18bf","#9e0067","#ff0040","#ff8e2b","#ffe11e"), nudge_lat=.001, nudge_long=-.006)
woodside<-get_map(city="Woodside", buffer=4000, pal = c("#5b18bf","#9e0067","#ff0040","#ff8e2b","#ffe11e"))
jackson_heights<-get_map(city="Jackson Heights", buffer=4000, pal = c("#5b18bf","#9e0067","#ff0040","#ff8e2b","#ffe11e"),nudge_long=-.003)
elmhurst<-get_map(city="Elmhurst", buffer=4000, pal =c("#5b18bf","#9e0067","#ff0040","#ff8e2b","#ffe11e"), nudge_lat=-.006, nudge_long=.002)
corona<-get_map(city="Corona", buffer=4000, pal =c("#5b18bf","#9e0067","#ff0040","#ff8e2b","#ffe11e"),nudge_lat=.011, nudge_long=.004)
flushing<-get_map(city="Flushing", buffer=4000, pal =c("#5b18bf","#9e0067","#ff0040","#ff8e2b","#ffe11e"),nudge_lat=-.003, nudge_long=.016)

# create base ggplot2
main_plot<-ggplot()+
  labs(
    title = "<span style='font-size:80px'>Neighborhoods Along Roosevelt Avenue</span><br><span style='font-size:50px'>Queens, New York City</span>",
    subtitle = "Roosevelt Avenue-a major throughfare in the borough of Queens, traverses through some of the most ethnically diverse neighborhoods",
    caption = "<b>Data Source</b>: OpenStreetMap® Contributors <b>Map by</b>: Samia B <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span> samiaab1990")+
  theme_void()+
  theme(plot.background = element_rect(fill='#060110', color='#060110'),
        panel.background = element_rect(fill='#060110',color='#060110'),
        plot.title = element_markdown(family="Saira Condensed", hjust=.5,face="bold",  margin=margin(t=10,unit="mm"),color="#F9F9F9",lineheight=.08),
        plot.caption = element_markdown(family="Roboto Condensed",hjust=.5, color="#F9F9F9",  size=11))+
  coord_fixed(xlim=c(0,17), ylim=c(0,11),clip="off")

# save files and use magick to put together 
ggsave(plot = main_plot, filename='main_plot.png',width=17, height=11, units='in', dpi=300, bg="#060110")

city_plots<-list(sunnyside,woodside,jackson_heights,elmhurst,corona,flushing)

for(i in 1:length(city_plots))
{
  ggsave(plot = city_plots[[i]], filename=paste0(queens_locations$cities[i],".png"),width=15, height=15, units='in', dpi=300, bg="transparent")
}

dir<-"~/GitHub/Data-Visualizations/OpenStreetMaps/"

main_plot_magick<- image_read(paste0(dir,"main_plot.png"))

for(i in 1:length(queens_locations$cities))
{
  assign(queens_locations$cities[i],image_read(paste0(dir,queens_locations$cities[i],".png")) %>% image_scale("1300") %>% image_scale("x1300"), envir=.GlobalEnv)
}

sunnyside_plot<-image_composite(main_plot_magick, Sunnyside, offset="+250+600")
woodside_plot<-image_composite(sunnyside_plot, Woodside, offset="+1900+600")
jh_plot<-image_composite(woodside_plot, `Jackson Heights`, offset="+3600+600")
elmhurst_plot<-image_composite(jh_plot, Elmhurst, offset="+250+1900")
corona_plot<-image_composite(elmhurst_plot, Corona, offset="+1900+1900")
flushing_plot<-image_composite(corona_plot, Flushing, offset="+3600+1900")


image_write(flushing_plot, "full_plot.png")