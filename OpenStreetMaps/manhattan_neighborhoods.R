library(osmdata)
library(sf)
library(tidyverse)
library(tibble)
library(ggtext)
library(magick)
library(gradienttext)
library(stringr)
library(smoothr)

dir<-"~/GitHub/Data-Visualizations/OpenStreetMaps/"


sysfonts::font_add_google("Bebas Neue","Bebas Neue")
sysfonts::font_add_google("Roboto Condensed","Roboto Condensed")
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


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

manhattan_shp<-paste0(dir,"Borough Boundaries/") %>% 
               list.files(full.names=TRUE) %>% 
               stringr::str_subset(".shp$") %>%
               st_read() %>%
               filter(boro_name=="Manhattan")
               
manhattan_roads<-opq('Manhattan, NY') %>% add_osm_feature(key='highway', value = highways) %>% osmdata_sf()

manhattan_roads_cropped<-manhattan_roads$osm_lines%>% st_transform(crs = st_crs(manhattan_shp)) %>% 
  st_intersection(manhattan_shp) %>% 
  smooth(method="ksmooth")
  

manhattan_neighborhoods<-paste0(dir,"Neighborhood Names GIS/") %>%
                         list.files(full.names=TRUE) %>%
                         str_subset(".shp$") %>% 
                         st_read() %>%
                         filter(borough=="Manhattan")


highways<-tribble(
  ~highway,  ~size_ratios,
  'motorway',       .9, 
  'motorway_link',  .9,
  'trunk',          .9,
  'primary',        .8,
  'primary_link',   .8,
  'secondary',      .7,
  'secondary_link', .7,
  'tertiary',       .6,
  'tertiary_link',  .6,
  'cycleway',       .6,
  'residential',    .5,
  'living_street',  .5,
  'unclassified',   .4,
  'pedestrian',     .4,
  'service',        .4,
  'footway',        .4,
  'path',           .4
  
)

# create a function for the map 
get_map<-function(neighborhood, nudge_long=0, nudge_lat=0, buffer=1500, pal_value)
{
  neighborhoods_not_listed<-tibble(
    neighborhood_name = c("Central Harlem","Sutton Place","Stuyvesant Town"),
    xmin = c(-73.94638,-73.9675,-73.9899),
    ymin = c(40.81383,40.7548,40.7258),
    xmax = c(-73.94028,-73.9589,-73.9665),
    ymax = c(40.81748,40.7608,40.7376))
  
  point<-manhattan_neighborhoods %>% filter(name == neighborhood)
  
  if(neighborhood %in% neighborhoods_not_listed$neighborhood_name)
  {
    corrected_nb<-neighborhoods_not_listed %>% filter(neighborhood == neighborhood_name)
    osm_query<-c(corrected_nb['xmin'],corrected_nb['ymin'],corrected_nb['xmax'],corrected_nb['ymax']) %>% unlist()
  }
  else{
  osm_query<-paste0(neighborhood,", Manhattan") %>% 
  getbb() %>% 
  as.vector()
  }
  
  coord<-point$geometry %>% unlist()
  
  water_osm<-list()
  
  highways_osm<-opq(bbox = osm_query) %>% add_osm_feature(key = 'highway', value = highways$highway) %>% osmdata_sf()
   
  buildings_osm<-opq(bbox = osm_query) %>% add_osm_feature(key='building') %>% osmdata_sf()
  
  park_osm<-opq(bbox = osm_query) %>% add_osm_feature(key = 'leisure', value='park') %>% osmdata_sf()
  
  railways_osm<-opq(bbox = osm_query) %>% add_osm_feature(key = 'railway') %>% osmdata_sf()
  
  highways_osm$osm_lines<-highways_osm$osm_lines %>% left_join(highways) 
  

  
  circle <- tibble(lat= coord[2] + nudge_lat, long = coord[1]+ nudge_long) %>% 
    st_as_sf(coords = c("long","lat"), crs=4326) %>% 
    st_transform(2263) %>% 
    st_buffer(dist = buffer) %>%
    st_transform(4326)
  
  circle_buff<-st_bbox(circle)
  
  p<-ggplot()+
    geom_sf(data=circle, fill = "#1F1F1F")+
    geom_sf(data=buildings_osm$osm_polygons %>% st_intersection(circle), fill=pal_value, color="#1F1F1F")+
    geom_sf(data=park_osm$osm_polygons %>% st_intersection(circle), pattern_spacing=.005, pattern='crosshatch', pattern_color=pal_value, fill="#1F1F1F", color=NA)+
    geom_sf(data=railways_osm$osm_lines %>% st_intersection(circle), color=pal_value)+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("motorway","motorway_link","trunk")) %>% st_intersection(circle),linewidth=.9, color=pal_value)+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("primary","primary_link"))%>% st_intersection(circle), linewidth=.7, color=pal_value)+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("secondary","secondary_link"))%>% st_intersection(circle), linewidth=.5, color=pal_value)+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("tertiary","tertiary_link","cycleway"))%>% st_intersection(circle), linewidth=.4, color=pal_value)+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("residential","living_street"))%>% st_intersection(circle), linewidth=.3, color=pal_value)+
    geom_sf(data=highways_osm$osm_lines %>% filter(highway %in% c("pedestrian","unclassified","service","footway","path"))%>% st_intersection(circle), linewidth=.2, color=pal_value)+
    geom_sf(data=circle,fill=NA,color=pal_value, linewidth=.8)+
    theme_void()+
    labs(caption = paste0("<b><span style='font-size:20px'>", neighborhood,"</span></b><br><span style='font-size:10px'>",round(coord[2],4),"°N ",round(coord[1],4),"°W</span>"))+
    theme(
      legend.position="none",
      plot.background = element_blank(),
      plot.caption=element_textbox(family="Roboto Condensed", color=pal_value,hjust=.5, width=unit(1,"npc"), halign=.5)
    )+
    coord_sf(xlim=c(circle_buff[1], circle_buff[3]), ylim=c(circle_buff[2], circle_buff[4]))
  
  return(p)
}

colors<-c("#ffa200","#dd4003","#cd0031", "#4e00ba")
pal<-colorRampPalette(colors)
neighborhoods_pal<-pal(length(manhattan_neighborhoods$name))

df_to_map<-tibble(neighborhood = manhattan_neighborhoods$name,
                  pal_value = neighborhoods_pal,
                  nudge_long = rep(0,length(manhattan_neighborhoods$name)),
                  nudge_lat= rep(0,length(manhattan_neighborhoods$name)),
                  buffer = rep(1500,length(manhattan_neighborhoods$name))
                  )
df_to_map<-df_to_map %>%
           mutate(
             nudge_lat = case_when(
             neighborhood == "Washington Heights" ~ -.004,
             neighborhood == "Greenwich Village" ~ .003,
             neighborhood == "Noho" ~ .003,
             neighborhood == "Sutton Place" ~ -.002,
             neighborhood == "Tudor City" ~ .002,
             neighborhood == "Flatiron" ~ .002,
             TRUE ~ nudge_lat
           ),
           buffer = case_when(
             neighborhood == "Central Harlem" ~ 1000,
             neighborhood == "Lincoln Square" ~ 1200,
             neighborhood == "Tudor City" ~ 1200,
             TRUE ~ buffer
           ),
           nudge_long = case_when(
             neighborhood == "Greenwich Village" ~ .001,
             neighborhood == "Noho" ~ -.005,
             neighborhood == "Stuyvesant Town" ~ -.004,
             neighborhood == "Flatiron" ~.001,
             TRUE ~ nudge_long
           ))
           

street_views<-df_to_map %>%
pmap(get_map)

neighborhood_grid<-grid.arrange(grobs = street_views, nrow=8)

manhattan_neighborhoods<-manhattan_neighborhoods %>%
  left_join(df_to_map %>% select(neighborhood, pal_value) %>% rename(name = neighborhood))

manhattan_street_map<-ggplot()+
  geom_sf(data=manhattan_roads_cropped, fill=NA, color="#6B6B6B", linewidth=.05)+
  geom_sf(data = manhattan_neighborhoods, aes(color=pal_value), size=2)+
  scale_color_identity()+
  theme_void()

title<-ggplot()+
  geom_textbox(aes(x=0,y=0,label=make_gradient(label = "Manhattan Neighborhoods", colors=colors)), family="Bebas Neue", fill=NA, box.colour=NA, size=35, width=unit(1,"npc"))+
  theme_void()+
  coord_fixed(clip="off")

caption<-ggplot()+
  geom_textbox(aes(x=0,y=0,label="Street views of 39 neighborhoods in the borough of Manhattan, New York City.<br>OpenStreetMap® Contributors, NYC Open Data <b>Viz By:</b> Samia B (<span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span> samiaab1990)"), width=unit(1,"npc"), family="Roboto Condensed", color="#6B6B6B", fill=NA, box.color=NA, size=7)+
  theme_void()+
  coord_fixed(clip="off")

p<-ggdraw()+
   draw_plot(title, x=.05, y=.43)+
   draw_plot(caption, x=.05,y=.39)+
   draw_plot(manhattan_street_map, width=.2, height=.2, x=.8, y=.8)+
   draw_plot(neighborhood_grid,  height=.8, y=.01)
   

ggsave(plot = p, filename="manhattan_neighborhoods.png",width=18, height=24, units='in', dpi=300, bg="#1F1F1F")