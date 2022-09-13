# libraries 
library(sf)
library(rgdal) 
library(ggplot2) 
library(tidyverse) 
library(raster) 
library(broom)
library(maptools)
library(ggdark)
library(ggrepel)
library(lubridate)
library(stringr)
library(ggthemes)
library(smoothr)
library(RColorBrewer)
library(reshape2)
library(sp)
library(broom)
library(rgeos)
library(patchwork)
library(cowplot)
library(Cairo)
library(extrafont)
library(ggtext)
library(stars)
CairoWin()

# fonts
sysfonts::font_add_google("Dosis","Dosis")
sysfonts::font_add_google("Bebas Neue","Bebas Neue")

# rainfall and temperature datasets
rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

# shapefiles 

## australia country shapefile 
australia<-readOGR("aust_cd66states.shp") 

## australia climate & rainfall grid
aus_climate<-raster('2019120120191231 (1).grid') 
rainfall_grid<-raster('precip_total_r005_20191201_20191231.txt')
#max_annual_avg<-raster('maxann.txt')

# Convert raster file to data frame 

## convert to spatial pixels data frame, then as.data.frame 
#max_ann_df<-max_annual_avg %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% rename(long=x,lat=y)

## from rainfall, find the distinct Austrian cities 
australia_cities<-rainfall %>% dplyr::select(city_name,long,lat) %>% distinct(city_name, .keep_all=TRUE)

# Fortify the Austriala shapefile 
australia_fortified<-australia %>% fortify(australia, region='STE')

# Convert raster to data frame 
climate_spdf <- aus_climate %>% as("SpatialPixelsDataFrame")
climate_df<- aus_climate %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% rename(long=x, lat=y)

# Convert rainfall grid raster to data frame
rainfall_spdf<-as(rainfall_grid, "SpatialPixelsDataFrame") %>% setNames("precipitation")
rainfall_df<-as.data.frame(rainfall_spdf)

# Create shapes for the rainfall interior 
rainfall_interior_spdf<-rainfall_spdf[!is.na(over(rainfall_spdf, as(australia,"SpatialPolygons"))),]
rainfall_interior<-as.data.frame(rainfall_interior_spdf) 
rainfall_interior_raster<-raster(rainfall_interior_spdf)

# Australia Heatmap-----------------------------------------------------------------------------
australia_climate_interior<-climate_spdf[!is.na(over(climate_spdf, as(australia, "SpatialPolygons"))), ] %>%
                            as.data.frame() %>%
                            rename(temp=X2019120120191231_.1.) %>%
                            mutate(temp.f=((temp*9/5)+32))


min(australia_climate_interior$temp.f)
max(australia_climate_interior$temp.f)

breaks<-seq(50,120, by=10)
breaks.c<-round(((breaks-32)*5/9),0)
labels<-paste0(breaks,rep(" ",8),"\n",breaks.c,rep(" ",8))



temp_pal<-c("#F29D80","#EC9884","#E69387","#DF8D8B","#D9888E","#D38392","#CD7E95","#C77999","#C1749C","#BA6EA0","#B469A3","#AE64A7","#A85FAA","#A25AAE","#9C55B1","#954FB5","#8F4AB8","#8945BC")

title2<-"A Year in Review"

make_title<-""

for(i in 1:nchar(title2))
{
  if(substr(title2,i,i) !=" ")
  {
    make_title<-paste0(make_title,"<span style='font-family:Dosis; font-size:80px; color:",temp_pal[i],";'>",substr(title2,i,i),"</span>")
  } else
  {
    make_title<-paste0(make_title," ")
  }
    
}
dec_avg<-ggplot()+
  geom_tile(data = australia_climate_interior, 
            aes(x = x, 
                y = y, 
                fill = temp.f))+
  geom_polygon(data = australia_fortified, 
               aes(x = long, 
                   y = lat, 
                   group = group), 
               fill = NA, 
               color = "#3E3E3E", 
               size = 0.3) +
  geom_point(data=australia_cities,aes(long,lat), color="#D8D0D0")+
  labs(
  title = "<b>Australia Climate 2019</b>",
  caption = "<b>Source:</b> Australian Government Bureau of Meteorology<br><b>Github:</b>samiaab1990</b>",
  subtitle = 
   paste0(make_title,"<br><br>2019 was Australia's warmest year in history, breaking previous records with a <b style='color:#F29D80'>1.52 °C increase</b><br> in mean average temperatures relative to the average temperatures recorded between 1961-1990<br> and a
   <b style='color:#EC9884'>2.09°C </b>increase in mean maximum temperatures. The warmest months were <b style='color:#E69387'>January</b>,<b style='color:#DF8D8B'> March </b><br>and <b style='color:#D9888E'>December</b>. The map below shows the daily mean maximum temperatures for December 2019."),
  )+
  geom_text_repel(data=australia_cities,aes(long,lat,label=city_name), size=5, color="#D8D0D0", family="Roboto")+
  scale_fill_viridis_c(option = "plasma", breaks=breaks, labels=labels) +
  coord_equal() +
  dark_theme_gray()+
  theme(plot.title = element_markdown(size=80, family="Bebas Neue", color="#D8D0D0", hjust=.5),
        plot.subtitle = element_markdown(size=15, family="Roboto", color="#D8D0D0", hjust=.5, margin=margin(0,0,-10,0)),
        plot.caption = element_markdown(size=10, family="Roboto", color="#D8D0D0", hjust=1),
        plot.background = element_rect(fill = "#1B1B1B", color=NA),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        legend.position = "bottom",
        legend.justification="right")+
  guides(fill=guide_colorbar(title="Temperature °F/°C",ticks=FALSE, direction="horizontal", barwidth = 15,  barheight= 1,label.position="bottom", title.position = "top", title.vjust = 0,  title.hjust=.5, label.vjust=2.5, label.hjust=.8,label.theme=element_text(size=12, family='Roboto', color="#D8D0D0"), title.theme = element_text(size=12, family='Roboto', color='#D8D0D0') ))

save_plot(plot = dec_avg, filename = "temp.png",  base_width=10, base_height=10, units='in', dpi=100)


# Rainfall Polygon map---------------------------------------------------------------------------

## Create a levels variable based on the precipitation
rainfall_interior_raster$level<-cut(rainfall_interior_raster$precipitation, c(-Inf,1,5,10,25,50,100,200,300,400,Inf), label=FALSE)

## Convert to polygon
rainfall_interior_raster_poly<-rainfall_interior_raster %>% st_as_stars() %>% st_as_sf()

## Convert to df 
rainfall_interior_raster_df<- rainfall_interior_raster %>% 
                              st_as_stars() %>% 
                              st_as_sf() %>% 
                              sf::as_Spatial()
rainfall_interior_raster_df<-SpatialPolygonsDataFrame(rainfall_interior_raster_df,rainfall_interior_raster_df@data) %>%
  tidy(region="level")

rain_pal<-c("#69EFC6","#6EE9C8","#74E4CB","#79DECD","#7FD9CF","#84D3D1","#89CED4","#8FC8D6","#94C3D8","#9ABDDB","#9FB8DD","#A5B2DF","#AAADE2","#AFA7E4","#B5A2E6","#BA9CE8","#C097EB","#C591ED")

title2<-"A Year in Review"

make_title<-""

for(i in 1:nchar(title2))
{
  if(substr(title2,i,i) !=" ")
  {
    make_title<-paste0(make_title,"<span style='font-family:Dosis; font-size:80px; color:",rain_pal[i],";'>",substr(title2,i,i),"</span>")
  } else
  {
    make_title<-paste0(make_title," ")
  }
  
}
rain_avg_plot<-ggplot()+
  geom_sf(data = rainfall_interior_raster_poly, aes(fill=level), color=NA)+
  geom_polygon(data=rainfall_interior_raster_df, aes(long,lat,group=group), color="#3E3E3E", fill=NA, size=.3)+
  geom_polygon(data = australia_fortified, 
               aes(x = long, 
                   y = lat, 
                   group = group), 
               fill = NA, 
               color = "#3E3E3E", 
               size = 0.3) +
  geom_point(data=australia_cities,aes(long,lat), color="#D8D0D0")+
  geom_text_repel(data=australia_cities,aes(long,lat,label=city_name), size=5, color="#D8D0D0", family="Roboto")+
  scale_fill_viridis_c(option="viridis", breaks=c(1,2,3,4,5,6,7,8,9,10), label=c("0","1","5","10","25","50","100","200","300","400+"))+
  coord_sf() +
  dark_theme_gray()+
  labs(
    title = "<b>Australia Climate 2019</b>",
    caption = "<b>Source:</b> Australian Government Bureau of Meteorology<br><b>Github:</b>samiaab1990</b>",
    subtitle = 
      paste0(make_title,"<br><br>Most parts of Australia were drier than average in 2019, recieving the <b style='color:#69EFC6'>least rainfall since 1900</b>.The <br>total rainfall across the country was <b style='color:#6EE9C8'>277.6 millimeters</b>&mdash;<b style='color:#74E4CB'>40% below</b> the average rainfall between <br> 1961-1990. The map below shows the rainfall totals for December 2019."),
  )+
  theme(
    plot.title = element_markdown(size=80, family="Bebas Neue", color="#D8D0D0", hjust=.5),
    plot.subtitle = element_markdown(size=15, family="Roboto", color="#D8D0D0", hjust=.5, margin=margin(0,0,-10,0)),
    plot.caption = element_markdown(size=10, family="Roboto", color="#D8D0D0", hjust=1),
    plot.background = element_rect(fill = "#1B1B1B", color=NA),
    legend.text=element_text(color="#B3B3B3", size=12, lineheight = .3, family="Roboto"),
    legend.title=element_text(color="#B3B3B3", size=12, vjust=.8, family="Roboto"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_blank(),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    axis.text=element_blank(),
    axis.title=element_blank(),
    legend.position = "bottom",
    legend.justification="right")+
guides(fill=guide_colorbar(title="Rainfall(mm)",ticks=FALSE, direction="horizontal", barwidth = 20,  barheight=.8,label.position="bottom", title.position = "top", title.vjust = -1, title.hjust=.5, label.vjust=2))
  save_plot(plot = rain_avg_plot, filename = "rain_avg_plot.png",  base_width=10, base_height=10, units='in', dpi=100)
  
# Temperature facet grid-------------------------------------------------------------------------

max_temp<-temperature %>% 
          filter(temp_type=="max") %>%
          mutate( date = ymd(date),
                  year = year(date),
                  month = month(date),
                  city_name = str_to_title(city_name),
                  city_name = ifelse(city_name=="Kent","Adelaide",city_name),
                  temp.f=(temperature*9/5)+32) %>%
          filter(city_name!="Port")

max_temp_avg<-max_temp %>% group_by(city_name, year) %>% 
              summarise(temp=mean(temperature, na.rm=TRUE)) %>% 
              mutate(temp.f=(temp*9/5)+32)

thirty_yr_avg<-max_temp_avg %>% 
               filter(year>=1961 & year<=1990)  %>%
               group_by(city_name) %>%
               summarise(avgtemp=mean(temp, na.rm=TRUE))
  

max_temp_avg<-merge(max_temp_avg,thirty_yr_avg) %>%
              mutate(diff=temp-avgtemp) %>%
              filter(year!=2020)



facet_grid_temp<-ggplot()+
  geom_hline(yintercept=0, size=.8, color="#232323")+
  geom_line(data=max_temp_avg, aes(x=year, y=diff, color=diff), size=.5)+ 
  scale_x_continuous(breaks=seq(1910,2019, by=20))+
  scale_y_continuous(limits=c(-7,7))+
  dark_theme_grey()+
  scale_color_viridis_c(option = "plasma")+
  geom_text(data=max_temp_avg %>% filter(year==2019),aes(x=year,y=diff+.5, label=paste0(round(diff,1),"°")), size=5)+
  geom_text(data=max_temp_avg,aes(x=2019,y=-1, label="2019"),size=5)+
  theme(plot.background = element_rect(fill="#000000"),
        axis.title.x = element_text(size=15, family="Roboto", color="#D8D0D0"),
        axis.title.y = element_text(size=15, family="Roboto", color="#D8D0D0"),
        axis.ticks=element_blank(), 
        strip.text.x = element_text(size=20, family="Roboto", color="#D8D0D0"),
        axis.text.x=element_text(size=20, family="Roboto", color="#D8D0D0"),
        legend.position = "none", 
        axis.text.y=element_text(size=20,family="Roboto", face="bold", color="#D8D0D0"),
        panel.grid.major = element_line(color="#232323", size=.3),
        panel.grid.minor = element_line(color="#232323", size=.3),
        legend.background = element_blank(), strip.text = element_text(size=20, face="bold", color="#D8D0D0", family="Roboto"), strip.background = element_blank(), panel.background = element_blank())+
  labs(x="Year", y="Change in °C")+
  facet_wrap(~city_name)
ggsave(filename ="facet_grid_temp2.png", plot=facet_grid_temp, width=15, height=10, dpi=300,device='png',type='cairo')


rainfall_avg<-rainfall %>%
              group_by(city_name,year) %>%
              summarise(rainfall=mean(rainfall,na.rm=TRUE)) %>%
              filter(year>=1910 & year<2020)

# Rainfall facet grid--------------------------------------------------------------------------------  

facet_grid_rainfall<-ggplot()+
  geom_col(data= rainfall_avg %>% filter(year<2019), aes(x=year,y=rainfall), fill="#232323")+
  geom_col(data= rainfall_avg %>% filter(year==2019), aes(x=year,y=rainfall, fill=rainfall), alpha=1)+
  facet_wrap(~city_name)+
  scale_x_continuous(breaks=seq(1910,2019, by=20))+
  geom_text(data=rainfall_avg,aes(x=2019-1,y=8, label="2019"),size=5, face="bold")+
  dark_theme_grey()+
  geom_text(data=rainfall_avg %>% filter(year==2019),aes(x=2019-1.5,y=rainfall+1, label=paste0(round(rainfall,1),"mm")),size=5, face="bold")+
  scale_fill_viridis_c(option="viridis")+
  theme(plot.background = element_rect(fill="#000000"),
        axis.ticks=element_blank(), 
        strip.text.x = element_text(size=20, family="Roboto", color="#D8D0D0"),
        axis.text.x=element_text(size=20, family="Roboto", color="#D8D0D0"),
        axis.title.x = element_text(size = 15, family="Roboto", color="#D8D0D0"),
        axis.title.y = element_text(size = 15, family="Roboto", color="#D8D0D0"),
        legend.position = "none",                           
        panel.grid.major = element_line(color="#232323", size=.3),
        panel.grid.minor = element_line(color="#232323", size=.3),
        legend.background = element_blank(), strip.text = element_text(size=12, face="bold", color="#D8D0D0", family="Roboto"), strip.background = element_blank(), panel.background = element_blank())+
  labs(x="Year", y="Average Rainfall(mm)")
ggsave(filename ="facet_grid_rainfall2.png", plot=facet_grid_rainfall, width=15, height=10, dpi=300,device='png',type='cairo')

