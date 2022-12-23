library(tidyverse)
library(cowplot)
library(ggtext)

# font for title
sysfonts::font_add_google("Bebas Neue","Bebas Neue")

# font for subtitles, etc.
sysfonts::font_add_google("Roboto Condensed","Roboto Condensed")
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

weather_forecasts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/weather_forecasts.csv')
cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv')
outlook_meanings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/outlook_meanings.csv')

# join and create full dataset
full_dat<-weather_forecasts %>%
left_join(cities) %>%
left_join(outlook_meanings) %>%
mutate(diff = observed_temp-forecast_temp,
       month = lubridate::month(date, label=TRUE, abbr=FALSE),
       region = case_when(
         state %in% c("WA","OR","CA","AK","HI") ~ "Pacific",
         state %in% c("MT","ID","WY","NV","AZ","NM","CO","UT") ~ "Mountain",
         state %in% c("ND","SD","MN","NE","IA","KS","MO") ~ "West North Central",
         state %in% c("WI","IL","IN","MI","OH") ~ "East North Central",
         state %in% c("PA","NY","NJ") ~ "Mid-Atlantic",
         state %in% c("ME","VT","NH","MA","CT","RI") ~ "New England",
         state %in% c("TX","OK","AR","LA") ~ "West South Central",
         state %in% c("KY","TN","MS","AL") ~ "East South Central",
         state %in% c("WV","DC","DE","MD","VA","NC","SC","GA","FL") ~ "South Atlantic",
         TRUE ~ "Other Regions"
       ),
       city = str_replace_all(city,"_"," "))

# create region and city mean diff datasets
region_diff<-full_dat %>%
filter(forecast_hours_before %in% c(24,12)) %>% 
group_by(month,region) %>%
summarise(mean_obs=mean(observed_temp,na.rm=TRUE),
          mean_forecast = mean(forecast_temp, na.rm=TRUE),
          mean_diff = mean(diff, na.rm=TRUE)) 

city_diff<-full_dat %>%
filter(forecast_hours_before %in% c(24,12)) %>% 
group_by(month,city) %>%
summarise(mean_obs= mean(observed_temp, na.rm=TRUE),
          mean_forecast = mean(forecast_temp, na.rm=TRUE),
          mean_diff = mean(diff, na.rm=TRUE)) %>%
left_join(full_dat %>% select(city,region,state) %>% distinct()) %>%
mutate(full_city_name = paste0(str_to_title(city),", ",state)) %>%
ungroup()


# find min and max 
min<-round(range(city_diff$mean_diff, na.rm=TRUE)[1])
max<-round(range(city_diff$mean_diff, na.rm=TRUE)[2])

# create the center plot that will have title, subtitle, legend and caption
title_plot<-ggplot(data = city_diff, aes(x=month, y=mean_diff, group=city, color=mean_diff))+
geom_segment(data = city_diff, aes(x=month, xend=month, y=0, yend=1.5),color="#B2B2B2", alpha=.2)+
geom_jitter(width=.45, alpha=.5, aes(size=abs(mean_diff)))+
scale_color_viridis_c(option="magma")+
scale_size(range=c(.5,3))+
geom_text(data = city_diff, aes(x=month,y=1.6, label=str_sub(month,1,3)), color="#B2B2B2", family="Roboto Condensed", fontface="bold", alpha=.2, size=6)+
ylim(-5,5)+
theme(
      panel.background = element_blank(),
      legend.position = "none",
      plot.title=element_text(hjust=.5, vjust=.5),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank())+coord_polar(clip="off")

# title 
title<-ggplot()+
geom_text(aes(x=0,y=0,label="Weather Forecasting \nAccuracy Across The US"), family="Bebas Neue", color="#B2B2B2", size=13, fontface="bold", lineheight=.8)+
theme_void()+
coord_fixed()

# title underline 
segment<-ggplot()+
geom_segment(aes(x=0,y=0,xend=.5,yend=0), color="#B2B2B2",linewidth=2.5)+
theme_void()+
coord_fixed()

# subtitle text
subtitle_text<-paste0("A look at the difference between observed versus forecasted temperature averages for each month across <b>", length(unique(city_diff$city))," cities</b> in the United States.")

# subtitle 
subtitle<-ggplot()+
geom_textbox(aes(x=0,y=0,label=subtitle_text), family="Roboto Condensed", color="#B2B2B2", size=5, width=unit(.3,"npc"), hjust=.5, box.color=NA, fill=NA)+
theme_void()+
coord_fixed()

# legend 
legend<-ggplot()+
geom_segment(aes(x=breaks,y=0,xend=breaks,yend=-.050), linetype="dotted", color="#B2B2B2")+
geom_point(aes(x=breaks,y=rep(0,4), size=breaks, color=breaks))+
geom_text(aes(x=breaks,y=-.050,label=label), family="Roboto Condensed", color="#B2B2B2", size=5)+
scale_size(c(.8,3))+
scale_color_viridis_c(option="magma")+
theme_void()+
theme(legend.position="none")

# main plot 
main_plot<-ggplot()+
theme_void()+
theme(plot.background = element_rect(fill="#ECECEC", color="#ECECEC"))

# subplots 
subplots<-function(region_name)
  {
  
  filtered_df<-city_diff %>% filter(region==region_name)
  filtered_min<-round(range(filtered_df$mean_diff)[1])
  filtered_max<-round(range(filtered_df$mean_diff)[2])
    
  p<-ggplot(data = city_diff %>% filter(region!=region_name),  aes(x=month, y=mean_diff, group=city))+
     geom_segment(data = city_diff,aes(x=month,xend=month, y=0, yend=4.8), color="#B2B2B2")+
     geom_text(data = city_diff,aes(x=month,y=5,label=str_sub(month,1,3)), color="#B2B2B2", family="Roboto Condensed")+
     geom_text_repel(data = city_diff %>% filter(region==region_name, month=="December"), aes(x="December", y=mean_diff, label=full_city_name), color="#B2B2B2", family="Roboto Condensed", max.overlaps=Inf, segment.alpha=.3,
                     direction= "y",
                     size = 2.5,
                     xlim = c(15,NA),
                     ylim = c(-6,6),
                     box.padding = 0.5,
                     vjust=0,
                     nudge_y=-.1,
                     segment.curvature = -0.1,
                     segment.ncp = 3,
                     segment.angle = 20)+
     geom_point(aes(size=abs(mean_diff)), alpha=.3,color="#D5D5D5")+
     geom_line(alpha=.3, color="#D5D5D5",linewidth=.5)+
     geom_line(data = city_diff %>% filter(region==region_name), aes(x=month, y=mean_diff, group=city, color=mean_diff), alpha=.3, linewidth=.7)+
     geom_point(data = city_diff %>% filter(region==region_name), aes(x=month, y=mean_diff, group=city, color=mean_diff, size=abs(mean_diff)), alpha=.3)+
     scale_color_viridis_c(option="magma", values=scales::rescale(min:max, to=c(0,1), from=c(filtered_min,filtered_max)))+
     scale_size(range=c(.8,3))+
     geom_textline(data = region_diff %>% filter(region==region_name), aes(x=month, y=mean_diff, group=region, label="Regional Average"),  size=4, alpha=.9, linewidth=2, family="Roboto Condensed", spacing=20, fontface="bold", hjust=.1, color="#222222")+
     ylim(-5,5)+
     labs(title=region_name)+
     ylab("Observed Temp - Forecasted Temp(°F)")+
     theme(
      plot.title = element_text(hjust=.5, family="Bebas Neue", size=20, color="#B2B2B2"),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.background = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(family="Roboto Condensed", color="#B2B2B2", size=10),
      axis.title.y = element_text(family="Roboto Condensed", color="#B2B2B2", size=10))+
    coord_cartesian(clip="off")
    return(p)
}


# create subplots 
unique(city_diff$region) %>%
map(function(x) assign(x,subplots(x), envir=.GlobalEnv))

# arrange with cowplot 
weather<-ggdraw()+
         draw_plot(main_plot)+
         draw_plot(title_plot, width=.9, height=.9, x=.04, y=.05)+
         draw_plot(title, x=.048, y=.08, width=.9)+
         draw_plot(subtitle, x=.048, y=.02, width=.9)+
         draw_plot(segment,x=.378,y=.048, width=.24)+
         draw_plot(Mountain,x=0,y=.73, width=.265, height=.265)+
         draw_plot(Pacific, x=0,y=.48, width=.265, height=.265)+
         draw_plot(`West South Central`, x=0, y=.23, width=.265, height=.265)+
         draw_plot(`Other Regions`, x=0,y=0, width=.265, height=.265)+
         draw_plot(`West North Central`, x=.34, y=.73, width=.265, height=.265)+
         draw_plot(`East South Central`, x=.34, y=0, width=.265, height=.265)+
         draw_plot(`East North Central`, x=.68, y=.73, width=.265, height=.265)+
         draw_plot(`New England`, x=.68, y=.48, width=.265, height=.265)+
         draw_plot(`Mid-Atlantic`, x=.68, y=.23, width=.265, height=.265)+
         draw_plot(`South Atlantic`, x=.68, y=0, width=.265, height=.265)


         
ggsave(plot = weather, filename="weather.png",width=20, height=15, units='in', dpi=300, bg="#ECECEC")