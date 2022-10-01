#libraries
library(dplyr)
library(ggplot2)
library(ggridges)
library(lubridate)
library(stringr)
library(forcats)
library(ggtext)
library(Cairo)
CairoWin()
sysfonts::font_add_google("Bebas Neue","Bebas Neue")
showtext::showtext_auto()

# temperature datasets
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

temp_summary<-temperature %>%
  mutate( date = ymd(date),
          year = year(date),
          month = month(date),
          city_name = str_to_title(city_name),
          city_name = ifelse(city_name=="Kent","Adelaide",city_name),
          decade = cut(year,10,labels=FALSE),
          temp.f=(temperature*9/5)+32
        ) %>%
  filter(city_name!="Port", temp_type=="max") 


mean_2019<-temp_summary %>%
filter(year==2019, temp_type=="max") %>%
group_by(city_name) %>%
summarise(mean_temp = mean(temperature, na.rm=TRUE))

mean_1961_1990<-temp_summary %>%
  filter(year>=1961 & year<=1990, temp_type=="max") %>%
  group_by(city_name) %>%
  summarise(mean_temp = mean(temperature, na.rm=TRUE))

mean_diff<-merge(mean_1961_1990, mean_2019, by="city_name") %>%
mutate(diff = mean_temp.y-mean_temp.x)


p<-ggplot()+
 geom_density_ridges_gradient(data=temp_summary %>% filter(year==2019), aes(x=temperature, y=city_name, fill=stat(x)), scale = 1.5, color="#484848")+
 geom_density_ridges(data=temp_summary %>% filter(year>=1961 & year<=1990), aes(x=temperature, y=city_name),fill="#FFFFFF", scale = 1.5, alpha=.1, color="#C1C1C1")+
 labs(
   title = "Australia Climate 2019",
   subtitle = "<br><br>2019 was Australia's warmest year in history, breaking previous records with a <b style='color:#F29D80'>1.52 °C increase</b> in mean <b>average</b> temperatures relative to the average temperatures recorded between 1961-1990 and a <b style='color:#EC9884'>2.09°C increase</b> in mean <b>maximum</b> temperatures relative to 1961-1990. The warmest months were <b style='color:#E69387'>January</b>,<b style='color:#DF8D8B'> March </b>and <b style='color:#D9888E'>December</b>. The chart below shows the mean maximum temperatures between 1961-1990 on the left compared to 2019 on the right for each city.",
   caption = "<b>Source:</b> Australian Government Bureau of Meteorology<br><b>Github:</b>samiaab1990</b>")+
 xlab("Temp(°C)")+
 scale_fill_viridis_c(option="plasma")+
 scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
 scale_x_continuous(expand = c(0, 0)) +
 geom_richtext(data = mean_1961_1990 %>% filter(city_name=="Sydney"), aes(x=-1, y = 6.7, label = "1961-1990"), size=60, alpha=.3, color="#C1C1C1", fill=NA, label.color=NA, family="Bebas Neue", lineheight=.2, hjust=0)+
 geom_richtext(data = mean_1961_1990 %>% filter(city_name=="Sydney"), aes(x=34, y = 6.7, label = "2019"), size=60, family="Bebas Neue", alpha=.3, color="#C86794", fill=NA, label.color=NA,hjust=0)+
 geom_richtext(data = mean_1961_1990 %>% filter(city_name=="Brisbane"), aes(x=mean_temp-22, y = 2.3 , label=paste0("Brisbane is Australia's <br> second hottest city, <br> having a mean max temp of<br><b style='color:#872C99'>", round(mean_1961_1990 %>% filter(city_name=="Brisbane") %>% pull(mean_temp)),"°C</b> between 1961-1990")), size=12, family="Roboto", fill=NA, color="#C1C1C1", label.color=NA, lineheight=.1, hjust=0)+
 geom_richtext(data = mean_2019 %>% filter(city_name=="Brisbane"), aes(x=mean_temp+6, y = 2.5 , label=paste0("vs 2019, where the <br> mean max temp <br> was <b style='color:#DD8EB1'>", round(mean_2019 %>% filter(city_name=="Brisbane") %>% pull(mean_temp)),"°C</b> ")), size=12, family="Roboto", fill=NA, color="#C1C1C1", label.color=NA, lineheight=.1, hjust=0)+
 geom_richtext(data = mean_diff %>% filter(city_name=="Adelaide"), aes(x=mean_temp.x-22, y = 1.3, label=paste0("Adelaide had the <br>greatest difference between<br> mean max temp in 2019<br> vs 1961-1990 at <b style='color:#F9F4B5'>", round(mean_diff %>% filter(city_name=="Adelaide") %>% pull(diff)),"°C</b> ")), size=12, family="Roboto", fill=NA, color="#C1C1C1", label.color=NA, lineheight=.1, hjust=0)+
 #geom_point(data = mean_diff %>% filter(city_name == "Adelaide"), aes(x=mean_temp.x, y = 1.7), color="#FFFFFF", size=2)+
 #geom_point(data = mean_diff %>% filter(city_name == "Adelaide"), aes(x=mean_temp.y, y=1.7), color="#FFFFFF", size=2)+
 #geom_segment(data = mean_diff %>% filter(city_name=="Adelaide"), aes(x=mean_temp.x, y=1.3, xend=mean_temp.x, yend=1.7),  linetype="dashed", color="#F9F4B5", arrow = arrow(length = unit(0.02, "npc"), type="closed"))+
 #geom_segment(data = mean_diff %>% filter(city_name=="Adelaide"), aes(x=mean_temp.y, y=1.3, xend=mean_temp.y, yend=1.7),  linetype="dashed", color="#F9F4B5", arrow = arrow(length = unit(0.02, "npc"), type="closed"))+
 geom_rect(data = mean_diff %>% filter(city_name == "Adelaide"), aes(xmin = mean_temp.x, ymin=1, xmax=mean_temp.y, ymax=1.8), fill="#F9F4B5", color="#F9F4B5", alpha=.1, linetype="dotted")+
 #geom_segment(data = mean_diff %>% filter(city_name=="Adelaide"), aes(x=mean_temp.x, y=1.3, xend=mean_temp.y, yend=1.3), linetype="dashed", color="#F9F4B5")+
 geom_segment(data = mean_diff %>% filter(city_name=="Adelaide"), aes(x=mean_temp.x-13, y=1.1, xend=mean_temp.x, yend=1.3), linetype="dotted", color="#F9F4B5")+
 geom_curve(data = mean_2019 %>% filter(city_name=="Brisbane"), aes(x = mean_temp+6, y = 2.4, xend = mean_temp, yend = 3.2),
    arrow = arrow(length = unit(0.01, "npc"), type="closed"), color="#DD8EB1", linetype="dashed")+
  geom_segment(data = mean_1961_1990 %>% filter(city_name=="Brisbane"), aes(x = mean_temp-12, y = 2.4, xend = mean_temp, yend = 3.4),
               arrow = arrow(length = unit(0.01, "npc"), type="closed"), color="#872C99", linetype="dashed")+
   theme(
     plot.title = element_text(size=250, family="Bebas Neue", color="#C1C1C1", hjust=-.5),
     plot.subtitle = element_textbox(size=45, family="Roboto", color="#C1C1C1", hjust=0, width = unit(.9, "npc"), lineheight=.3, margin = margin(t=-10, b=30)),
     plot.background = element_rect(fill="#1B1B1B", color=NA),
     plot.caption = element_markdown(size=30, family="Roboto", color="#C1C1C1", hjust=1, lineheight=.1),
     panel.background = element_blank(),
     legend.position="none",
     panel.grid = element_blank(),
     axis.ticks = element_blank(),
     axis.title.y = element_blank(),
     axis.title.x = element_text(size=50, color="#C1C1C1", family="Bebas Neue"),
     axis.text.y = element_text(size=50, color="#C1C1C1", family="Bebas Neue"),
     axis.text.x = element_text(size=50, color="#C1C1C1", family="Roboto")
   )+
  coord_cartesian(clip="off")

  ggsave(plot = p, filename = "temp_ggrid.png",  width=10, height=10, units='in', dpi=300, bg="#1B1B1B")