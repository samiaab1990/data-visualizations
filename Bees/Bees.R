library(ggplot2)
library(tidyverse)
library(geofacet)
library(png)
library(ggimage)
library(Cairo)
library(extrafont)
memory.limit(24000)
CairoWin()

# read in data
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# load bee image 
bee_png<-"~/GitHub/DataViz/Bees/bee.png"

# summarize bee data by year
bee_dat<-merge(colony, stressor, by=c("year","months","state")) %>%
  filter(str_detect(months,"December")) %>% 
  group_by(year, state) %>%
  summarise(winter_pct = (colony_lost/colony_max)*100) %>%
  ungroup() %>% 
  mutate(image=ifelse(year==max(year), bee_png, NA))


hexagon<- data.frame(
  x = c(5.5, 11, 11, 5.5, 0, 0),
  y = c(50, 40, 0, -10, 0, 40)
)

loadfonts(device="win")

bee_plot<-bee_dat %>% 
ggplot()+
geom_polygon(data=hexagon, aes(x=x, y=y), color="#B88300", fill="#ffe49e", alpha=.5, size=3)+
geom_line(aes(x=as.factor(year), y=winter_pct, group=state),linetype="dotted", color='black',size=.8)+
geom_image(aes(x=as.factor(year+2), y=winter_pct+3, image=image, group=state), size=.4)+
geom_polygon(data=hexagon, aes(x=x, y=y), color="black", fill=NA, size=2)+
theme_minimal()+
expand_limits(x=c(1,10),
  y=c(0,max(bee_dat$winter_pct, na.rm=TRUE)+5))+
scale_x_discrete(breaks=c("2015","2016","2017","2018","2019","2020","2021"),
                 labels=c("2015","","","","","","2021"))+
scale_y_continuous(breaks=c(0,10,20,30,40))+
labs(caption="Source: USDA\nGithub: @samiaab1990")+
ggtitle("Bee Colonies in the Winter", 
  subtitle = "Percentages of bee colonies lost during from October to December \nby year between 2015 to 2021 in the United States.")+
facet_geo(~state, label="code")+
theme(
plot.title = element_text(size=100, face="bold", color="#835F07", family="Pacifico", hjust=.5, margin=margin(10,0,0,0)),
plot.subtitle = element_text(size=35, color="#7E5A01", family="Nunito ExtraBold", hjust=.5),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
strip.text = element_text(size=30, face="bold", color="#B88300", family="Nunito ExtraBold"),
axis.text.y = element_text(size=22, face="bold", color="#B88300", family="Nunito ExtraBold"),
axis.text.x = element_text(size=22, face="bold", color="#B88300", family="Nunito ExtraBold"),
axis.ticks.x = element_line(size=.5, color="black"),
axis.ticks.y = element_line(size=.5, color="black"),
axis.title = element_blank(),
plot.caption = element_text(size=22, face="bold", color="#B88300", family="Nunito ExtraBold"),
panel.spacing = unit(1,"lines")
#panel.spacing.y=unit(1,"lines")
)

ggsave(bee_plot, filename = '~/GitHub/DataViz/Bees/bees_with_cairo.png', dpi = 200, type = 'cairo',
       width = 24, height = 22, units = 'in')
          