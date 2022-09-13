# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-02-16')
tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

georgia_pop <- tuesdata$georgia_pop

# Or read in the data manually

georgia_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/census.csv')
furniture <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/furniture.csv')
city_rural <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/city_rural.csv')
income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/income.csv')
freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')
occupation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/occupation.csv')
conjugal <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/conjugal.csv')

library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(showtext)
library(pBrackets)

font_add("Vasarely", "Vasarely.otf")
showtext_auto()


georgia_pop_plot<-georgia_pop %>%
  pivot_longer(cols=c("Colored","White"), names_to="race_cat", values_to="pop") %>%
  mutate(num_year=Year,
         Year=as.character(Year)) %>% 
  ggplot()+
  geom_line(aes(x=Year, y=pop, group=race_cat, linetype=race_cat))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_reverse(breaks=seq(0,100, by=5), expand=c(0,0))+
  scale_linetype_manual(labels=c("Colored                                                                 ",
                                 "White"),values=c("solid", "longdash"))+
  labs(title = "Comparitive Increase of White and Colored\nPopulation of Georgia.",
       caption = "Github:@samiaab1990")+
  xlab("")+
  ylab("")+
  coord_flip()+
  theme(
    plot.background=element_rect(fill="#decdb1"),
    panel.background = element_rect(fill="#decdb1"),
    panel.grid.major=element_line(color="#c9a692", size=.5),
    panel.grid.minor=element_blank(),
    axis.text.x=element_text(size=25),
    axis.text.y=element_text(size=30),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    legend.title=element_blank(),
    legend.position="bottom",
    legend.key.width=unit(2,"cm"),
    axis.title.x = element_text(size=25, family="Vasarely",  margin(t=200, b=200)), 
    legend.text=element_text(size=30, family="Vasarely", margin=margin(r=10,l=-10, unit="pt")),
    legend.background=element_blank(),
    legend.key=element_rect(fill="#decdb1"),
    plot.title = element_text(family="Vasarely", size=55, lineheight=.3, hjust=.4, face="bold"),
    plot.caption = element_text(family="Vasarely", size=20, hjust=1)
)

png("GA_pop_plot.png", units="in", width=6, height=8, res=300)
grid.newpage()
georgia_pop_plot
grid.brackets(1790, 2160, 125, 2160, lwd=.90, curvature = .2)
grid.text("Percents", x=unit(8.1,"cm"), y=unit(.78,"cm"), gp=gpar(fontsize = 30, fontfamily="Vasarely"))
dev.off()

png("Freed_slaves_plot.png", units="in", width=9, height=10, res=300)
font_add("Vasarely","Vasarely.otf")
showtext_auto()
ggplot()+
  geom_area(data=freed_slaves, aes(x=Year, y=100), fill="#649568", color="#191b1a", outline.type = "full")+
  geom_area(data=freed_slaves, aes(x=Year, y=Slave), fill="#191b1a")+
  geom_text(data=freed_slaves[-nrow(freed_slaves),], aes(x=Year, y=Slave+1.5, label=paste0(Free,"%")), size=15, fontface="bold")+
  geom_text(data=freed_slaves[nrow(freed_slaves),], aes(x=Year, y=Slave+90, label=paste0(Free,"%")), size=15, fontface="bold")+
  geom_text(data=freed_slaves, aes(x=1830, y=70, label="SLAVES"), color="#e3dad1", size=30, family="Vasarely", fontface="bold")+
  geom_text(data=freed_slaves, aes(x=1830, y=66, label="ESCLAVES"), color="#e3dad1", size=30, family="Vasarely", fontface="bold")+
  geom_text(data=freed_slaves, aes(x=1830, y=96, label="FREE - LIBRE"), color="#191b1a", size=25, family="Vasarely", fontface="bold")+
  scale_x_continuous(position="top", breaks=c(freed_slaves %>% select(Year) %>% pull()),labels=c(freed_slaves %>% select(Year) %>% pull()))+
  xlab("")+
  labs(caption="Github:@samiaab1990")
  theme(
    plot.background = element_rect(fill="#D5CDC2"),
    plot.caption = element_text(family="Vasarely", size=20, hjust=1),
    panel.background = element_rect(fill="#D5CDC2"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y=element_blank(),
    axis.text.x.top = element_text(size=50,face="bold", margin=margin(b=-30, unit="pt"), color="#2D2C28"),
    axis.ticks.x=element_blank(),
    axis.ticks.y = element_blank()
  )
dev.off()  

