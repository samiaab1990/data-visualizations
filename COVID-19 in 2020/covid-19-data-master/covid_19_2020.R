library(tidyverse)
library(ggplot2)
library(gganimate)
library(maps)
library(mapproj)
library(usmap)
library(ggdark)
library(lubridate)
library(transformr)
memory.limit(size=10000000)

dir<-"~/GitHub/DataViz/COVID-19 in 2020/covid-19-data-master/"

covid_19_2020<-read.csv(paste0(dir,"us-counties.csv")) %>%
  mutate(date=as.Date(date)
  )

countypop<-countypop %>% mutate(fips=as.numeric(fips))

# Merge
covid_19_2020_pop<-covid_19_2020 %>% merge(
  countypop, by="fips"
)

# Check antijoin
fips_antijoin<-anti_join(covid_19_2020, countypop, by="fips")

nrow(fips_antijoin) # Unjoined entries 

# Most unjoined entries are NA fips aggregates of states
# Also unjoined are Virgin Islands, Puerto Rico and Northern Mariana Islands 

fips_antijoin %>% filter(!is.na(fips)) %>% distinct(state)

covid_19_2020_pop<-
  covid_19_2020_pop %>%
  mutate(
    case_rate=(cases/pop_2015)*100,000
  )

covid_19_2020_pop_avg<-
  covid_19_2020_pop %>% mutate(quarter=quarter(date,fiscal_start = 1)) %>%
  group_by(fips,quarter) %>%
  summarise(quarter_avg_rate=mean(case_rate))


#Quantile breaks for gradient scale
breaks<- quantile(covid_19_2020_pop_avg$quarter_avg_rate, probs=seq(0,1,.25), na.rm=TRUE) %>% unname() %>% round(digits=1)

# Test this in ggplot2
g<-plot_usmap(data=covid_19_2020_pop, color=alpha("black", .3), size=.1, values="case_rate")+
  theme_void()+
  scale_fill_viridis_c(option="inferno", direction=-1, trans="pseudo_log",
                       limits=c(min(breaks), max(breaks)),breaks=breaks[c(1,3:5)], labels=breaks[c(1,3:5)])+
  dark_theme_void()+
  theme(legend.position="bottom",
        legend.spacing.x = unit(0.5, 'cm'))+
  guides(fill = guide_colorbar(title="",
                               frame.colour = "black",
                               direction="horizontal",
                               label.position="top",
                               barwidth = 8,
                               barheight = 1, 
                               ticks=FALSE,
                               keywidth=15,
                               label.hjust = .5,
                               label.theme = element_text(color="white", angle=45, size=10, margin=margin(t=10))))+
  #Allows for vaccine column to display as a subtitle relative to plot animation
  theme(legend.position="bottom", legend.justification=c(.8,0),
        plot.title=element_text(face="bold", size=14, color="#262626",hjust=.5),
        plot.subtitle=element_text(hjust=.5))

g+transition_states(date)