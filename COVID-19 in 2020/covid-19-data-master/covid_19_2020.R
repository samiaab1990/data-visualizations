library(tidyverse)
library(ggplot2)
library(gganimate)
library(maps)
library(usmap)
library(mapproj)
library(ggdark)
library(lubridate)
library(transformr)
memory.limit(size=50000)


covid_19_2020<-read.csv("us-counties.csv") %>%
  mutate(date=as.Date(date)
  )


countypop<-countypop %>% mutate(fips=as.numeric(fips)) %>% 
  select(fips, pop_2015)

# Merge
covid_19_2020_pop<-covid_19_2020 %>% 
  mutate(month=month(date)) %>%
  select(date, fips, cases) %>%
  merge(countypop, by="fips")%>%
  mutate(case_rate=(cases/fips)*100,000) %>%
  select(date,fips,case_rate)%>%
  mutate(week=epiweek(date),
         date_range=paste0(floor_date(date, unit="week")," to ",ceiling_date(date,unit="week")-1))

# Create a weekly average dataset
covid_19_2020_pop_week<-
  covid_19_2020_pop %>%
  group_by(date_range,week,fips) %>%
  summarise(avg_case=mean(case_rate))

# Check antijoin
fips_antijoin<-anti_join(covid_19_2020, countypop, by="fips")

nrow(fips_antijoin) # Unjoined entries 

# Most unjoined entries are NA fips aggregates of states
# Also unjoined are Virgin Islands, Puerto Rico and Northern Mariana Islands 

fips_antijoin %>% filter(!is.na(fips)) %>% distinct(state)

county_fips<-data.frame(urbnmapr::counties) %>%
  mutate(fips=as.numeric(county_fips))

covid_19_week_map_data<-left_join(covid_19_2020_pop_week, county_fips, by = "fips")
#Quantile breaks for gradient scale
breaks<- quantile(covid_19_2020_pop_week$avg_case, probs=seq(0,1,.25), na.rm=TRUE) %>% unname() %>% round(digits=1)


g<-
  ggplot()+
  geom_polygon(data=covid_19_week_map_data,aes(long, lat, group = group, fill = avg_case), color=alpha("#8F8F8F",.3))+
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  theme_void()+
  scale_fill_viridis_c(option="inferno", direction=-1, trans="pseudo_log",
                       limits=c(min(breaks), max(breaks)),breaks=breaks[c(1,3:5)], labels=breaks[c(1,3:5)], na.value="grey")+
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
                               label.theme = element_text(color="black", angle=45, size=10, margin=margin(t=10))))+
  ggtitle('COVID-19 Cases',
          subtitle='Epi Week:{frame} of {nframes}\n Dates:"{unique(covid_19_2020_pop_week$covid_19_2020_pop_week$week==(frame_time)])}"')+
  #Allows for vaccine column to display as a subtitle relative to plot animation
  theme(legend.position="bottom", legend.justification=c(.8,0),
        plot.title=element_text(face="bold", size=14, color="#262626",hjust=.5),
        plot.subtitle=element_text(hjust=.5))+transition_time(week)

animate(g, nframes=49, fps=1)