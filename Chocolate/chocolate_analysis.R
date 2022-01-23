# libraries
library(tidyverse)
library(ggplot2)
library(ggdark)
library(geosphere)
library(sf)
library(randomcoloR)
library(Cairo)
library(extrafont)
CairoWin()

# read the csv file
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# from the ggplot package, get the coordinates of all the world countries 
world_coords<-map_data("world")

# for each manufacturer, find the distinct manufacturer-company location-country of bean origin link
## cleaning manually after checking merges in later steps 

chocolate_dat<-chocolate %>%
  select(company_manufacturer, company_location, country_of_bean_origin) %>%
  distinct() %>%
  mutate(across(c("company_location","country_of_bean_origin"), ~str_remove_all(.x,'\\.')),
         
          company_location = case_when(
          company_location=="UAE" ~ "United Arab Emirates",
          company_location=="Wales"|company_location=="Scotland" ~ "UK",
          company_location=="Amsterdam" ~ "Netherlands",
          company_location=="St Lucia" ~ "Saint Lucia",
          str_detect(company_location,"Sao Tome")|str_detect(company_location,"Principe")~ "Sao Tome and Principe",
          str_detect(company_location,"Vincent") ~ "Saint Vincent",
          company_location=="Burma" ~ "Myanmar",
          str_detect(company_location,"Congo") ~ "Democratic Republic of the Congo",
          company_location=="Sumatra" | company_location=="Sulawesi" ~ "Indonesia",
          TRUE ~ company_location), 
          
         country_of_bean_origin=case_when(
             country_of_bean_origin=="UAE" ~ "United Arab Emirates",
             country_of_bean_origin=="Wales"|country_of_bean_origin=="Scotland" ~ "UK",
             country_of_bean_origin=="Amsterdam" ~ "Netherlands",
             country_of_bean_origin=="St Lucia" ~ "Saint Lucia",
             str_detect(country_of_bean_origin,"Sao Tome")|str_detect(country_of_bean_origin,"Principe") ~ "Sao Tome and Principe",
             str_detect(country_of_bean_origin,"Vincent") ~ "Saint Vincent",
             country_of_bean_origin=="Burma" ~ "Myanmar",
             str_detect(country_of_bean_origin,"Congo") ~ "Democratic Republic of the Congo",
             country_of_bean_origin=="Sumatra" | country_of_bean_origin=="Sulawesi" ~ "Indonesia",
             TRUE ~ country_of_bean_origin))

# find centroids (average of world coordinates by region)
centroids<-world_coords %>% 
           group_by(region) %>% 
           mutate(centroid =c(long,lat))
           filter(region %in% c(chocolate_dat$company_location,chocolate_dat$country_of_bean_origin)) %>%
           mutate(long = ifelse(region=='USA',long+20,long),
                  lat = ifelse (region=='USA',lat-10,lat))
 
# get lat long for each company manufacturer & check na
company_xy<-chocolate_dat %>% 
  left_join(centroids, by=c("company_location"="region")) %>%
  rename(long_company=long, lat_company=lat) 

check_na<-company_xy %>% filter(is.na(long_company)) %>% distinct(company_location)

# get lat long for each country of bean origin & check na
origin_xy<-chocolate_dat %>% 
  left_join(centroids, by=c("country_of_bean_origin"="region")) %>%
  rename(long_origin=long, lat_origin=lat) %>%
  filter(!is.na(long_origin))

check_na<-origin_xy %>% filter(is.na(long_origin)) %>% distinct(country_of_bean_origin)

# create a data frame with both origin & manufacturer lat & long
map_dat<-merge(company_xy,origin_xy)

dat <- data.frame()

loadfonts(device="win")

for(i in 1:nrow(map_dat)){

diff <-abs(map_dat$long_company[i]-map_dat$long_origin[i])
inter<-gcIntermediate(c(map_dat$long_company[i], map_dat$lat_company[i]), c(map_dat$long_origin[i], map_dat$lat_origin[i]), addStartEnd = TRUE) %>% 
  as.data.frame() %>%
  mutate(country_location=map_dat$company_location[i],
         country_origin = map_dat$country_of_bean_origin[i])

if(diff<180){
inter <- inter %>% 
  mutate(group = as.character(i))
}
else {
inter<-inter %>% mutate(group = ifelse(lon>0, paste0(as.character(i),"a"),paste0(as.character(i),"b")))
}

dat<-bind_rows(dat, inter)
}

n<-dat %>% select(country_origin) %>% n_distinct()
pal <- distinctColorPalette(n)


p<-ggplot()+
  geom_map(data = world_coords, map = world_coords,
  aes(long, lat, map_id = region), size=.1, color=NA, fill="#252525")+
  geom_point(data=centroids, aes(x=long, y=lat), size=.3, alpha=.5, color="white")+
  geom_path(data=dat, aes(x=lon, y=lat, group=group, color=country_origin),alpha=.2, size=.2)+
  labs(title="Where Have You Bean", 
       subtitle="The path various chocolate beans are traveled from country of origin to location of manufacturer.",
       caption="Source: Flavors of Cacao\nGithub:@samiaab1990")+
  scale_color_manual(values=pal)+
  dark_theme_void()+
  theme(legend.position="none",
        plot.title=element_text(size=25, face="bold", family="Segoe UI"),
        plot.subtitle=element_text(size=20, family="Segoe UI"),
        plot.caption=element_text(size=15, face="bold", family="Segoe UI"))


ggsave(p, filename = '~/GitHub/DataViz/Chocolate/choc_plot.png', dpi = 300, type = 'cairo',
       width = 17, height = 10, units = 'in')