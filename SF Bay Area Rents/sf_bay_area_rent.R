library(dplyr)
library(ggplot2)
library(randomcoloR)
library(geofacet)
library(stringr)
library(ggtext)
library(ggnewscale)

sysfonts::font_add_google("Montserrat", "Montserrat")
sysfonts::font_add_google("Bungee", "Bungee")
sysfonts::font_add_google("Bebas Neue","Bebas Neue")
sysfonts::font_add_google("Unica One","Unica One")
showtext::showtext_auto()

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-07-05')
tuesdata <- tidytuesdayR::tt_load(2022, week = 27)

rent <- tuesdata$rent

# Or read in the data manually

rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')
permits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/sf_permits.csv')
new_construction <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/new_construction.csv')


# generate random pallete colors based on distinct country origin
n<-rent %>% filter(!is.na(county)) %>% select(county) %>% n_distinct()
pal <- distinctColorPalette(n)

rent_summary<-rent %>%
filter(is.na(baths) | baths == 1, beds <= 1) %>% 
group_by(county, year) %>%
summarise(avg_rent = mean(price, na.rm=TRUE))%>%
filter(!is.na(county),county!="santa cruz")

custom_grid<-sf_bay_area_counties_grid1 %>%
mutate(name = str_trim(str_remove(tolower(name),"county|count"))) 


p<-ggplot(data=rent_summary, aes(x=year, y=avg_rent))+
geom_text(aes(x=2010, y=100, label=county, color=county), alpha=.07, size=25, fontface="bold", family="Bebas Neue")+
geom_segment(aes(x=year, xend=year, y=0, yend=avg_rent-2, color=county), linetype="dotted")+
scale_color_manual(values=pal)+
geom_point(data = rent_summary, aes(x=year, y=avg_rent, color=county, size=avg_rent, alpha=avg_rent))+
geom_textbox(data = rent_summary %>% filter(county=="san francisco"), aes(x=2005, y=2400, label="avg rent in 2018<br>in sf county<br> was ~$2,773 <br> a <b>74% increase</b><br> from rent in 2000"), family="Monserrat", size=10, color="#CDCDCD", lineheight=.3, fill=NA, box.color=NA)+
geom_textbox(data = rent_summary %>% filter(county=="contra costa"), aes(x=2005, y=2400, label="avg rent in<br>contra costa county<br> was ~$1,904 <br> in 2017, a <b>158% increase</b><br> from rents in 2000"), family="Monserrat", size=10, color="#CDCDCD", lineheight=.3, fill=NA, box.color=NA)+
geom_curve(data = rent_summary %>% filter(county=="san francisco"), aes(x=2005, y=2000, xend = 2018, yend = 2773),
    arrow = arrow(length = unit(0.03, "npc"), type="closed"), color="#CDCDCD", linetype="dotted")+
geom_curve(data = rent_summary %>% filter(county=="contra costa"), aes(x=2005, y=2100, xend = 2017, yend = 1904),
             arrow = arrow(length = unit(0.03, "npc"), type="closed"), color="#CDCDCD", linetype="dotted", curvature = -0.3)+
facet_geo(~county, grid=custom_grid)+
scale_x_continuous(breaks = c(2000,2009,2018),labels = c("2000","2009","2018"))+
scale_y_continuous(limits=c(0,3300))+
theme(
  legend.position = "none",
  plot.margin = margin(t=-360),
  plot.background = element_rect(fill="#04001A"),
  plot.title = element_text(family="Bebas Neue", size=180, color="#CDCDCD", lineheight=.2, hjust=.5, vjust=-150),
  plot.subtitle = element_text(family="Montserrat", size=40, color="#CDCDCD", lineheight=.3, hjust=.5, vjust =-225),
  plot.caption = element_textbox(family="Montserrat", size=30, color="#CDCDCD", lineheight=.2, width=unit(.5, units="npc"), hjust=0),
  panel.background = element_blank(),
  panel.grid.minor= element_blank(),
  panel.grid.major=element_blank(),
  strip.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_text(color="#6A6A6A", size=30, family="Montserrat"),
  strip.text = element_blank(),
)+
labs(title = "Bay Area\nRental Prices\nThrough The\nYears",
     subtitle = "The SF Bay Area has been\n known to have one of\n the highest housing rental costs\n throughout the country. In 2018,\nthe average monthly rent in\nSan Francisco County was 74% higher\n than the monthly rent in 2000.\nThe graphs shows the changes\n in average monthly rent\n (in dollars) throughout the region for\n housing units with 1 or\nless beds and 1 baths.",
     caption = "<b>Source:</b> Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018.<br><b>Github: </b>samiaab1990")

ggsave(plot = p, filename = "sf_rent.png",  width=13, height=10, units='in', dpi=300)

