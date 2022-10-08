library(dplyr)
library(ggplot2)
library(randomcoloR)
library(geofacet)
library(stringr)
library(ggtext)

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
group_by(county, year) %>%
summarise(avg_rent = mean(price, na.rm=TRUE))%>%
filter(!is.na(county),county!="santa cruz")

#joined_tables<-left_join(rent_summary,new_construction_summary)

custom_grid<-sf_bay_area_counties_grid1 %>%
mutate(name = str_trim(str_remove(tolower(name),"county|count"))) 


p<-ggplot(data=rent_summary, aes(x=year, y=avg_rent))+
geom_text(aes(x=2010, y=500, label=county, color=county), alpha=.04, size=30, fontface="bold", family="Bebas Neue")+
geom_segment(aes(x=year, xend=year, y=0, yend=avg_rent-2, color=county), linetype="dotted")+
geom_point(aes(color=county, alpha=avg_rent, size=avg_rent))+
facet_geo(~county, grid=custom_grid)+
scale_color_manual(values=pal)+
scale_x_continuous(breaks = c(2000,2009,2018),labels = c("2000","2009","2018"))+
scale_y_continuous(limits=c(0,5000), breaks=c(0,5000))+
theme(
  legend.position = "none",
  plot.background = element_rect(fill="#04001A"),
  plot.title = element_text(family="Bebas Neue", size=180, color="#CDCDCD", lineheight=.1),
  plot.subtitle = element_markdown(family="Montserrat", size=50, color="#CDCDCD", lineheight=.2, margin=margin(b=30)),
  plot.caption = element_markdown(family="Montserrat", size=30, color="#CDCDCD", lineheight=.2),
  panel.background = element_blank(),
  panel.grid.minor= element_blank(),
  panel.grid.major=element_blank(),
  strip.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_text(color="#6A6A6A", size=30, family="Montserrat"),
  strip.text = element_blank(),
)+
labs(title = "Bay Area Rental Prices Through The Years",
     subtitle = "The SF Bay Area has been known to have one of the highest housing rental costs throughout the country.<br> In 2018, the average monthly rent in San Francisco County was <b>more than double the monthly rent</b> in 2000.<br> The chart below shows the changes in average monthly rent (in dollars) throughout the region, inclusive<br> of all housing types (single and multifamily units).",
     caption = "<b>Source:</b> Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018.<br><b>Github:</b>samiaab1990")

ggsave(plot = p, filename = "sf_rent.png",  width=13, height=10, units='in', dpi=300)
