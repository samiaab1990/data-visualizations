library(tidyverse)
library(geomtextpath)
library(ggtext)
library(cowplot)
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
# font for title
sysfonts::font_add_google("Space Mono","Space Mono")
sysfonts::font_add_google("Roboto Condensed","Roboto Condensed", bold.wt=700)
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/fonts/Font Awesome 5 Free-Solid-900.otf")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

tuesdata <- tidytuesdayR::tt_load('2023-03-14')
tuesdata <- tidytuesdayR::tt_load(2023, week = 11)

drugs <- tuesdata$drugs

min(drugs$marketing_authorisation_date, na.rm=TRUE)
max(drugs$marketing_authorisation_date, na.rm=TRUE)

drugs_sum<-drugs %>%
filter(category=="human") %>%
separate_rows(therapeutic_area, sep=";") %>%
mutate(therapeutic_area = toupper(trimws(therapeutic_area)),
       year = lubridate::year(marketing_authorisation_date),
       year_cat = case_when(
         year >= 1995 & year <= 1999 ~ "1995<br>1999",
         year >= 2000 & year <= 2004 ~ "2000<br>2004",
         year >= 2005 & year <= 2009 ~ "2005<br>2009",
         year >= 2010 & year <= 2014 ~ "2010<br>2014",
         year >= 2015 & year <= 2019 ~ "2015<br>2019",
         year >= 2020 ~ "2020<br>PRESENT",
         TRUE ~ NA_character_
         
       )) %>% 
filter(!is.na(year_cat)) %>% 
group_by(year_cat, therapeutic_area) %>%
summarize(counts=n()) %>%
slice_max(order_by=counts, n=5) %>%
mutate(percent = counts/sum(counts),
       ymax = cumsum(percent),
       ymin = lag(ymax, default=0, n=1),
       lab_position = (ymax+ymin)/2,
       therapeutic_area = case_when(
         str_detect(therapeutic_area,",") ~ str_replace_all(therapeutic_area,",","<br>"),
         str_detect(therapeutic_area, "PERIPHERAL VASCULAR") ~ "PERIPHERAL <br> VASCULAR DISEASES",
         str_detect(therapeutic_area, "MYOCARDIAL INFARCTION") ~ "MYOCARDIAL<br>INFARCTION",
         str_detect(therapeutic_area, "ALZHEIMER") ~ "ALZHEIMER<br>DISEASE",
         therapeutic_area == "BREAST NEOPLASMS" ~ "BREAST<br>NEOPLASMS",
         therapeutic_area == "PROSTATIC NEOPLASMS" ~ "PROSTATIC<br>NEOPLASMS",
         therapeutic_area == "PARKINSON DISEASE" ~ "PARKINSON<br>DISEASE",
         therapeutic_area == "COVID-19 VIRUS INFECTION" ~ "COVID-19<br> VIRUS INFECTION",
         TRUE ~ therapeutic_area
       ))


pal1<-c("#F0E3D0","#FDF0DF","#B3A898")
pal2<-c("#F6B5D3","#F6CFDD","#C78AAC")
pal3<-c("#8EC3E3","#C0EEFF","#68818A")
pal4<-c("#E3E3E3","#FFFFFF","#707070")
pal5<-c("#C4B1DE","#EAD8FF","#8F849C")
pal6<-c("#EBE980","#FFFD9C","#8A8854")



make_tablet<-function(time_frame, tablet_color, offset_color, text_color)
{
tablet_plot<-ggplot()+
geom_rect(data = drugs_sum %>% filter(year_cat == time_frame), aes(xmin=0, xmax=3, ymin=0, ymax=1), fill=tablet_color, color=NA)+
geom_rect(data = drugs_sum %>% filter(year_cat == time_frame), aes(xmin=2.5,xmax=4, ymin=ymin, ymax=ymax), fill=tablet_color, color=offset_color, linewidth=5)+
geom_textsegment(data = drugs_sum %>% filter(year_cat == time_frame), aes(x=3.25, xend=3.25, y=ymin, yend=ymax,label=paste0(therapeutic_area,"<br>",round(percent * 100, 1),"%")), linewidth=NA, color=text_color,  family="Space Mono", rich=TRUE, spacing=50, lineheight=1, size=3, fontface="bold")+
coord_polar(theta="y")+
xlim(c(0, 4))+
ylim(c(0,1))+
theme_void()

markings_plot<-ggplot()+
geom_richtext(data = drugs_sum %>% filter(year_cat == time_frame), aes(x=-.05,y=0, label=year_cat), label.color=NA, fill=NA, lineheight=1.6, hjust=.5, size=10, family="Space Mono", color=text_color, fontface="bold")+
xlim(c(-.5,.5))+
ylim(c(-.5,.5))+
theme_void()+
coord_fixed()

line_segment<-ggplot()+
  geom_segment(aes(x=-.5, xend=.5, y=0, yend=0), linewidth=5, color=offset_color)+
  xlim(c(-.5,.5))+
  ylim(c(-.5,.5))+
  theme_void()+
  coord_fixed()

p<-ggdraw() +
  draw_plot(tablet_plot) +
  draw_plot(line_segment, width=.55, x=.22)+
  draw_plot(markings_plot, width=.5, height=.8, x=.25, y=.1)

return(p)
}

tablet_grid<-tibble(
  time_frame = unique(drugs_sum$year_cat),
  tablet_color = c(pal1[1], pal2[1], pal3[1], pal4[1], pal5[1], pal6[1]),
  offset_color = c(pal1[2], pal2[2], pal3[2], pal4[2], pal5[2], pal6[2]),
  text_color = c(pal1[3], pal2[3], pal3[3], pal4[3], pal5[3], pal6[3])
)

all_tablets<-tablet_grid %>% 
pmap(make_tablet)

main_plot<-ggplot()+
geom_textbox(aes(x=7.5,y=13, label="Common Therapeutic Classes of Authorized Drugs in Europe Over the Years"), family="Space Mono", size=15, fontface="bold", box.color=NA, fill=NA, width=unit(1,"npc"), lineheight=0, color="#494949")+
geom_textbox(aes(x=6.7,y=11.8, label="An exploration of <b> 28 years </b> of human drug development trends in Europe. Each chart below shows <b>the percent of authorizations for each therapeutic class</b> over the total authorizations for the <b> 5 most common classes </b> of approved drugs in every 5 year period since 1995."), family="Roboto Condensed", size=6,  box.color=NA, fill=NA, width=unit(.9,"npc"), lineheight=.5, color="#494949")+
geom_textbox(aes(x=11.8,y=0, label="<b>Source:</b> European Medicines Agency <b>Viz By:</b> Samia B (<span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span> samiaab1990)</span>"), family="Space Mono", size=4,  box.color=NA, fill=NA, width=unit(.9,"npc"), lineheight=0, color="#494949")+
xlim(c(0,15))+
ylim(c(0,13))+
theme_void()+
theme(plot.background = element_rect(fil="#FFFFFF", color=NA),
      panel.background = element_rect(fill=NA, color=NA))+
coord_fixed(clip="off")

tablet_layout<-plot_grid(plotlist=all_tablets, nrow=2)

complete_plot<-ggdraw()+
draw_plot(main_plot, y=-.03)+
draw_plot(tablet_layout, height=.85)



ggsave(filename = "drug_development_part_to_whole.png", 
       plot=complete_plot, 
       width= 5 * 3, 
       height=13, 
       dpi=300,
       units="in")