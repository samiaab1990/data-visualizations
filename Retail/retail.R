library(dplyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(ggnewscale)
library(cowplot)
library(statebins)
library(stringr)
library(ggtext)
library(ggchicklet)

# font for the title 
sysfonts::font_add_google("Archivo Narrow","Archivo Narrow", bold.wt=600)

# font for the subtitle, caption, etc.
sysfonts::font_add_google("Roboto Condensed","Roboto Condensed", bold.wt=700)
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/fonts/Font Awesome 5 Free-Solid-900.otf")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# Get the Data

# Read in with tidytuesdayR package
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-12-13')
tuesdata <- tidytuesdayR::tt_load(2022, week = 50)

state_retail <- tuesdata$state_retail
coverage_codes <- tuesdata$coverage_codes

# get the US retail data for all subsectors (remove total)
# filter:
# total US, year 2020
# mutate:
## change numeric values to numeric
## comebine year and month
## get month as a label 
us_retail<-state_retail %>%
filter(state_abbr=="USA", subsector!="total", year==2020) %>%
mutate(ym = as.Date(paste0(year,"-",month,"-01")),
       across(c(change_yoy,change_yoy_se),as.numeric),
       month_lab = month(ym, label=TRUE, abbr=TRUE))

# for the state retail statebin chart
state_retail_mod<-
  state_retail %>%
  filter(state_abbr!="USA", year==2020, subsector!="total") %>%
  mutate(across(c(change_yoy,change_yoy_se),as.numeric)) %>%
  group_by(state_abbr, subsector) %>%
  summarise(change_yoy=mean(change_yoy,na.rm=TRUE))

# to get the US mean averages
us_total_change<-us_retail %>%
                group_by(subsector) %>%
                summarise(change_yoy=mean(change_yoy,na.rm=TRUE))



custom_pal<-c("#1a1423","#372549","#774c60","#b75d69","#eacdc2",
              "#000000","#2f4550","#586f7c","#b8dbd9","#f4f4f9","#d6d3f0")

# main plot 
p<-ggplot(data = us_retail, aes(x = 0, y=change_yoy))+

# solution from https://stackoverflow.com/questions/9847559/conditionally-change-panel-background-with-facet-grid
geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill=ym), color=NA)+
scale_fill_gradient(low="#00424E",high="#001D22")+
guides(fill="none")+
new_scale_fill()+
geom_chicklet(data = us_retail,aes(fill=subsector),position="dodge", radius=unit(2,"pt"), color="#357783")+
geom_segment(aes(x=-Inf,xend=Inf, y=0,yend=0),  color="#D1D1D1", linewidth=.5)+
scale_y_continuous(limits=c(-100,100), labels=paste0(c(-100,-50,0,50,100),"%"))+
scale_fill_manual(values=custom_pal)+
geom_linerange(aes(ymin=change_yoy-change_yoy_se, ymax=change_yoy+change_yoy_se, group=subsector),
                position=position_dodge(.9), color="#FEFEFE", linetype="dashed",linewidth=.5, alpha=.5) +
facet_grid(~ym)+
geom_text(data = us_retail, aes(x=0, y=100, label=month_lab), family="Roboto Condensed", size=6, color="#FEFEFE")+
scale_color_manual(values=custom_pal)+
guides(fill=guide_legend(title="", nrow=6, override.aes = list(linetype=c(rep(0,11)))))+
theme(
      legend.position = c(.23,.8),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(family="Roboto Condensed", color="#D1D1D1", size=10),
      plot.background = element_rect(fill="#00424E"),
      panel.spacing = unit(c(0), "lines"),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(family="Roboto Condensed", size=11, color="#FEFEFE"),
      axis.ticks=element_blank(),
      axis.title = element_blank())+
coord_cartesian(clip="off")


# make a random dataset-will be used to generate annotations
# generating some annotations as separate plots to use ggtext
dat<-tibble(x=15,y=5)

# title 
q<-ggplot(data=dat,aes(x=x,y=y))+
   geom_text(aes(label="US Retail Sales in 2020"), size=20, family="Archivo Narrow", fontface="bold", color="#D1D1D1")+
   theme_void()

# subtitle 
r<-ggplot(data=dat, aes(x=x,y=y))+
   geom_textbox(aes(label="Retail sales in the United States, while initially increasing modestly across most sectors at the beginning of 2020 relative to the same time in 2019, underwent drastic changes after COVID-19 was declared a national emergency in March, though changes varied depending on sector. <b>Building materials and supplies</b> and<b> food and beverage</b> retailers overall had higher sales in 2020 compared to 2019, while<b> clothing</b>, <b>electronics and appliances</b> and <b>gasoline stations </b> had considerably lower sales. The following graph shows the <b>percent year-over-year change</b> during each month in 2020 across 11 sectors. Data on retail sales comes from the US Census Bureau's Monthly State Retail Sales data product that gathers data from survey, administrative data, and third-party data. <b>Note:</b> data collection for the MSRS may be limited in quality due to
                    collection during the pandemic, standard errors (represented by <span style='color:#FEFEFE'>----</span> dashed lines on the bar graph) are included to show the
                    possible interval of the true year-over-year estimates."), width=unit(.5,"npc"), family="Roboto Condensed", box.colour=NA, color="#D1D1D1", fill=NA, hjust=.5)+
   theme_void()

# annotation/title for maps 
s<-ggplot(data=dat, aes(x=x,y=y))+
  geom_textbox(aes(label=paste0("Retail sales for building materials and supplies dealers had a <b>",round(us_total_change %>% filter(str_detect(subsector,"Building")) %>% pull(change_yoy)),"%</b> mean year-over-year percent change in 2020 compared to 2019 whereas clothing retail sales had a mean change of <b>", round(us_total_change %>% filter(str_detect(subsector,"Clothing")) %>% pull(change_yoy)),"%</b> compared to 2019.")), width=unit(.4,"npc"), family="Roboto Condensed", box.colour=NA, color="#D1D1D1", fill=NA, hjust=.5)+
  theme_void()

# arrows 
arrow_1<-ggplot(data=dat, aes(x=x,y=y))+
         geom_curve(aes(x = x, xend = x+.5, y = y, yend=y+.5), curvature=-.5, arrow=arrow(length=unit(.05,"npc"), type="closed"), color="#D1D1D1")+
         ylim(5,6)+
         xlim(15,16)+
         theme_void()

arrow_2<-ggplot(data=dat, aes(x=x,y=y))+
  geom_curve(aes(x = 15.5, xend = 15.1, y = 5.5, yend=5), arrow=arrow(length=unit(.05,"npc"), type="closed"), color="#D1D1D1")+
  ylim(5,6)+
  xlim(15,16)+
  theme_void()

arrow_3<-ggplot(data=dat, aes(x=x,y=y))+
  geom_curve(aes(x = 15.5, xend = 15.1, y = 5.5, yend=5), curvature=.5, arrow=arrow(length=unit(.05,"npc"), type="closed"), color="#D1D1D1")+
  ylim(5,6)+
  xlim(15,16)+
  theme_void()

# caption 
caption <- ggplot(data=dat, aes(x=x,y=y))+
  geom_textbox(aes(label="<b>Source</b>: US Census Bureau <b>Data Viz By</b>: Samia B <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span> samiaab1990"), width=unit(.7,"npc"), family="Roboto Condensed", box.colour=NA, color="#D1D1D1", fill=NA, hjust=.5, size=3.5)+
  theme_void()

# statebins map 
t<-ggplot(data=state_retail_mod %>% filter(str_detect(subsector,"Building|Clothing")), aes(state=state_abbr, fill=change_yoy))+
geom_statebins(border_col="#357783", border_size=.5, family="Roboto Condensed", light_lbl="#D1D1D1", lbl_size=3, fontface="bold")+
coord_equal()+
scale_fill_gradient(high="#e1e5f2", low="#001427", na.value=NA)+
facet_wrap(~subsector, nrow=1, strip.position="bottom")+
theme_statebins(base_family="Roboto Condensed", base_size=13)+
theme(
  strip.background = element_blank(),
  strip.text = element_text(color="#D1D1D1", face="bold"),
  legend.position = c(.45,-.4),
  legend.background = element_blank(),
  legend.text = element_text(family="Roboto Condensed", color="#D1D1D1", size=11),
  legend.title = element_text(family="Roboto Condensed", color="#D1D1D1", size=10)
)+
guides(fill = guide_colorbar(ticks=FALSE, direction="horizontal", title.position="top", title="Change Year-over-Year(%)", barwidth=10, barheight=.5, title.hjust=.5, label.hjust=.5))


# use cowplot to put together and add additional annotations

annotate_1<-paste0("Clothing and clothing accessories\n retail sales dropped ",round(us_retail %>% filter(change_yoy==min(change_yoy)) %>% pull(change_yoy)),"% in April 2020,\n the most out of any sector in 2020.")
annotate_2<-"Building materials and supplies\n dealers and food and beverage were\n the only sectors that did not have a negative \nyear-over-year change in 2020."

u<-ggdraw() +
  draw_plot(p) +
  draw_plot(q, x = .2, y = .4, width = 1, height = .9)+
  draw_plot(r, x = .2, y = .27, width=1, height=.9)+
  draw_plot(s, x = .20, y=-.09, width=1, height=.9)+
  draw_plot(t, x = .20, y = -.23, width=1, height=.9, scale=.54)+
  draw_plot(arrow_1, x=.24, y=.07, width=.08, height=.08)+
  draw_plot(arrow_2, x=.21, y=.62, width=.08, height=.08)+
  draw_plot(arrow_3, x=.19, y=.551, width=.1, height=.15)+
  draw_plot(caption, x=.2,y=-.23, width=1, height=.5)+
  draw_label(annotate_1, x=.2, y=.05, size=10, fontfamily="Roboto Condensed", color="#D1D1D1")+
  draw_label(annotate_2, x=.32, y=.65, size=10, fontfamily="Roboto Condensed", color="#D1D1D1")

ggsave(plot = u, filename = "retail.png",  width=15, height=10, units='in', dpi=300, bg="#00424E")

## comment out-to test statebins
#ggsave(plot = s, filename = "statebin.png",  width=15, height=10, units='in', dpi=300, bg="transparent")
