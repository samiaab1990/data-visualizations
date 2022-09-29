# libraries
library(ggplot2)
library(dplyr)
library(Cairo)
library(extrafont)
library(ggtext)
CairoWin()

# Get the data
tuesdata <- tidytuesdayR::tt_load('2022-09-27')
tuesdata <- tidytuesdayR::tt_load(2022, week = 39)
artists <- tuesdata$artists



artists_grouped<-artists %>% 
# create var where location quotient is > 1 or not 
mutate(location_quotient_1 = ifelse(location_quotient <= 1, 0, 1)) %>%
# group by occupation and race
group_by(type, race) %>%
# create a percent location quotient which sums the total number of states > 1
# over total n (by type and race)
summarise(perc_loc_quot = round(sum(location_quotient_1, na.rm=TRUE)/n()*100,2)) %>%
# x labs 
mutate(x_labs = paste0("<img src='paintbrush.png' width='300' height='300'/><br>", race))

# x and y labels 
labels<-unique(as.factor(artists_grouped$x_labs))
ylabels<-paste0(seq(0,50,10),"%")

# custom palette values 
pal<-c("#BE5952", "#C67A02", "#C6A617" ,"#778D14", "#1A8400", "#019558", "#029585", "#0092AA", "#0092D6","#6168CA", "#A359C3", "#CF53B8", "#DE5E99")


p<-ggplot(data = artists_grouped, aes(x=as.factor(race), y=perc_loc_quot, fill=type))+
geom_col(position="dodge")+
scale_x_discrete(name=NULL, labels = labels)+
scale_y_continuous(expand = c(0, 0), limits=c(0,50), labels = ylabels)+
guides(fill = guide_legend(ncol=1, label.position = "right", title = element_blank()))+
scale_fill_manual(values=pal)+
  labs(title = "Calling All Artists",
       subtitle = "The Arts Data Profile, from The American Community Survey, gives national and state-level estimates<br>of artists in the workforce. State-level estimates are available for the total number of artists and for<br>individual artists in <b>13 specific occupations</b>. The concentration of each artist in the state's labor force<br>relative to the US labor force is measured by a <b>location quotient (LQ)</b>, where a location quotient above<br> 1 indicates a greater share than the national labor force.The following chart shows the <b>share\n of states <br>(as a percentage)</b> where the labor quotient was higher than 1 for <b>each occupation</b> by <b>race</b>.",
       caption = "<b>Source</b>: ACS<br><b>Github</b>: samiaab1990<br><b>Brush vector</b>: Mehwish via Flaticon")+
theme(
  plot.background = element_rect(fill="#F0EBB2"),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  legend.position = "right",
  legend.justification = "bottom",
  legend.background = element_blank(),
  legend.text.align = 0,
  legend.text = element_text(family="Bebas Neue", size=20, hjust=.5, color="#6A526A"),
  plot.title = element_text(family="Moving Skate", size=200, hjust=.5, color="#6A526A"),
  plot.subtitle = element_markdown(family="Patrick Hand", size=22, hjust=.5, color="#6A526A"),
  plot.caption = element_markdown(family="Patrick Hand", size=15, hjust=1, color="#6A526A"),
  axis.text.x= element_markdown(size=25, family = "Bebas Neue", color="#6A526A"),
  axis.text.y = element_text(size=25, family="Patrick Hand", color="#6A526A", face="bold")
  
)


ggsave(p, filename = '~/GitHub/DataViz/Artists/artists_test.png', dpi = 200, type = 'cairo',
       width = 20, height = 15, units = 'in')


