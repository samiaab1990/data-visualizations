library(tidytuesdayR)
library(tidyverse)
library(waffle)
library(Cairo)
library(extrafont)
library(ggtext)
library(cowplot)
CairoWin()


inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventories_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
colors<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
inventory_parts<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
parts<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/parts.csv.gz')

dat<-inventory_parts %>%
merge(colors, by.x = "color_id", by.y = "id") %>%
merge(inventories, by.x = "inventory_id", by.y="id") %>%
merge(sets, by.x = "set_num", by.y = "set_num")


color_summary<-dat %>%
                
                mutate(name.x = ifelse(str_detect(name.x,"Any"), "Black", name.x)) %>%
                mutate(color_id = ifelse(color_id == 9999, 0, color_id)) %>% 
                mutate(decade = case_when(
                  year < 1950 ~ 0,
                  year >=1950 & year < 1960 ~ 1,
                  year >=1960 & year < 1970 ~ 2,
                  year >=1970 & year < 1980 ~ 3,
                  year >=1980 & year < 1990 ~ 4,
                  year >=1990 & year < 2000 ~ 5,
                  year >=2000 & year < 2010 ~ 6,
                  year >=2010 & year < 2020 ~ 7,
                  year >=2020 ~ 8,
                  )) %>%
                  group_by(decade, color_id, name.x, rgb) %>%
                  summarise(counts=n()) %>%
                  ungroup() %>%
                  group_by(decade) %>%
                  mutate(percent = round(counts/sum(counts)*100),
                         rgb = paste0("#",rgb)) %>%
                  arrange(desc(percent)) %>% 
                  slice(1:10)


decade_labels<-c(`0` = "Before 1950s", `1` = "1950s", `2` = "1960s", `3` = "1970s", `4` = "1980s", `5` = "1990s", `6` = "2000s", `7` = "2010s", `8` = "2020 to Present")

caption<-(1:9) %>%
map(function(i)
paste0("<b>",decade_labels[i],":</b> ",paste(color_summary %>% filter(decade==i-1) %>% pull(name.x), collapse = ', '),"<br>")) %>% paste(collapse='')

p<-ggplot(color_summary, aes(fill = rgb, values = percent))+
  geom_waffle(
    n_rows = 10, size = 0.33, colour = "#DEDEDE",  make_proportional = TRUE
  )+
  facet_wrap(~decade, labeller = labeller(decade = decade_labels))+
  coord_equal()+
  scale_fill_identity()+
  theme_void()+
  labs(title = "LEGOS IN <span style='color: #C91A09'>C</span><span style='color:#237841'>O</span><span style='color:#F2CD37'>L</span><span style='color:#0055BF'>O</span><span style='color:#9BA19D'>R</span>",
       subtitle = "<b>The <span style = 'color: #C91A09'>10 most popular colors</span> in Lego sets each decade</b>\n",
       caption = paste0(caption,"<br><br><br><span style='font-size:15px;'><b style='color:#237841; font-family:Legothick; font-size:20px'>SOURCE</b> rebrickable <b style='color:#0055BF; font-family:Legothick; font-size:20px'>GITHUB</b> samiaab1990</span>")) +
  theme(legend.position='none',
        plot.background = element_rect(fill="#E7E7E7", color=NA),
        strip.text = element_text(size=15, family='Quicksand'),
        plot.title = element_markdown(size=75, family="Legothick", hjust=.5),
        plot.subtitle = element_markdown(size=15, family='Quicksand', hjust = .5, margin=margin(10,0,10,0)),
        plot.caption = element_markdown(size=10, family='Quicksand', hjust=0, margin=margin(10,0,20,0)),
        panel.background = element_rect(color=NA),
        panel.border = element_rect(color='black', fill=NA),
        panel.spacing = unit(1,"lines"))


ggsave(p, filename = '~/GitHub/DataViz/Legos/legos_test.png', dpi = 200, type = 'cairo',
       width = 10, height = 10, units = 'in')
