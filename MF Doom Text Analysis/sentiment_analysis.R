# Packages 
library(genius)
library(tidyverse)
library(tidytext)
library(ggimage)
library(imager)
library(magick)
library(ggdark)
library(showtext)
library(gtable)
library(grid)

# Create a 2x2 table of artist and albums 
artist_albums<-tribble(
  ~artist, ~album,
  "Danger Doom", "The Mouse and the Mask",
  "Madvillain", "Madvillainy",
  "MF Doom", "MM..FOOD",
  "MF DOOM", "Operation:Doomsday"
)

# Read into add genius function
mf_doom<-artist_albums %>%
  add_genius(artist, album, type="album")

# Create dataset of unique words
# Remove stop words
# Filter words that appear only once 
# Summarize by track 
doom_unique_words<-mf_doom %>%
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words) %>% filter(!is.na(word)) %>% group_by(album,track_title,word) %>% count(word) %>%
  filter(n==1) %>%
  group_by(album,track_title, n) %>%
  summarise(number_of_unique_words=n()) %>%
  select(album,track_title,number_of_unique_words) 

# Images/Icons 
doom<-image_read("~/GitHub/DataViz/Text_Analysis/doom.png")
doom2<-png::readPNG("~/GitHub/DataViz/Text_Analysis/doom2.png") 
thmb<-image_scale(doom, "35")
image_write(thmb, path="~/GitHub/DataViz/Text_Analysis/doom_thmb.png")
doom_thmb<-"~/GitHub/DataViz/Text_Analysis/doom_thmb.png"

showtext.auto()

# GGplot 
g<-ggplot(data=doom_unique_words) +
  annotation_custom(rasterGrob(doom2), xmin=.5, xmax=4)+
  geom_image(aes(album, number_of_unique_words, image=doom_thmb), size=.05)+
  geom_text(data= . %>% group_by(album) %>% slice(which.max(number_of_unique_words)), aes(x=album, y=number_of_unique_words, label=track_title), size=6, fontface="bold",nudge_y=17)+
  dark_theme_gray(base_family = "Roboto", base_size = 25) + 
  theme(axis.title.y=element_text(face="bold", size=20),
        axis.text.y=element_text(face="bold", size=20),
        axis.text.x=element_text(face="bold", size=18),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))+
  xlab("")+
  ylab("Number of Unique Words")

# Save Graph
png("doom_graph.png", units="in", width=5, height=4, res=300)
g
dev.off()

