# libraries 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggraph)
library(ggtext)
library(tidygraph)
library(purrr)
library(lubridate)
library(Cairo)
library(randomcoloR)
CairoWin()

# title font
sysfonts::font_add_google("Fjalla One","Fjalla One")

# font for the subtitle, caption, nodes
sysfonts::font_add_google("Gemunu Libre","Gemunu Libre")
showtext::showtext_auto()

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-10-04')
tuesdata <- tidytuesdayR::tt_load(2022, week = 40)

product_hunt <- tuesdata$product_hunt


tech<-product_hunt %>% 
      mutate(year = year(as.Date(release_date))) %>% 
      filter(str_detect(category_tags,"GOOGLE")) %>%
      separate_rows(category_tags, sep=",") %>%
      mutate(category_tags = str_trim(str_remove_all(category_tags,"[:punct:]"))) 

  
ggraph_setup<- tech %>% 
               filter(category_tags != "GOOGLE") %>% 
  
# Using ID as a tag to find combination pairs of category_tags for network mapping
# reference: https://stackoverflow.com/questions/66471448/how-to-iterate-column-values-to-find-out-all-possible-combinations-in-r
  
               nest(dataset=-id) %>% 
               mutate(dataset=map(dataset, function(dataset){
                                           if(nrow(dataset)>1){
                                            dataset %>% .$category_tags %>% combn(., 2) %>% t() %>% as_tibble(.name_repair=~c("to", "from")) %>% return()
                                            }
                                            else{
                                            return(NULL)
                                            }
                                            })) %>% 
                unnest(cols=dataset) 


# conveert to graph object
ggraph<- as_tbl_graph(ggraph_setup)

# custom color pal
n<-n_distinct(ggraph_setup$id)
pal <- distinctColorPalette(n)

p<-ggraph(ggraph) + 
  geom_edge_link(aes(color=id), show.legend=FALSE, edge_alpha=0.2) + 
  scale_color_manual(values=pal)+
  geom_node_point(size = 2, fill = NA, color="#D4D4D4", alpha=.3)+
  theme_graph(background = "#1E1E1E")+
  geom_node_text(aes(label = name), size=9, color = "#D4D4D4", repel=TRUE, family="Gemunu Libre")+
  coord_fixed()+
  labs(title = "<span style = 'color:#4285F4'>G</span><span style='color:#DB4437'>o</span><span style='color:#F4B400'>o</span><span style='color:#4285F4'>g</span><span style='color:#0F9D58'>l</span><span style='color:#DB4437'>e</span> Product Tags",
       subtitle = paste0("Tags associated with <b>",n," Google products</b> listed on Product Hunt, a social network site for sharing and discovering new products."),
       caption = "<b>Source:</b> components.one by way of Data is Plural<br><b>Github:</b>samiaab1990")+
  theme(
    plot.title = element_markdown(hjust=.5, size=150, color="#D4D4D4", family="Fjalla One"),
    plot.subtitle = element_textbox(width = unit(.9, "npc"), size=70, family="Gemunu Libre", hjust=.5, color="#D4D4D4", lineheight=.1),
    plot.caption = element_markdown(size=30, color = "#D4D4D4", family="Gemunu Libre", hjust=1, lineheight=.1)
  )

ggsave(plot = p, filename = "network_graph.png",  width=10, height=10, units='in', dpi=300)

