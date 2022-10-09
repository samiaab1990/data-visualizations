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
library(igraph)
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

top_4<-tech %>% 
select(id,upvotes) %>%
distinct(.keep_all=TRUE) %>%
arrange(desc(upvotes)) %>%
slice(1:4)

color_ids<-top_4 %>%
pull(id) %>%
paste(collapse="|")

ggraph_setup<- tech %>% 
  
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


# convert to graph object
ggraph<- as_tbl_graph(ggraph_setup)


# custom color pal
# n<-n_distinct(ggraph_setup$id)
# pal <- distinctColorPalette(n)

google_pal<-c("#4285F4","#DB4437","#F4B400","#0F9D58")

p<-ggraph(ggraph, layout = "sphere") + 
  geom_edge_diagonal(color="#474747", show.legend=FALSE, edge_alpha=0.1)+ 
  geom_edge_diagonal(aes(filter=grepl(color_ids,id), color=id), show.legend=FALSE, edge_alpha=.8)+
  scale_edge_color_manual(values=google_pal)+
  geom_node_point(aes(size = name), fill = NA, color="#D4D4D4", alpha=.3, show.legend=FALSE)+
  theme_graph(background = "#1E1E1E")+
  geom_node_text(aes(label = name), size=9, color = "#D4D4D4", repel=TRUE, family="Gemunu Libre")+
  coord_fixed()+
  labs(title = "<span style = 'color:#4285F4'>G</span><span style='color:#DB4437'>o</span><span style='color:#F4B400'>o</span><span style='color:#4285F4'>g</span><span style='color:#0F9D58'>l</span><span style='color:#DB4437'>e</span> Product Tags",
       subtitle = paste0("Tags associated with <b>",n," Google products</b> listed on Product Hunt, a social network site for sharing and discovering new products. Tags associated with the <b>four most upvoted products</b> are highlighted below."),
       caption = "<b>Source:</b> components.one by way of Data is Plural<br><b>Github:</b>samiaab1990")+
  theme(
    plot.title = element_markdown(hjust=.5, size=150, color="#D4D4D4", family="Fjalla One"),
    plot.subtitle = element_textbox(width = unit(.9, "npc"), size=60, family="Gemunu Libre", hjust=.5, color="#D4D4D4", lineheight=.1),
    plot.caption = element_markdown(size=30, color = "#D4D4D4", family="Gemunu Libre", hjust=1, lineheight=.1)
  )

ggsave(plot = p, filename = "network_graph_bezier_sphere2.png",  width=10, height=10, units='in', dpi=300)

