# libraries 
library(ggplot2)
library(dplyr)
library(patchwork)
library(forcats)
library(stringr)
library(ggtext)
library(ggchicklet)
library(sysfonts)

# title font
sysfonts::font_add_google("Sacramento","Sacramento")

# font for the subtitle, caption, etc.
sysfonts::font_add_google("Rajdhani","Rajdhani")
showtext::showtext_auto()

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-10-11')
tuesdata <- tidytuesdayR::tt_load(2022, week = 41)

yarn <- tuesdata$yarn

# find the top 20 most rated yarn brands by summarizing rating count
top_20_popular_yarn<-yarn %>%
group_by(yarn_company_name) %>%
summarise(counts = sum(rating_count, na.rm=TRUE)) %>%
arrange(desc(counts)) %>%
slice(1:20) %>%
mutate(yarn_company_name = case_when(str_detect(yarn_company_name,"Cascade")~ "Cascade Yarns",TRUE ~ yarn_company_name))


## Filter yarn dataset on 20 most popular yarns (to be used for plot2) 
top_20_popular_yarn_data<-yarn %>%
  mutate(yarn_company_name = case_when(str_detect(yarn_company_name,"Cascade")~ "Cascade Yarns",TRUE ~ yarn_company_name)) %>% 
  filter(yarn_company_name %in% (top_20_popular_yarn %>% pull(yarn_company_name))) 
  

## Groupings by yarn company and yarn weight
yarn_weight<-top_20_popular_yarn_data %>%
filter(yarn_company_name %in% (top_20_popular_yarn %>% pull(yarn_company_name)))%>%
group_by(yarn_company_name, yarn_weight_name, yarn_weight_ply, yarn_weight_knit_gauge) %>%
summarise(avg_rating = mean(rating_average, na.rm=TRUE)) %>%

## Merge for ordering variables in ggplot2 
merge(top_20_popular_yarn) %>% 
filter(!is.na(avg_rating), !is.na(yarn_weight_name)) %>%
mutate( yarn_weight_ply_char = ifelse(is.na(yarn_weight_ply),"Unavailable", as.character(yarn_weight_ply)),
        yarn_weight_knit_gauge = ifelse(is.na(yarn_weight_knit_gauge),"Unavailable", yarn_weight_knit_gauge),
        label = paste0("<b style = 'font-size:70px'>",yarn_weight_name,"</b><br><b>Ply: </b>", yarn_weight_ply_char,"<br><b>Knit Gauge: </b>",yarn_weight_knit_gauge))

## filter point that has highest rating count
max_rating<-top_20_popular_yarn_data %>% 
filter(rating_count == max(rating_count, na.rm=TRUE)) %>%
pull(name)

## filter point that has highest average rating 
highest_avg_rating<-yarn_weight %>% ungroup() %>% filter(avg_rating == max(avg_rating, na.rm=TRUE))


p<-ggplot(data = yarn_weight, aes(x=reorder(label,yarn_weight_ply), y=reorder(yarn_company_name,counts), size = avg_rating, color = avg_rating))+
geom_point()+
scale_y_discrete(position="right")+
geom_text(data = (yarn_weight %>% filter(avg_rating == highest_avg_rating$avg_rating)), aes(x=9.5, y=16, label="Lang Yarn's DK / Sport\nweighted yarn had\n the highest average\n rating"), color="#484848", lineheight=.3, size=12, family="Rajdhani", fontface="bold")+
geom_curve(data = (yarn_weight %>% filter(avg_rating == highest_avg_rating$avg_rating)), aes(x=9.5, y=16, xend=9, yend=4), arrow = arrow(length=unit(.02,"npc"), type="closed"), color="#484848", linetype="dotted", size=.5)+
labs(title = "Average Rating of Brands\n by Yarn Weight Type")+
theme(
  plot.background = element_rect(fill = "#FFFCF2"),
  plot.title = element_text(size=80, family="Sacramento", color="#484848", hjust=.5,lineheight=.3),
  legend.background = element_rect(fill = "#FFFCF2"),
  legend.key = element_rect(fill = "#FFFCF2"),
  legend.position = "bottom",
  legend.title = element_text(family="Rajdhani", size=40, face="bold", color="#484848"),
  legend.text = element_text(family="Rajdhani", size=40, color="#484848"),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_markdown(vjust=.5,family="Rajdhani", hjust=1, angle=90, size=35, color='#484848', lineheight=.2),
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  panel.grid.major = element_line(linetype="dotted", color="#9E9E9E")
)+
scale_color_gradientn(colors = c("#151e3f","#030027","#f2f3d9","#dc9e82","#c16e70"))+
guides(color=guide_legend(title = "Average Rating", title.position="top", title.hjust=.5), size = guide_legend(title = "Average Rating", title.position="top", title.hjust = .5))+
coord_cartesian(clip="off")


jitter_pal<-c("#0F3249", "#19364C", "#223A4F", "#2C3F52", "#354355", "#3F4758", "#484B5B", "#524F5E", "#5B5461", "#655864", "#6E5C67", "#78606A", "#81656D", "#8B6970", "#946D73", "#9E7176", "#A77579", "#B17A7C", "#BA7E7F", "#C48282")

q<-ggplot(top_20_popular_yarn_data, aes(x=reorder(yarn_company_name,counts), y=rating_count, color=reorder(yarn_company_name, counts)))+
  geom_jitter()+
  scale_x_discrete(position="bottom")+
  geom_text(data = (top_20_popular_yarn_data %>% filter(rating_count==max(rating_count,na.rm=TRUE))), aes(x=16, y=15000, label=paste0(max_rating,"yarns\nhad the greatest number \nof ratings on Ravelry")), size=12, face="bold", color="#484848", lineheight=.2, family="Rajdhani", fontface="bold")+
  geom_curve(data = (top_20_popular_yarn_data %>% filter(rating_count==max(rating_count,na.rm=TRUE))), aes(x=15, xend=19, y=15000, yend=rating_count-2), arrow = arrow(length = unit(0.02, "npc"), type="closed"), color="#484848", linetype="dotted")+
  scale_color_manual(values = jitter_pal)+
  labs(title="Number of Ratings by Brand")+
  theme(
    legend.position = "none",
    plot.title = element_text(size=80, family="Sacramento", color="#484848", hjust=.5, margin=margin(t=50, unit="pt")),
    plot.background = element_rect(fill = "#FFFCF2"),
    panel.background = element_blank(),
    panel.grid.major.y= element_line(linetype="dotted", color="#9E9E9E"),
    panel.grid.major.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x= element_text(size=50, family="Rajdhani", color="#484848", face="bold"),
    axis.text.y = element_text(size=50, family="Rajdhani", color='#484848', hjust=.5, face="bold")
  )+coord_flip(clip="off")

  r <- p + q + 
    plot_annotation(
      title = "Yarn Ratings", 
      subtitle = "Average ratings of yarn by brand and yarn weight type of <b>top 20 most rated brands</b>. Data comes from Ravelry, a social networking and organizational tool for knitters, crocheters, designers, spinners, weavers and dyers.", 
      caption = "<b>Source:</b> ravelry.com<br><b>Github:</b> samiaab1990",
      theme = theme(plot.title=element_text(size=250, hjust=.5, family="Sacramento", color="#484848"), plot.subtitle = element_textbox(family="Rajdhani", color="#484848", size=60, width=unit(.9,"npc"), hjust=.5,lineheight=.3), plot.caption = element_markdown(family="Rajdhani", color="#484848", size=35,lineheight=.3, hjust=1))) & theme(plot.background = element_rect(fill="#FFFCF2"))
  ggsave(plot = r, filename = "yarn.png",  width=15, height=10, units='in', dpi=300)
  
  
