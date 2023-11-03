# libraries 

library(tidyverse)
library(colorspace)
library(ggridges)
library(ggtext)
library(gradienttext)
library(geomtextpath)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load(2023, week = 43)
# font for the title 
sysfonts::font_add_google("Bebas Neue","Bebas Neue")

# font for the subtitle, caption, etc.
sysfonts::font_add_google("Roboto Condensed","Roboto Condensed", bold.wt=700)
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/fonts/Font Awesome 5 Free-Solid-900.otf")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# Get the data 
patient_risk_profiles <- tuesdata$patient_risk_profiles

# Transform the data so age group is a long variable 

age_groups<-str_subset(colnames(patient_risk_profiles), "age group")

age_abx<-patient_risk_profiles %>%
  pivot_longer(cols = age_groups,
               names_to = "age_groups",
               values_to = "vals") %>%
  filter(vals == 1) %>% 
  mutate(age_groups = str_remove_all(age_groups, "age group:|\\s"),
         age_cat = case_when(
           str_detect(age_groups,"^0-4$|^5-9$|^10-14$|^15-19$") ~ "0-19",
           str_detect(age_groups,"20-24|25-29|30-34|35-39|40-44|45-49|50-54|55-59|60-64") ~ "20-64",
           TRUE ~ "65+"
         )) %>%
  rename(hearing_loss_risk = `predicted risk of Sudden Hearing Loss, No congenital anomaly or middle or inner ear conditions`) %>%
  select(age_groups, age_cat, hearing_loss_risk, matches("antibiotics")) %>%
  # pivot antibiotics to longer column 
  pivot_longer(cols = matches("antibiotics"),
               names_to = "antibiotic",
               values_to = "exposure") %>%
  mutate(antibiotic = str_trim(str_remove_all(antibiotic, "Antibiotics|in prior year"), "both"),
         antibiotic = case_when(str_detect(antibiotic,"Glycopeptide") ~ "Glycopeptide/lipoglycopeptides",
                                TRUE ~ antibiotic))

# Summary graphs 
color_scheme = c("#FFBE2A","#FC466B","#3F5EFB","#2297FF")

pal<-colorRampPalette(color_scheme)

## Create a function for the mean and median 

mean_median_graph_fn<-function(measure)
{
  str<-substitute(measure)
  
  dataset<-age_abx %>%
    group_by(antibiotic, exposure) %>%
    summarise(hearing_loss_risk = measure(hearing_loss_risk) * 100) %>%
    ungroup() %>%
    group_by(antibiotic) %>% 
    summarise(diff = diff(hearing_loss_risk)) %>%
    mutate(antibiotic = fct_reorder(antibiotic, diff)) %>%
    arrange(diff) %>% 
    bind_cols(fill = pal(nrow(.))) %>%
    mutate(alpha = ifelse(row_number() %in% c(nrow(.)-1,nrow(.)), 1, .2))
  
  plot<-ggplot()+
    geom_col(data=dataset, aes(x=antibiotic, y=diff, fill=fill, alpha=alpha), color=NA, width=.9)+
    geom_richtext(data = dataset %>% tail(2), aes(x=antibiotic, y=diff-.5, label=antibiotic), angle=90, family="Roboto Condensed", fill=NA, label.color=NA, size=3.3, color=lighten("#333940", .8), hjust=1, vjust=.5)+
    geom_richtext(data = dataset %>% tail(2), aes(x=antibiotic, y=diff+.05, label=paste0(round(diff,2),"%")), family="Roboto Condensed", fill=NA, label.color=NA, size=3, color=lighten("#333940", .8), hjust=0, vjust=.5, angle=90)+
    geom_textbox(data = dataset, aes(x=4.5, y=.5, label=paste0(str_to_title(str)," Risk Difference (%) in Exposed vs. Unexposed, by Class")), family="Roboto Condensed", fill=NA, box.color=NA, size=4, color=lighten("#333940", .8), hjust=.5, width=unit(.5,"npc"), fontface="bold")+
    theme_void()+
    scale_fill_identity()+
    scale_alpha_identity()+
    coord_cartesian(clip="off")
  
  return(list(dataset,plot))
  
}


## For finding >= 75th percentile 

exposed_only<-age_abx %>% filter(exposure == 1)

quant<-quantile(exposed_only$hearing_loss_risk)

percentile_75<-exposed_only %>% 
mutate(higher_percentile = ifelse(hearing_loss_risk >= quant[4],"yes","no")) %>%
group_by(antibiotic, higher_percentile) %>%
summarise(counts=n()) %>%
mutate(percent = (counts/sum(counts) * 100)) %>%
filter(higher_percentile == "yes") %>%
arrange(percent) %>%
ungroup() %>% 
bind_cols(color = pal(nrow(.))) %>%
mutate(alpha = ifelse(row_number() %in% c(nrow(.)-1,nrow(.)), 1, .4))

# get the antibiotics that have results in summary stats to highlight in density graph 
select_abx<-c(mean_median_graph_fn(median)[[1]]  %>% tail(2) %>% pull(antibiotic) %>% as.character(),
mean_median_graph_fn(mean)[[1]] %>% tail(2) %>% pull(antibiotic) %>% as.character(),
percentile_75 %>% tail(2) %>% pull(antibiotic)) %>%
unique()

age_abx <- age_abx %>%
  mutate(color = ifelse(antibiotic %in% select_abx, lighten("#333940", .8), lighten("#333940", .3)))

# percentile graph 

percentile_75_graph<-ggplot()+
geom_point(data = percentile_75, aes(x=reorder(antibiotic,percent), y=1, size=percent, color=color, alpha=alpha))+
geom_richtext(data = percentile_75 %>% tail(2), aes(x=reorder(antibiotic,percent),y=1, label=round(percent,0)), label.color=NA, fill=NA, size=3, family="Roboto Condensed", fontface="bold", color=lighten("#333940", .8))+
geom_textpath(data = percentile_75 %>% tail(2), aes(x=reorder(antibiotic,percent),y=1.4, label=antibiotic), size=2.5, family="Roboto Condensed", fontface="bold", color=lighten("#333940", .8), angle=90, hjust=0)+
geom_textbox(data = percentile_75, aes(x=10,y=1.8, label="Percent Exposed with Risk >= 75th Percentile, by Class"), fill=NA, size=4, family="Roboto Condensed", fontface="bold", color=lighten("#333940", .8), hjust=.5, box.color=NA)+
ylim(c(0,2))+
coord_polar(start=90)+
theme_void()+
theme(
  legend.position = "none"
)+
scale_size(range = c(0, 10))+
scale_color_identity()+
scale_alpha_identity()


#darken("#495159", amount=.3) = base color 
#make_gradient(label = "Patient Risk", colors = c("#FFBE2A","#FC466B","#3F5EFB","#2297FF"))

#main plots 

#title gradient text
gradient1<-"<span style='color:#FFBE2A;'>P</span><span style='color:#FE9A3D;'>a</span><span style='color:#FD7551;'>t</span><span style='color:#FC5164;'>i</span><span style='color:#D64A87;'>e</span><span style='color:#9D52B3;'>n</span><span style='color:#6459DE;'>t</span>"
gradient2<-"<span style='color:#3C63FB;'>R</span><span style='color:#3374FC;'>i</span><span style='color:#2A85FD;'>s</span><span style='color:#2297FF;'>k</span>"

# main
main_plot<-ggplot()+
  xlim(0,10)+
  ylim(0,10)+
  theme_void()+
  theme(
    plot.background = element_blank(),
    panel.background = element_rect(fill="#333940")
  )

#title plot 
title<-ggplot()+
geom_textbox(aes(x=2.5,y=2.5,label=paste0("Visualizing ",gradient1)), family="Bebas Neue", box.color=NA, fill=NA, size=30, color=lighten("#333940", .3), width=unit(.8,"npc"), hjust=.5)+
geom_textbox(aes(x=4.5,y=1.8,label=paste0(gradient2," Profiles")), family="Bebas Neue", box.color=NA, fill=NA, size=30, color=lighten("#333940", .3), width=unit(.8,"npc"), hjust=.5)+
xlim(c(0,5))+
ylim(c(0,5))+
theme_void()


# subtitle plot 
subtitle<-ggplot()+
geom_textbox(aes(x=0,y=0,label="Exploring <b>Antibiotic Use</b> and <b>Risk of Hearing Loss</b> With Simulated Patient Data."), family="Roboto Condensed", fill=NA, size=9, color=lighten("#333940", .5), width=unit(5,"npc"), hjust=.5, halign=.5, box.color=NA, fontface="bold")+
theme_void()

# caption/text plot 
caption<-ggplot()+
geom_textbox(aes(x=0,y=0,label=paste0("Several antibiotic classes, namely <span style='color:",lighten("#333940", .8),"'><b>macrolides, aminoglycosides and glycopeptides</b></span> (with some evidence additionally pointing to <span style='color:",lighten("#333940", .8),"'><b>tetracyclines, fluoroquinolones, oxazilodones and sulfonamides</b></span>) are known to potentially cause hearing loss or other ototoxic side effects. This visualization uses
hundred rows of patient medical history features simulated with a real world data source to explore the relationship between antibiotic exposure and 1-year risk of hearing loss, expressed as a percent. <b>Note:</b> because of the size, nature and context of the dataset, this visualization is not relaying <i>causal</i> effects, but patterns observed in the data.")),
             box.color=NA, fill=NA, family="Roboto Condensed", size=6, width=unit(1,"npc"),color=lighten("#333940", .5))+
theme_void()


# legend plot 
legend<-ggplot()+
geom_density_ridges_gradient(data = age_abx %>% filter(exposure == 1 & antibiotic == "Tetracyclines"), aes(x=hearing_loss_risk, y=antibiotic, fill=stat(x)),color=lighten("#333940", .3))+
geom_density_ridges(data = age_abx %>% filter(exposure == 0 & antibiotic == "Tetracyclines"), aes(x=hearing_loss_risk, y=antibiotic),color=lighten("#333940", .9), alpha=0, linetype="dotted", size=.9)+
xlim(c(-.01,.02))+
geom_textbox(aes(x=.017, y=230, label="Distribution of Risk with Antibiotic Class Exposure"), fill=NA, box.color=NA, family="Roboto Condensed", width=unit(.5,"npc"), color=lighten("#333940", .9))+
geom_segment(aes(x=.0089,y=160,xend=.0064, yend=160), size=.8, color=lighten("#333940", .9))+
geom_textbox(aes(x=-.01, y=110, label="Distribution of Risk without Antibiotic Class Exposure"), fill=NA, box.color=NA, family="Roboto Condensed", width=unit(.5,"npc"), color=lighten("#333940", .9))+
geom_textbox(aes(x=.01, y=350, label="Key:"), fill=NA, box.color=NA, family="Roboto Condensed", width=unit(.5,"npc"), color=lighten("#333940", .9), fontface="bold", size=5)+
geom_segment(aes(x=-.0034,y=110,xend=-.0005, yend=110), size=.8, color=lighten("#333940", .9))+
scale_fill_gradientn(colours = c(color_scheme[1],color_scheme[2]))+
theme_void()+
theme(
  legend.position = "none",
  panel.background = element_rect(fill="transparent",color="transparent")
)+
coord_cartesian(clip="off")


# source annotation
annotation<-ggplot()+
  geom_textbox(aes(x=0,y=0,label="<b>Source</b>: RPharma <b>Data Viz By</b>: Samia B <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span> samiaab1990"), width=unit(.8,"npc"), family="Roboto Condensed", box.colour=NA, color=lighten("#333940", .3), fill=NA, hjust=.5, size=4)+
  theme_void()

# density plot 
p<-ggplot()+
geom_density_ridges_gradient(data = age_abx %>% filter(exposure == 1), aes(x= hearing_loss_risk, y=antibiotic, fill=stat(x)), color=lighten("#333940", .3), scale=1.7, size=.8)+
geom_density_ridges(data = age_abx %>% filter(exposure == 0), aes(x= hearing_loss_risk, y=antibiotic), fill="white", alpha=0, size = .9, color=lighten("#333940", .9), scale=1.7, linetype="dotted")+
geom_textbox(data = age_abx %>% filter(exposure == 0), aes(x = max(hearing_loss_risk), y=as.numeric(factor(antibiotic)) + .5, label=antibiotic, color=color), box.color=NA, fill=NA, family="Bebas Neue", size=6, hjust=.5, halign=.5, alpha=.5, width=unit(.4,"npc"))+
scale_x_continuous(labels = scales::percent, expand=c(0,0))+
scale_color_identity()+
theme_void()+
theme(
  axis.text.y = element_blank(),
  axis.text.x = element_text(family="Roboto Condensed", size=15, face="bold", hjust=.5, color=lighten("#333940", .3)),
  axis.title= element_blank(),
  axis.ticks.y = element_blank(),
  panel.grid = element_blank(),
  plot.background = element_blank(),
  panel.background = element_rect(fill="transparent", color=NA),
  legend.position="none"
)+
scale_fill_gradientn(colours = color_scheme)+
coord_cartesian(clip="off")

# full plot 
full_plot<-ggdraw()+
  draw_plot(main_plot)+ 
  draw_plot(title, x=-.1, y=.71, width=1.32, height=.5)+
  draw_plot(subtitle, x=0, y=.74, width=1, height=.2)+
  draw_plot(caption, x=.03, y=.64, width=.95, height=.25)+
  draw_plot(legend, x=.45, y=.66, width=.28, height=.04)+
  draw_plot(p, x=.01, y=.03, width=.7, height=.65)+
  draw_plot(percentile_75_graph, x=.685, y=.35, width=.3, height=.3)+
  draw_plot(mean_median_graph_fn(median)[[2]], x=.73, y=.30, width=.25, height=.1)+
  draw_plot(mean_median_graph_fn(mean)[[2]], x=.73, y=.15, width=.25, height=.1)+
  draw_plot(annotation, x=.58, y=-.045, width=1, height=.1)

ggsave(plot = full_plot, filename = "abx_hearing_loss_risk_viz.png",  width=12, height=15, units='in', dpi=300, bg="#333940")