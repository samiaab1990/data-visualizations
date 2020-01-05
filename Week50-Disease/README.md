
#Diseases: Tidy Tuesday Week 50


```r
#packages
library(tidyverse)
library(gganimate)
library(ggplot2)
library(grid)
library(sf)
library(usmap)
library(scales)
```


## Read in the diseases CSV file 

```r
diseases<-read.csv(file="~/GitHub/TidyTuesday/data/2019/2019-12-10/diseases.csv")
```


## View the dataset

```r
diseases %>%nrow() #18870
```

```
## [1] 18870
```

```r
diseases %>%ncol() #6
```

```
## [1] 6
```

```r
diseases %>% colnames() #disease, count, state, population, year, weeks_reporting
```

```
## [1] "disease"         "state"           "year"            "weeks_reporting"
## [5] "count"           "population"
```

```r
head(diseases)
```

## Explore dataset and mutate
### See the unique diseases in the dataset
### Calculate incidence from count and population columns


```r
#See the unique diseases in the dataset
unique (diseases$disease)
```

```
## [1] Hepatitis A Measles     Mumps       Pertussis   Polio       Rubella     Smallpox   
## Levels: Hepatitis A Measles Mumps Pertussis Polio Rubella Smallpox
```

```r
#Calculate incidence 
diseases<- diseases %>% mutate(incidence=(count/population)*100000) 
```


## Filter measles from the dataset 

```r
#filter by measles/summary
diseases %>% filter(disease=="Measles") %>% summary()
```

```
##         disease            state           year      weeks_reporting     count       
##  Hepatitis A:   0   Alabama   :  76   Min.   :1928   Min.   : 0.00   Min.   :     0  
##  Measles    :3876   Alaska    :  76   1st Qu.:1947   1st Qu.:32.00   1st Qu.:     8  
##  Mumps      :   0   Arizona   :  76   Median :1966   Median :47.00   Median :   543  
##  Pertussis  :   0   Arkansas  :  76   Mean   :1966   Mean   :37.45   Mean   :  4817  
##  Polio      :   0   California:  76   3rd Qu.:1984   3rd Qu.:50.00   3rd Qu.:  3986  
##  Rubella    :   0   Colorado  :  76   Max.   :2003   Max.   :52.00   Max.   :132342  
##  Smallpox   :   0   (Other)   :3420                                                  
##    population         incidence        
##  Min.   :   86853   Min.   :   0.0000  
##  1st Qu.:  966309   1st Qu.:   0.3458  
##  Median : 2590843   Median :  27.7595  
##  Mean   : 3858493   Mean   : 166.6918  
##  3rd Qu.: 4696902   3rd Qu.: 226.1823  
##  Max.   :34861711   Max.   :2964.4269  
##  NA's   :64         NA's   :64
```

```r
#create a measles dataset
measles<-diseases %>% filter(disease=="Measles") 
measles<-measles %>% mutate (incidence_rounded = round(incidence, digits=1))
```

## Create a FIPS column using `fips()` from `usmap` package
## Create a new column to classify year by vaccine availability-will be used for subtitle in ggplot

```r
#Create a FIPS column using the fips() function from usmap 
measles$fips<-fips(measles$state)

#Create a column to classify year by vaccine availability
measles$vaccine<-ifelse((measles$year>=1963) & (measles$year<1989),2,
                 ifelse((measles$year>=1989),3,
                 ifelse((measles$year<1963), 1,1)))
measles$vaccine<-recode(measles$vaccine, '1'="",
                                     '2'="Vaccine introduced",
                                     '3'="Second dose recommended")
```

## Create the plot

```r
#Quantile breaks for gradient scale
breaks<- quantile(measles$incidence_rounded, probs=seq(0,1,.25), na.rm=TRUE) %>% unname() %>% round(digits=1)

#Color palette
gradient<-c("#B1C055","#6CC682","#39C2B6","#6BB4D7","#B69DD1","#E685A8","#ED7F72")

#Set these image quality options 
options(gganimate.dev_args = list(width = 6, height = 4, units = 'in', res=300))

#Create the plot
measles_plot<-plot_usmap(data=measles, color="#262626", size=.3, values="incidence_rounded")+
theme_void()+
scale_fill_gradientn(colors = gradient, trans="pseudo_log", 
#The pseudo_log allows for log transformation even though 0 is in the dataset
na.value="grey",limits=c(min(breaks), max(breaks)),breaks=breaks[c(1,3:5)], labels=breaks[c(1,3:5)])+
#only including min, 50th, 75th and max values  
guides(fill = guide_colorbar(title="",
                               frame.colour = "black",
                               label.position="top",
                               barwidth = 8,
                               barheight = 1, 
                               ticks=FALSE,
                               keywidth=15,
                               label.hjust = 0.5,
                               label.vjust = 0.3,
                    label.theme = element_text(angle = 45, size=10)))+
  labs(title = "Measles Incidence per 100,000 in {frame_time}",
       subtitle="{unique(measles$vaccine[measles$year==(frame_time)])}")+ 
       #Allows for vaccine column to display as a subtitle relative to plot animation
  theme(legend.position="bottom", legend.justification=c(.8,0),
        plot.title=element_text(face="bold", size=14, color="#262626",hjust=.5),
        plot.subtitle=element_text(hjust=.5))+
   transition_time(year)
```

## Save

```r
anim<-animate(measles_plot, nframes=76, fps=1)
#76 frames because 76 years 
anim_save("measlesmap.gif", anim)
```


## The Map
![alt text](~/Week50-Disease/measlesmap.gif)



