---
title: "google_mob"
author: "Andrea Lado"
date: "9/3/2021"
output: html_document
---

A partir de la pandemia, Google ha liberado datos de movilidad para los países del mundo, desagregándolos en diferentes unidades territoriales dentro de los países. En este documento se toma la última actualización de los datos y se generan gráficos para la evolución temporal de la mobilidad por departamento, en el área metropolitana y para el total del país. 

```{r , echo=FALSE, warning=FALSE}
###### librerías ######
library(tidyverse)
library(plotly)
library(ggpubr)
library(dbplyr)
library(modelr)
library(xlsx)
library(writexl)
library(ggpubr)
library(kableExtra)

###### Cargo y guardo la base #####
gmr <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", na.strings=c("","NA"))%>%
  rename(
    retail_and_recreation=retail_and_recreation_percent_change_from_baseline,
    workplaces= workplaces_percent_change_from_baseline, 
    grocery_and_pharmacy= grocery_and_pharmacy_percent_change_from_baseline, 
    parks= parks_percent_change_from_baseline, 
    workplaces=workplaces_percent_change_from_baseline, 
    transit_stations= transit_stations_percent_change_from_baseline, 
    residential= residential_percent_change_from_baseline
  )%>%
  mutate(sub_region_1=as.character(sub_region_1), 
         metro_area= as.character(metro_area))%>%
  mutate(sub_region_1= replace_na(sub_region_1, "Total"), 
         metro_area= replace_na(metro_area, ""))%>%
  mutate(date=as.Date(date))

gmr$area <- paste(gmr$sub_region_1, gmr$metro_area)  

gmr_uy<- gmr%>%
  dplyr::filter(country_region_code=="UY")

#cuantos días hay en la nueva actualizacion
cantidad_dias<- gmr%>%
  dplyr::filter(country_region_code=="UY")%>%
  filter(area=="Total ")%>%
  count(unique(date))%>%
  summarise(sum(n))%>%
  pull()%>%
  print()



```

Los datos para Uruguay están desagregados por departamento, y también para área metropolitana de Montevideo. 
```{r, echo=FALSE, warning=FALSE}
###### datos ######
gmr%>%
  filter(country_region_code=="UY")%>%
  dplyr::filter(area=="Total ")%>%
  dplyr::filter(area!="Total Montevideo Metropolitan Area")%>%
  select(date,retail_and_recreation, workplaces, grocery_and_pharmacy)%>%
  kable(caption="Total UY") %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "200px")

gmr%>%
  filter(country_region_code=="UY")%>%
  dplyr::filter(area!="Total Montevideo Metropolitan Area")%>%
  select(area, date,retail_and_recreation, workplaces, grocery_and_pharmacy)%>%
  kable(caption="Por departamento") %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "200px")






```


```{r, echo=FALSE, warning=FALSE}
###### gráficos ######

retail_pordpto <- gmr%>%
  filter(country_region_code=="UY")%>%
  dplyr::filter(area!="Total ")%>%
  dplyr::filter(area!="Total Montevideo Metropolitan Area")%>%
  # dplyr::filter(date!= "2020-08-25",date!= "2020-08-24" )%>%
  ggplot(aes(x=date, y=retail_and_recreation))+
  geom_point() +
  stat_smooth()+
  ylim(-80, 10)+
  facet_wrap( ~area)+
  theme_classic()

workplaces_pordpto<- gmr%>%
  dplyr::filter(country_region_code=="UY")%>%
  dplyr::filter(area!="Total ")%>%
  dplyr::filter(area!="Total Montevideo Metropolitan Area")%>%
  # dplyr::filter(date!= "2020-08-25",date!= "2020-08-24" )%>%
  ggplot(aes(x=date, y=workplaces ))+
  geom_point() +
  stat_smooth()+
  ylim(-80, 10)+
  facet_wrap( ~area)+
  theme_classic()

ggplotly(retail_pordpto)
ggplotly(workplaces_pordpto)



workplaces_metro<- gmr%>%
  dplyr::filter(country_region_code=="UY")%>%
  dplyr::filter(area=="Total Montevideo Metropolitan Area")%>%
  # dplyr::filter(date!= "2020-08-25",date!= "2020-08-24" )%>%
  ggplot(aes(x=date, y=workplaces ))+
  geom_point() +
  stat_smooth()+
  ylim(-80, 10)+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_hline(yintercept=-50, linetype="dashed")+
  geom_hline(yintercept=-25, linetype="dashed")+
  facet_wrap( ~area)+
  theme_classic() 

ggplotly(workplaces_metro)


# 
retail_tot<-gmr_uy%>%
  filter(area=="Total ")%>%
  mutate(precovid=if_else(date<"2020-03-15", 1, 0))%>%
  # dplyr::filter(date!= "2020-12-25",date!= "2021-01-01" )%>%
  ggplot(aes(x=date, y=retail_and_recreation ,  
             colour = factor(precovid) ))+
  geom_point() + 
  stat_smooth()+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_hline(yintercept=-50, linetype="dashed")+
  geom_hline(yintercept=-25, linetype="dashed")+
  ylim(-80, 10)+
  theme_classic()



grocery_total <-gmr_uy%>%
  filter(area=="Total ")%>%
  # dplyr::filter(date!= "2020-08-25",date!= "2020-08-24" )%>%
  mutate(precovid=if_else(date<"2020-03-15", 1, 0))%>%
  ggplot(aes(x=date, y=grocery_and_pharmacy ,  
             colour = factor(precovid) ))+
  geom_point() +
  stat_smooth()+
  # stat_smooth(method = "lm", formula = y ~ log(x))+
  # stat_smooth(method = "lm", formula = y ~ x)+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_hline(yintercept=-50, linetype="dashed")+
  geom_hline(yintercept=-25, linetype="dashed")+
  ylim(-80, 10)+
  theme_classic()



workplace_total<- gmr_uy%>%
  filter(area=="Total ")%>%
  # dplyr::filter(date!= "2020-08-25",date!= "2020-08-24" )%>%
  mutate(precovid=if_else(date<"2020-03-15", 1, 0))%>%
  ggplot(aes(x=date, y=workplaces ,  
             colour = factor(precovid) ))+
  geom_point() + 
  # stat_smooth(method = "lm", formula = y ~ log(x))+
  stat_smooth()+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_hline(yintercept=-50, linetype="dashed")+
  geom_hline(yintercept=-25, linetype="dashed")+
  ylim(-80, 10)+
  theme_classic()

ggplotly(workplace_total) 
ggplotly(retail_tot)
ggplotly(grocery_total)



```
