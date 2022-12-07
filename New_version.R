rm(list=ls())

#version2
### File for electricity costs
setwd("~/Wohnen/Unterlagen WOhnung Degerloch/Strom")
base.dir <- getwd()

## Laden von externen Informationen erst nach dem Festlegen der Ordnerstruktur
source(paste0(base.dir,"/","Energy_parameters.R"))

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

excel_sheets("Gas_Strom_berechnung_ver4.xlsx")
Energy_data_raw <- read_excel("Gas_Strom_berechnung_ver4.xlsx", sheet="Sheet1")
str(Energy_data_raw)
head(Energy_data_raw)

# Stromverbrauch
Only_strom <-Energy_data_raw %>% filter(Gas_oder_Strom=="Strom")
Only_strom <- Only_strom %>% arrange(Datum)
Only_strom_V <- Only_strom[-c(5,8,11),]

#estimate missing data 14.07.2022
mis <- Only_strom_V[c(6,7),]
((21218-20905)/2)+20905
mis <- c("2022-07-15", 21061.5,"Strom","Tristan,Choye,Jiyoung")

Only_strom_V<-rbind(Only_strom_V,mis) %>% arrange(Datum)
Only_strom_V <- Only_strom_V %>% mutate(Zaehlerstand=as.numeric(Zaehlerstand))

# Grafik zum Verbrauch von Strom
Only_strom_V <- Only_strom_V %>% mutate(kwh_monthly =Zaehlerstand-lag(Zaehlerstand), time_diff=Datum-lag(Datum))

dates <- c("NA", "1.Feb-17.Feb", "17.Feb-15.Mar", "15.Mar.-13.Apr", 
           "13.Apr.-14.Mai", "14.Mai-20.Jun.","20.Jun-14.Jul","14.Jul-14.Aug",
           "14.Aug-14.Sept","14.Sept-14.Okt","14.Okt-14.Nov")

Only_strom_V <- cbind(Only_strom_V, dates)

#Assuring right order in the graph
Only_strom_V$dates <- factor(Only_strom_V$dates, levels = Only_strom_V$dates)

  #Verbrauchsgraph_Strom
  graph_V <- Only_strom_V[-1,] 
  ggplot(graph_V%>%select(dates,kwh_monthly), aes(dates,kwh_monthly))+
  geom_col()
##############################################################################################
## Cost Calculation ################################################
  
before_july <- Only_strom_V[c(2:6),]

before_july <- before_july %>% mutate(kwh_cost=kwh_monthly*Verbrauchspreis_bis1July/100,
                                      Grund_cost=as.numeric(time_diff)*Grundpreis_Strom_brutto/(365/12))
  
# mischkalkulation June  
Only_strom
july <- Only_strom[c(7,8),]
july <- july %>% mutate(kwh_monthly=Zaehlerstand-lag(Zaehlerstand), 
                        time_diff=Datum-lag(Datum))


july <- july %>% mutate(kwh_cost=kwh_monthly*Verbrauchspreis_bis1July/100,
                                      Grund_cost=as.numeric(time_diff)*
                                      Grundpreis_Strom_brutto/(365/12))

july2 <- Only_strom[c(8),]
july3 <- Only_strom_V[7,]
july3 <- july3 %>% select(-c(kwh_monthly,time_diff,dates))

july2 <- rbind(july2, july3)

july2 <- july2 %>% mutate(kwh_monthly=Zaehlerstand-lag(Zaehlerstand), 
                        time_diff=Datum-lag(Datum))


july2 <- july2 %>% mutate(kwh_cost=kwh_monthly*Verbrauchspreis_bis1July/100,
                        Grund_cost=as.numeric(time_diff)*
                          Grundpreis_Strom_brutto/(365/12))
str(july)
str(july2)

july <- rbind(july, july2)
july <- july[-c(1,3),]

first_p<-july %>% summarise(kwh_monthly=sum(kwh_monthly), time_diff=sum(time_diff),
                   kwh_cost=sum(kwh_cost), Grund_cost=sum(Grund_cost))

second_part<-july %>% select(1:4)
second_part<-second_part[-1,]

july <- cbind(second_part,first_p)
july <- cbind(july, dates="20.Jun-14.Jul")

before_august<-rbind(before_july, july)

rm(july, july2, july3,second_part, first_p, before_july)

### Continue with before August ###########################
before_august

after_august <- Only_strom_V[c(8:11),]

after_august <- after_august %>% mutate(kwh_cost=kwh_monthly*Verbrauchspreis_abJuly/100,
                          Grund_cost=as.numeric(time_diff)*
                            Grundpreis_Strom_brutto/(365/12))

strom_cost <- rbind(before_august,after_august)
strom_cost

# Calculate VAT and total cost
strom_cost <- strom_cost %>% mutate(VAT=0.19*(kwh_cost+Grund_cost)) %>%
                             mutate(Total_cost=VAT+kwh_cost+Grund_cost)

strom_graph <- strom_cost %>% select(dates, kwh_cost, Grund_cost, VAT)

stacked<-strom_graph %>% select(kwh_cost)%>% pivot_longer(kwh_cost)
stacked2<-strom_graph %>% select(Grund_cost)%>% pivot_longer(Grund_cost)
stacked3<-strom_graph %>% select(VAT)%>% pivot_longer(VAT)

dates_graph <- strom_graph %>% select(dates)
dates_graph<-  rbind(dates_graph, dates_graph,dates_graph)

strom_graph <- rbind(stacked, stacked2, stacked3)
strom_graph <- cbind(dates_graph, strom_graph)


#Assuring right order in the graph
strom_graph$dates <- factor(strom_graph$dates, levels = strom_graph$dates)


# Stacked
ggplot(strom_graph, aes(fill=name, y=value, x=dates)) + 
  geom_bar(position="stack", stat="identity")+geom_hline(yintercept = 125)+geom_hline(yintercept = 143)







