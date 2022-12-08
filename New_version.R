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

# Stacked
ggplot(strom_graph, aes(fill=name, y=value, x=dates)) + 
  geom_bar(position="stack", stat="identity")+geom_hline(yintercept = 125)+geom_hline(yintercept = 143)

rm(stacked, stacked2, stacked3)

strom_cost %>% select(Total_cost, dates)
strom_cost %>% select(Total_cost, dates) %>% summarise(mean(Total_cost))

# Durchschnittsverbrauch liegt bei 135 € per month 

######################################################################################
################### GAS VERBRAUCH ###############################
################################################################

# Gasverbrauch
Only_gas <-Energy_data_raw %>% filter(Gas_oder_Strom=="Gas")
Only_gas <- Only_gas %>% arrange(Datum)

Only_gas_V <- Only_gas[-c(5,7),]

#estimate missing data 14.07.2022 and 20.06.2022
mis <- Only_gas_V[c(5,6),]
per_month <-((8529-8491)/3)

mis <- c("2022-06-15", per_month+8491 ,"Gas","Tristan,Choye,Jiyoung")
mis2 <- c("2022-07-15", per_month*2+8491,"Gas","Tristan,Choye,Jiyoung")

Only_gas_V<-rbind(Only_gas_V,mis, mis2) %>% arrange(Datum)
Only_gas_V <- Only_gas_V %>% mutate(Zaehlerstand=as.numeric(Zaehlerstand))

# Grafik zum Verbrauch von Strom
Only_gas_V<-Only_gas_V[-10,]

Only_gas_V <- Only_gas_V %>% mutate(m3_monthly =Zaehlerstand-lag(Zaehlerstand), 
                             time_diff=Datum-lag(Datum))

dates_gas <- c("NA", "02.Jan-12.Feb", "12.Feb-15.Mar","15.Mar.-13.Apr", 
           "13.Apr.-14.Mai", "14.Mai-14.Jun.","14.Jun-14.Jul","14.Jul-14.Aug",
           "14.Aug-14.Sept","14.Sept-14.Okt","14.Okt-14.Nov")

Only_gas_V <- cbind(Only_gas_V, dates_gas)

#Assuring right order in the graph
Only_gas_V$dates <- factor(Only_gas_V$dates, levels = Only_gas_V$dates)

#Verbrauchsgraph_Strom
graph_gV <- Only_gas_V[-1,] 
ggplot(graph_gV%>%select(dates,m3_monthly), aes(dates,m3_monthly))+
  geom_col()

#######################################################################
######### Gas cost calculation #######################################
######################################################################

# #To Do's 1. different costs from abrechnung eprimo -da gabs abweichungen
#          2. erster October Rückung der VAT von 19 of 7%
#          3. ab erster october gasspeicherumlage und andere umlage
#          4. Im Dezember werden die Gas_kosten übernommen, bzw der abschlag
#          5. 50€ savings from choye additional


# Same pricing until 1. okt

before_october<-Only_gas_V[c(2:9),]

before_october<- before_october %>% mutate(kwh=m3_monthly*Brennwert*Zustandszahl) %>% 
                   mutate(Kwh_cost=kwh*Arbeitspreis_Gas, Grund_cost=Grundpreis_Gas_brutto/(365/12)*as.numeric(time_diff)) %>%
                   mutate(CO2_tax_cost= CO2_tax*kwh) %>% mutate(kwh_cost_co2=Kwh_cost+CO2_tax_cost)

before_october <- before_october %>% mutate(VAT=0.19*(kwh_cost_co2+Grund_cost)) %>% mutate(total_cost=VAT+kwh_cost_co2+Grund_cost)
before_october %>% select(dates_gas, total_cost)
before_october %>% select(dates_gas, total_cost) %>% summarise(mean(total_cost))

# until september we have a mean cost of 108.38 €/month

## Mischkalkulation für oktober
october<-Only_gas[c(9:11),]
october

october <- october %>% mutate(m3_monthly=Zaehlerstand-lag(Zaehlerstand), 
                        time_diff=Datum-lag(Datum))


october <- october %>% mutate(kwh=m3_monthly*Zustandszahl*Brennwert)
                              
october_old_price <- october[2,]                          
october_new_price <- october[3,]

october_old_price<- october_old_price %>% mutate(kwh=m3_monthly*Brennwert*Zustandszahl) %>% 
  mutate(Kwh_cost=kwh*Arbeitspreis_Gas, Grund_cost=Grundpreis_Gas_brutto/(365/12)*as.numeric(time_diff)) %>%
  mutate(CO2_tax_cost= CO2_tax*kwh) %>% mutate(kwh_cost_co2=Kwh_cost+CO2_tax_cost)

october_old_price <- october_old_price %>% mutate(VAT=0.19*(kwh_cost_co2+Grund_cost)) %>% mutate(total_cost=VAT+kwh_cost_co2+Grund_cost)


october_new_price<- october_new_price %>% mutate(kwh=m3_monthly*Brennwert*Zustandszahl) %>% 
  mutate(Kwh_cost=kwh*Arbeitspreis_Gas, Grund_cost=Grundpreis_Gas_brutto/(365/12)*as.numeric(time_diff)) %>%
  mutate(Gas_spe_umlage=Gas_speicherumlage*kwh)%>%
  mutate(CO2_tax_cost= CO2_tax*kwh) %>% mutate(kwh_cost_co2=Kwh_cost+CO2_tax_cost)

october_new_price <- october_new_price %>% mutate(VAT=0.07*(kwh_cost_co2+Grund_cost+Gas_spe_umlage)) %>% mutate(total_cost=VAT+kwh_cost_co2+Grund_cost+Gas_spe_umlage)

### october combined
before_october <- before_october %>% select(m3_monthly, time_diff, dates_gas, kwh:total_cost)
str(before_october)

str(october_old_price)
october_old_price <- october_old_price %>% select(m3_monthly:total_cost)
str(october_new_price)
october_new_price <- october_new_price %>% select(m3_monthly:total_cost)

october_old_price<- october_old_price %>% mutate(Gas_spe_umlage=0)

october_finished<-rbind(october_old_price, october_new_price)
str(october_finished)


october_finished<-october_finished %>%summarise(m3_monthly=sum(m3_monthly), time_diff=sum(time_diff), 
                    kwh=sum(kwh), Grund_cost=sum(Grund_cost), CO2_tax_cost=sum(CO2_tax_cost),
                    Kwh_cost=sum(Kwh_cost), kwh_cost_co2=sum(kwh_cost_co2), VAT=sum(VAT),
                    total_cost=sum(total_cost), Gas_spe_umlage=sum(Gas_spe_umlage))


str(before_october)
str(october_finished)
Only_gas_V

october_finished<- mutate(october_finished,dates_gas="14.Sept-14.Okt")
before_october<- mutate(before_october, Gas_spe_umlage=0)

gas_cost<-rbind(before_october, october_finished)

#########
# Calculation for from ocotber onwards, coded in the way that it can continue
Only_gas_V

after_october <- Only_gas_V[-c(1:10),]
after_october <- after_october %>% select(m3_monthly:dates_gas)

after_october<- after_october %>% mutate(kwh=m3_monthly*Brennwert*Zustandszahl) %>% 
  mutate(Kwh_cost=kwh*Arbeitspreis_Gas, Grund_cost=Grundpreis_Gas_brutto/(365/12)*as.numeric(time_diff)) %>%
  mutate(CO2_tax_cost= CO2_tax*kwh) %>% mutate(kwh_cost_co2=Kwh_cost+CO2_tax_cost) %>% mutate(Gas_spe_umlage=kwh*Gas_speicherumlage)

after_october <- after_october %>% mutate(VAT=0.07*(kwh_cost_co2+Grund_cost+Gas_spe_umlage)) %>% mutate(total_cost=VAT+kwh_cost_co2+Grund_cost+Gas_spe_umlage)

str(gas_cost)
str(after_october)

gas_cost <- rbind(gas_cost, after_october)
gas_cost


#####################################################################################
#############################################################################

# create stacked barplot for gas_cost
str(gas_cost)
gas_graph <- gas_cost %>% select(dates_gas, kwh_cost_co2, Grund_cost, VAT, Gas_spe_umlage)

stacked<-gas_graph %>% select(kwh_cost_co2)%>% pivot_longer(kwh_cost_co2)
stacked2<-gas_graph %>% select(Grund_cost)%>% pivot_longer(Grund_cost)
stacked3<-gas_graph %>% select(VAT)%>% pivot_longer(VAT)
stacked4 <- gas_graph %>% select(Gas_spe_umlage)%>% pivot_longer(Gas_spe_umlage)

dates_graph <- gas_graph %>% select(dates_gas)

one <- dates_graph
two <- dates_graph
three <- dates_graph
four <- dates_graph

dates_graph<-  rbind(one, two, three, four) 
rm(one, two,three, four)

gas_graph <- rbind(stacked, stacked2, stacked3, stacked4)
rm(stacked, stacked2, stacked3, stacked4)
gas <- cbind(dates_graph, gas_graph)

#Assuring right order in the graph
#gas$dates_gas <- factor(gas$dates_gas, levels = gas$dates_gas)

dates_gas <- c("02.Jan-12.Feb", "12.Feb-15.Mar","15.Mar.-13.Apr", 
              "13.Apr.-14.Mai", "14.Mai-14.Jun.","14.Jun-14.Jul","14.Jul-14.Aug",
              "14.Aug-14.Sept","14.Sept-14.Okt","14.Okt-14.Nov")

p <- ggplot(theTable, aes(x = Position)) + scale_x_discrete(limits = positions)

# Stacked
ggplot(gas, aes(fill=name, y=value, x=dates_gas)) + 
  geom_bar(position="stack", stat="identity")+geom_hline(yintercept = 125)







