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
           "14.Aug-14.Sept","14.Sept-14.Okt","14.Okt-14.Nov", "14.Nov-14.Dez")

Only_strom_V <- cbind(Only_strom_V, dates)

#Assuring right order in the graph
Only_strom_V$dates <- factor(Only_strom_V$dates, levels = Only_strom_V$dates)

  #Verbrauchsgraph_Strom
  graph_V <- Only_strom_V[-1,] 
sv<-  ggplot(graph_V%>%select(dates,kwh_monthly), aes(dates,kwh_monthly))+
  geom_col()
sv
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

after_august <- Only_strom_V[c(8:12),]

after_august <- after_august %>% mutate(kwh_cost=kwh_monthly*Verbrauchspreis_abJuly/100,
                          Grund_cost=as.numeric(time_diff)*
                            Grundpreis_Strom_brutto/(365/12))

strom_cost <- rbind(before_august,after_august)
strom_cost

# Calculate VAT and total cost
strom_cost <- strom_cost %>% mutate(strom_tax=kwh_monthly*Stromsteuer)%>%
                             mutate(VAT=0.19*(kwh_cost+Grund_cost+strom_tax)) %>%
                             mutate(Total_cost=VAT+kwh_cost+Grund_cost)

strom_graph <- strom_cost %>% select(dates, kwh_cost, Grund_cost, VAT, strom_tax)

stacked<-strom_graph %>% select(kwh_cost)%>% pivot_longer(kwh_cost)
stacked2<-strom_graph %>% select(Grund_cost)%>% pivot_longer(Grund_cost)
stacked3<-strom_graph %>% select(VAT)%>% pivot_longer(VAT)
stacked4 <- strom_graph %>% select(strom_tax)%>% pivot_longer(strom_tax)

dates_graph <- strom_graph %>% select(dates)
dates_graph<-  rbind(dates_graph, dates_graph,dates_graph, dates_graph)

strom_graph <- rbind(stacked, stacked2, stacked3, stacked4)
strom_graph <- cbind(dates_graph, strom_graph)

# Stacked
# s1<-ggplot(strom_graph, aes(fill=name, y=value, x=dates)) + 
#   geom_bar(position="stack", stat="identity")+geom_hline(yintercept = 108)+geom_hline(yintercept = 143)
# s1

s1<-ggplot(strom_graph, aes(fill=name, y=value, x=dates)) + 
  geom_bar(position="stack", stat="identity")+ geom_segment(aes(x=0, xend=8, y=108, yend=108))+
  geom_segment(aes(x=8, xend=11.5, y=143, yend=143))
  
s1  
  

rm(stacked, stacked2, stacked3, stacked4)

strom_cost %>% select(Total_cost, dates)
strom_cost %>% select(Total_cost, dates) %>% summarise(mean(Total_cost))

# Durchschnittsverbrauch liegt bei 122.25 € per month 

# check calculations until August, in August we had to pay 333,xx for electricity
august<-strom_cost[c(1:7),]
august
str(august)
august$kwh_monthly
# forgot the strom tax
#august <- august %>% mutate(strom_tax=kwh_monthly*Stromsteuer)
#august <- august %>% mutate(VAT2=strom_tax*0.19 )
august

august %>% summarise(sum(strom_tax))
august <- august %>% mutate(totaltotal_cost= VAT+Grund_cost+kwh_cost+strom_tax)

total_august<-august %>% summarise(total_august=sum(sum(totaltotal_cost)))

total_august

august
###
# Zählersatnd bis 14.august: 21218-18250=2968
21218-18250
### from the bill 3194
1.19*((21218-18250)*(((Verbrauchspreis_bis1July*5/6+Verbrauchspreis_abJuly*1/6))/100)+Grundpreis_Strom_brutto*6+(21218-18250)*Stromsteuer)

1.19*((3194)*(((Verbrauchspreis_bis1July*5/6+Verbrauchspreis_abJuly*1/6))/100)+Grundpreis_Strom_brutto*6+(3194)*Stromsteuer)

# wieviel abschlag wurde bis august bezahlt, check excel file 648€


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
           "14.Aug-14.Sept","14.Sept-14.Okt","14.Okt-14.Nov", "14.Nov-14.Dez")

Only_gas_V <- cbind(Only_gas_V, dates_gas)

#Assuring right order in the graph
Only_gas_V$dates <- factor(Only_gas_V$dates, levels = Only_gas_V$dates)

#Verbrauchsgraph_gas
graph_gV <- Only_gas_V[-1,] 
gv<- ggplot(graph_gV%>%select(dates,m3_monthly), aes(dates,m3_monthly))+
  geom_col()
gv

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

gas_cost %>%summarise(sum(kwh))
ggplot(gas_cost, aes(x=dates_gas, y=kwh))+geom_col()
gas_cost %>% summarise(mean(sum(total_cost)/sum(kwh)))


strom_cost %>% summarise(sum(kwh_monthly))
ggplot(strom_cost, aes(x=dates, y=kwh_monthly))+geom_col()
strom_cost %>% summarise(mean(sum(Total_cost)/sum(kwh_monthly)))

# Average price for Gas is in total at: 0.154 ct/kwh
# Average price for eletricity is in total at: 0.290 ct/kwh

# judging from this analysis it is better to let the run in the hallway and reduce private room temperature

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

one$dates_gas <- factor(one$dates_gas, levels = one$dates_gas)
two$dates_gas <- factor(two$dates_gas, levels = two$dates_gas)
three$dates_gas <- factor(three$dates_gas, levels = three$dates_gas)
four$dates_gas <- factor(four$dates_gas, levels = four$dates_gas)


dates_graph<-  rbind(one, two, three, four) 
rm(one, two,three, four)

gas_graph <- rbind(stacked, stacked2, stacked3, stacked4)
rm(stacked, stacked2, stacked3, stacked4)
gas <- cbind(dates_graph, gas_graph)

# Stacked
g1 <-ggplot(gas, aes(fill=name, y=value, x=dates_gas)) + 
  geom_bar(position="stack", stat="identity") + geom_segment(x=0, xend=7, y=125, yend=125) + geom_segment(x=7, xend=11.5, y=116, yend=116)+
  geom_segment(x=7, xend=7, y=116, yend=125)
g1


###################################################################################################
## Creating a balance per person ####################################################
rm(october, october_finished, october_new_price, october_old_price)

# first the cost side for electricity before choye
strom_cost
before_choye<-Only_strom[c(1,5),]

before_choye <- before_choye %>%mutate(kwh_monthly=Zaehlerstand-lag(Zaehlerstand), 
                                       time_diff=Datum-lag(Datum))

before_choye <- before_choye %>% mutate(kwh_cost=kwh_monthly*Verbrauchspreis_bis1July/100,
                                 Grund_cost=as.numeric(time_diff)*
                                 Grundpreis_Strom_brutto/(365/12),
                                 strom_tax=kwh_monthly*Stromsteuer)

before_choye <- before_choye %>% mutate(VAT=0.19*(kwh_cost+Grund_cost+strom_tax)) %>% mutate(total_cost=kwh_cost+Grund_cost+VAT+strom_tax) %>%
                                 select(Gas_oder_Strom, kwh_monthly:total_cost)

before_choye <- before_choye %>% mutate(before_choye, dates="01.Feb.22-01.May.22", People_living_flat="Tristan,Hien,JiYoung")

before_choye<- before_choye[2,]
before_choye

# until price change in July
july<-Only_strom[c(5,8),]
july

july <- july %>%mutate(kwh_monthly=Zaehlerstand-lag(Zaehlerstand), 
                                       time_diff=Datum-lag(Datum))

july <- july %>% mutate(kwh_cost=kwh_monthly*Verbrauchspreis_bis1July/100,
                                        Grund_cost=as.numeric(time_diff)*
                                          Grundpreis_Strom_brutto/(365/12),
                                          strom_tax=Stromsteuer*kwh_monthly)

july <- july %>% mutate(VAT=0.19*(kwh_cost+Grund_cost+strom_tax)) %>% mutate(total_cost=kwh_cost+Grund_cost+VAT+strom_tax) %>%
                select(Gas_oder_Strom:total_cost)

july <- july %>% mutate(july, dates="01.May.22-01.July.22")

july<- july[2,]
july


# until dez
november <- Only_strom[c(8,14),]
november

november <- november %>%mutate(kwh_monthly=Zaehlerstand-lag(Zaehlerstand), 
                       time_diff=Datum-lag(Datum))

november <- november %>% mutate(kwh_cost=kwh_monthly*Verbrauchspreis_bis1July/100,
                        Grund_cost=as.numeric(time_diff)*
                          Grundpreis_Strom_brutto/(365/12),
                        strom_tax=Stromsteuer*kwh_monthly)

november <- november %>% mutate(VAT=0.19*(kwh_cost+Grund_cost+strom_tax)) %>% mutate(total_cost=kwh_cost+Grund_cost+VAT+strom_tax) %>%
  select(Gas_oder_Strom:total_cost)

november <- november %>% mutate(november, dates="01.July.22-14.Dez.22")

november<- november[2,]
november

total_snov_cost <- rbind(before_choye, july, november)
total_snov_cost
rm(before_choye, july, november)

##########################################################################################################
#########################################################################################################
# for gas until november

Only_gas

before_choye <- Only_gas[c(1,5),]
before_choye

before_choye <- before_choye %>% mutate(m3_monthly =Zaehlerstand-lag(Zaehlerstand), 
                                    time_diff=Datum-lag(Datum))

before_choye<- before_choye %>% mutate(kwh=m3_monthly*Brennwert*Zustandszahl) %>% 
  mutate(Kwh_cost=kwh*Arbeitspreis_Gas, Grund_cost=Grundpreis_Gas_brutto/(365/12)*as.numeric(time_diff)) %>%
  mutate(CO2_tax_cost= CO2_tax*kwh) %>% mutate(kwh_cost_co2=Kwh_cost+CO2_tax_cost)

before_choye <- before_choye %>% mutate(VAT=0.19*(kwh_cost_co2+Grund_cost)) %>% mutate(total_cost=VAT+kwh_cost_co2+Grund_cost)%>%
                                 select(Gas_oder_Strom, m3_monthly:total_cost)

before_choye

before_choye <- before_choye %>% mutate(before_choye, dates="02.Jan.22-01.May.22", People_living_flat="Tristan,Hien,JiYoung")

before_choye<- before_choye[2,]
before_choye

####################
# october
october <- Only_gas[c(5,10),]


october <- october %>% mutate(m3_monthly =Zaehlerstand-lag(Zaehlerstand), 
                                        time_diff=Datum-lag(Datum))

october<- october %>% mutate(kwh=m3_monthly*Brennwert*Zustandszahl) %>% 
  mutate(Kwh_cost=kwh*Arbeitspreis_Gas, Grund_cost=Grundpreis_Gas_brutto/(365/12)*as.numeric(time_diff)) %>%
  mutate(CO2_tax_cost= CO2_tax*kwh) %>% mutate(kwh_cost_co2=Kwh_cost+CO2_tax_cost)

october <- october %>% mutate(VAT=0.19*(kwh_cost_co2+Grund_cost)) %>% mutate(total_cost=VAT+kwh_cost_co2+Grund_cost)%>%
  select(Gas_oder_Strom:total_cost)

october <- october %>% mutate(october, dates="01.May.22-01.Oct.22")

october<- october[2,]
october

###########
# from ocotber onwards, november so far can later be coded so that it runs automatically
november <-Only_gas[c(10,13),]

november <- november %>% mutate(m3_monthly =Zaehlerstand-lag(Zaehlerstand), 
                              time_diff=Datum-lag(Datum))

november<- november %>% mutate(kwh=m3_monthly*Brennwert*Zustandszahl) %>% 
  mutate(Kwh_cost=kwh*Arbeitspreis_Gas, Grund_cost=Grundpreis_Gas_brutto/(365/12)*as.numeric(time_diff)) %>%
  mutate(CO2_tax_cost= CO2_tax*kwh) %>% mutate(kwh_cost_co2=Kwh_cost+CO2_tax_cost) %>%mutate(Gas_spe_umlage=Gas_speicherumlage*kwh)

november <- november %>% mutate(VAT=0.07*(kwh_cost_co2+Grund_cost+Gas_spe_umlage)) %>% mutate(total_cost=VAT+kwh_cost_co2+Grund_cost+ Gas_spe_umlage)%>%
  select(Gas_oder_Strom:total_cost)

november <- november %>% mutate(november, dates="01.Oct.22-14.Dez.22")

november<- november[2,]
november

## bringing the gas cost tables together
before_choye
october
november

before_choye <- before_choye %>% mutate(before_choye, Gas_spe_umlage=0)
october <- october %>% mutate(october, Gas_spe_umlage=0)

total_gnov_cost <- rbind(before_choye, october, november)
rm(before_choye, october, november)

total_gnov_cost

###########
########### Integrating abschläge für strom und gas

total_gnov_cost
total_snov_cost

#Gas abschlagszahlungen für die  jeweiligen zeiträume
abschlag_gass <- c(4*125,3*125+2*116,116+116+0.5*116)
abschlag_gass <- data.frame(abschlag_gass=abschlag_gass)
abschlag_gass

total_gnov_cost <- cbind(total_gnov_cost,abschlag_gass)
total_gnov_cost
total_gnov_cost %>% group_by(dates) %>%summarise(balance=abschlag_gass-total_cost)
total_gnov_cost %>% group_by(dates) %>%summarise(balance=abschlag_gass-total_cost) %>% summarise(sum_balance=sum(balance))

# Strom
abschlag_stromm <- c(3*108, 2*108, 2*108+3*143+0.5*143)
abschlag_stromm <- data.frame(abschlag_stromm=abschlag_stromm)
total_snov_cost <- cbind(total_snov_cost, abschlag_stromm)
total_snov_cost %>% group_by(dates) %>% summarise(balance=abschlag_stromm-total_cost)
total_snov_cost %>% group_by(dates) %>% summarise(balance=abschlag_stromm-total_cost) %>% summarise(sum_balance=sum(balance))+333.26


## füge billing 2022 august für gas und für strom hinzu

total_gnov_cost
total_snov_cost

# balance until first of mai
balance_may_gas <-total_gnov_cost[1,]
balance_may_strom <- total_snov_cost[1,]


balance_may_gas<-balance_may_gas %>% select(dates,People_living_flat, Gas_oder_Strom,total_cost, abschlag_gass)
balance_may_strom <- balance_may_strom %>% select(dates,People_living_flat, Gas_oder_Strom,total_cost, abschlag_stromm)

balance_may_gas <-balance_may_gas %>% mutate(abschlag=abschlag_gass) %>% select(-abschlag_gass)
balance_may_strom <- balance_may_strom %>% mutate(abschlag=abschlag_stromm) %>% select(-abschlag_stromm)

balance_may<-rbind(balance_may_strom, balance_may_gas)
balance_may

savings_2021 <- c(226.06, 0)

balance_may <- cbind(balance_may, savings_2021)
balance_may

savings_may_2022 <- c(202.8,0)

balance_may <- cbind(balance_may, savings_may_2022)
balance_may


may<-balance_may %>% summarise(total_cost=sum(total_cost), abschlag=sum(abschlag), savings_2021=sum(savings_2021), savings_may_2022=sum(savings_may_2022))
may <- may %>% mutate(may, dates="01.Jan.22-01.May.22")
may


may <- may %>% mutate(abs_minus_total_cost=abschlag-total_cost)
may
### there is a negative of 480€ for this period

may <- may %>% select(dates,total_cost, abschlag, abs_minus_total_cost, savings_2021, savings_may_2022)
may <- may %>% mutate(balance=abs_minus_total_cost+savings_may_2022+savings_2021)
may
may <- may %>% mutate(per_person=balance/2)
may_per_person <- may %>% mutate(Jiyoung=per_person, Tristan=per_person, Choye=0)
may_per_person

########################################################################
g_nov14<-total_gnov_cost %>% select(dates,People_living_flat, Gas_oder_Strom,total_cost, abschlag_gass)
s_nov14<-total_snov_cost %>%select(dates, People_living_flat, Gas_oder_Strom,total_cost, abschlag_stromm)

# balance until november, money back from eprimo is added equally to the savings of each person
g_nov14 <- g_nov14[c(2,3),]
s_nov14 <- s_nov14[c(2,3),]

g_nov14 <-g_nov14 %>% mutate(abschlag=abschlag_gass) %>% select(-abschlag_gass)
s_nov14 <- s_nov14 %>% mutate(abschlag=abschlag_stromm) %>% select(-abschlag_stromm)

gs_nov14<-rbind(g_nov14, s_nov14)
gs_nov14

savings_Tristan_nov14 <- c(151.3, 0,0,0)
savings_Choye_no14 <- c(151.3, 0, 0, 0)
savings_Jiyoung_nov_14 <- c(151.3,0,0,0)


gs_nov14 <- cbind(gs_nov14, savings_Tristan_nov14, savings_Choye_no14, savings_Jiyoung_nov_14)
gs_nov14


nov<-gs_nov14 %>% summarise(total_cost=sum(total_cost), abschlag=sum(abschlag), savings_2021=sum(savings_2021), savings_may_2022=sum(savings_may_2022),
                            savings_Tristan_nov14=sum(savings_Tristan_nov14), savings_Choye_no14=sum(savings_Choye_no14), savings_Jiyoung_nov_14=sum(savings_Jiyoung_nov_14))

nov
nov <- nov %>% mutate(nov, dates="01.May.22-14.Dez.22")


nov <- nov %>% mutate(abs_minus_total_cost=abschlag-total_cost)
nov

nov <- nov %>% select(dates,total_cost, abschlag, abs_minus_total_cost, savings_Tristan_nov14, savings_Jiyoung_nov_14, savings_Choye_no14)
nov

nov <- nov %>% mutate(balance=abs_minus_total_cost+savings_Choye_no14+savings_Tristan_nov14+savings_Jiyoung_nov_14)
nov

nov <- nov %>% mutate(per_person=balance/3)
nov_per_person <- nov %>% mutate(Jiyoung=per_person, Tristan=per_person, Choye=per_person)
nov_per_person

########
balance_may_gas 
balance_may_strom 

# ab may für Strom und gas
gs_nov14



may_per_person
nov_per_person


####
# checking if the gas bill is correct
Only_gas_V
august_gas<-Only_gas[c(1,8),]

august_gas <- august_gas %>% mutate(m3_monthly =Zaehlerstand-lag(Zaehlerstand), 
                                    time_diff=Datum-lag(Datum))
august_gas

august_gas <- august_gas %>% mutate(kwh=Brennwert*Zustandszahl*m3_monthly) %>%
                             mutate(kwh_cost=kwh*Arbeitspreis_Gas,
                                    Grund_cost=as.numeric(time_diff)*Grundpreis_Gas_brutto/(365/12),
                                    Gas_tax=kwh*CO2_tax)%>%
                             mutate(VAT=0.19*(kwh_cost+Grund_cost+Gas_tax))%>%
                             mutate(total_cost=VAT+kwh_cost+Gas_tax+Grund_cost)

august_gas <- august_gas[2,]
august_gas <- mutate(august_gas, abschlag=875)
august_gas

##### checking gas an electricity for 11. dezember, heating starting for real
#Gas
Only_gas_V
dez_gas <- Only_gas_V %>% select(Zaehlerstand, m3_monthly, time_diff, dates_gas)
dez_gas <-dez_gas[11,]
dez_gas

#27days
8652.1-8565
dez<- c(8652.1, 87.1, 30, "14.Nov-11.Dez" )
dez_gas <- rbind(dez_gas, dez)

dez_gas <- dez_gas[2,]
str(dez_gas)


dez_gas <- dez_gas %>% mutate(kwh=Brennwert*Zustandszahl*as.numeric(m3_monthly)) %>%
  mutate(kwh_cost=kwh*Arbeitspreis_Gas,
         Grund_cost=as.numeric(time_diff)*Grundpreis_Gas_brutto/(365/12),
         Gas_tax=kwh*CO2_tax)%>%
  mutate(VAT=0.19*(kwh_cost+Grund_cost+Gas_tax))%>%
  mutate(total_cost=VAT+kwh_cost+Gas_tax+Grund_cost)

dez_gas

### Total cost is at 94€ still below abschlag... 116€ will be taken over
#Strom
Only_strom_V
dez_strom <- Only_strom_V %>% select(Zaehlerstand, kwh_monthly, time_diff, dates)
dez_strom <-dez_strom[11,]
dez_strom

#27days
23032.7-22102
dez<- c(23032.7, 930.7, 27, "14.Nov-11.Dez" )
dez_strom <- rbind(dez_strom, dez)

dez_strom <- dez_strom[2,]
str(dez_strom)


dez_strom <- dez_strom %>% mutate(kwh_cost=as.numeric(kwh_monthly)*Verbrauchspreis_abJuly/100,
                               Grund_cost=as.numeric(time_diff)*
                                 Grundpreis_Strom_brutto/(365/12),
                               strom_tax=Stromsteuer*as.numeric(kwh_monthly))

dez_strom <- dez_strom %>% mutate(VAT=0.19*(kwh_cost+Grund_cost+strom_tax)) %>% mutate(total_cost=kwh_cost+Grund_cost+VAT+strom_tax) 
dez_strom  

dez_gas
## For DEzember: 
  # Gas cost: 94€
  # Strom cost 209.7

may_per_person
nov_per_person
gs_nov14

#### only problem can be liquidity... do I have enough on acount to run it... 



s1<-s1+theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5))+theme(axis.title.x = element_blank())+ ylab("Costs in Eurofor Electricity")
g1<-g1+theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5))+theme(axis.title.x = element_blank())+ ylab("Costs in Euro for Gas")
sv<-sv+theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5))+theme(axis.title.x = element_blank())+ ylab("consumption in kwh for Electricity")
gv<-gv+theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5))+theme(axis.title.x = element_blank())+ ylab("consumption in m3 for Gas")


gv_kwh<-ggplot(graph_gV%>%mutate(kwh_monthly=m3_monthly*Brennwert*Zustandszahl)%>%select(dates,kwh_monthly), aes(dates,kwh_monthly))+
  geom_col()+theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.5))+theme(axis.title.x = element_blank())+ylab("consumption in kwh for Gas")
gv_kwh


require(gridExtra)
plot1 <- sv
plot2 <- gv_kwh
plot3<- s1
plot4 <-g1

plot3<-plot3 + geom_segment(x=8, xend=8, y=108, yend=143)+ theme(legend.title = element_blank())
plot3
plot4<- plot4+ theme(legend.title=element_blank())
grid.arrange(plot1, plot2, plot3,plot4, ncol=2)

(12*50.02+5.86*12000/100)/12000

# built in themes

# theme_gray() is the default.
# theme_bw() is useful when you use transparency.
# theme_classic() is more traditional.
# theme_void() removes everything but the data.

plot1 + theme_bw()
plot1 + theme_gray()
plot1 + theme_classic()
plot1 + theme_void()

#ggthemes
#install.packages("ggthemes")
library(ggthemes)

plot1+ theme_fivethirtyeight()
plot1+theme_tufte()
plot1+ theme_wsj()

plot3


geom_segment(aes()) 
# # Add a geom_segment() layer
# ggplot(gm2007, aes(x = lifeExp, y = country, color = lifeExp)) +
#   geom_point(size = 4) +
#   geom_segment(aes(xend = 30, yend = country), size = 2)

