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

#First Graph for Strom
s <-  ggplot(Energy_data_raw%>%filter(Gas_oder_Strom=="Strom"), aes(Datum, Zählerstand))+geom_point()
s

# First graph for gas
g <-  ggplot(Energy_data_raw%>%filter(Gas_oder_Strom=="Gas"), aes(Datum, Zählerstand))+geom_point() 
g

Energy_data_raw[1,1] - Energy_data_raw[2,1]



## okay ggplot tutorial would be good here...
## what would be cool to show?

# 1 Automatically calculate total electricty consumption and calculate the final net and brut price

Only_strom <-Energy_data_raw %>% filter(Gas_oder_Strom=="Strom") 
max(Only_strom$Zählerstand)
min(Only_strom$Zählerstand)
max(Only_strom$Datum)
min(Only_strom$Datum)

#total kwh used # different price from july onwards # price decrease, wegfall EEG umlage
date_july1<-Only_strom[6,1]  
Zählerstand_july1 <- Only_strom[6,2] 

total_kwh_strom_untilJuly1 <- Zählerstand_july1 - min(Only_strom$Zählerstand)
total_days_strom_untilJuly1 <- Only_strom[6,1]-Only_strom[1,1]
total_days_strom_untilJuly1 <-as.numeric(total_days_strom_untilJuly1)  
  
# claculation of total kosten Brutto until July1
Total_Brutto_Strom_untilJuly1 <- (total_kwh_strom_untilJuly1*Verbrauchspreis_bis1July/100)+total_days_strom_untilJuly1*(Grundpreis_Strom_brutto/30)
Total_Brutto_Strom_untilJuly1

#Total_Netto_Strom_untilJuly1 <- Total_Brutto_Strom_untilJuly1*0.19+Total_Brutto_Strom_untilJuly1
#Total_Netto_Strom_untilJuly1

### from July onwards I need to calculate differently, operating somehow with max and the july1st value
# Day difference to first july
max_value_date_strom <- difftime(max(Only_strom$Datum),"2022-07-03", units="days")
max_value_date_strom  <- as.numeric(max_value_date_strom)

Total_Brutto_Strom_afterJuly1 <- (max(Only_strom$Zählerstand)-Zählerstand_july1)*Verbrauchspreis_abJuly/100+max_value_date_strom*(Grundpreis_Strom_brutto/30)

## Total bruttostrom_gesamt bis July und ab July
Total_Brutto_strom <- Total_Brutto_Strom_untilJuly1+Total_Brutto_Strom_afterJuly1

## Total Netto Betrag in Euro
Total_Netto_Strom <- 0.19*Total_Brutto_strom+Total_Brutto_strom
Total_Netto_Strom


# 2 Automatically calculate total gas consumption and calculate the final net and brut price; based on given assumptions
Only_GAS <-Energy_data_raw %>% filter(Gas_oder_Strom=="Gas") 
max(Only_GAS$Zählerstand)
min(Only_GAS$Zählerstand)
max(Only_GAS$Datum)
min(Only_GAS$Datum)

time_difference_GAS <- difftime(max(Only_GAS$Datum),min(Only_GAS$Datum), unit="days")
time_difference_GAS

quantity_m3_GAS <- max(Only_GAS$Zählerstand)-min(Only_GAS$Zählerstand)
quantity_m3_GAS

#Umrechnung m3 Gas-Verbrauch in kwh

Gas_kwh =quantity_m3_GAS*Brenntwert*Zustandszahl
Gas_kwh

Brutto_Gas <- Gas_kwh*Arbeitspreis_Gas
Brutto_Gas

Brutto_Grundpreis <- Grundpreis_Gas_brutto/30 *round(as.numeric(time_difference_GAS))
Brutto_Grundpreis

Brutto_Gesamt_Gas <- Brutto_Gas+Brutto_Grundpreis
Brutto_Gesamt_Gas

## Netto Gas Preis
Netto_Gas <- Brutto_Gesamt_Gas*0.19+Brutto_Gesamt_Gas
Netto_Gas

### calclating our saving, on 06.07.2022 we have 800??? on our account
### We have paid gas from January onwards under currrent contract
### We have paid Electricity from Feb onwards under contract

savings_per_month_wg <-Rent_C+Rent_J+Rent_T-miete-Internet-Abschlag_Gas-Abschlag_Strom
savings_per_month_wg
July1_money <- 800-Internet- Abschlag_Gas- Abschlag_Strom
Starting_savings <- July1_money -5*savings_per_month_wg

## Savings basically contain when I battled our old supplier and they gave me some compensation money 
Starting_savings



as.numeric(month(as.POSIXlt(max(Only_strom$Datum), format="%Y-%m-%d")))


#2 separate total statistc for choye and rest

#3

savings_per_month_wg <-Rent_C+Rent_J+Rent_T-miete-Internet-Abschlag_Gas-Abschlag_Strom



##################################################################################################################################
#################################################################################################################################
##
##                                    Trying to make some nice graphs                                                   ########

##################################################################################################################################

str(Only_strom)

# Verbrauch in kwh in this year
min(Only_strom$Zählerstand)

Only_strom<-Only_strom %>% mutate(Verbrauch_Strom_kWh=Zählerstand-min(Only_strom$Zählerstand))
Only_strom$Datum <- strptime(Only_strom$Datum, "%Y-%m-%d" )
Only_strom$Datum <- as.POSIXct(Only_strom$Datum)


Only_strom <- mutate(Only_strom, MonthYear = paste(year(Datum),formatC(month(Datum), width = 2, flag = "0")))
Only_strom

Only_strom <- Only_strom%>%arrange(Only_strom$Datum)
Only_strom

## Bis zum 3.07. gilt der alte Preis, danach günstiger

Only_strom %>% filter(Datum <= "2022-07-03")

Only_Strom_before_July <-Only_strom %>% filter(Datum <= "2022-07-03") %>% mutate(Verbrauch_Euro_brutto=Verbrauch_Strom_kWh*Verbrauchspreis_bis1July/100)
Only_Strom_after_July <-Only_strom %>% filter(Datum > "2022-07-03") %>% mutate(Verbrauch_Euro_brutto=2561*Verbrauchspreis_bis1July/100+(Verbrauch_Strom_kWh-2561)*Verbrauchspreis_abJuly/100)
                                                                                 
Only_strom <- rbind(Only_Strom_before_July,Only_Strom_after_July)



#What I really do need is the estimated energy use per month...
# Alternatively I can simply take the daily rate between to datapoints- and the have a curve... ?



Only_strom<-Only_strom %>% mutate(Verbrauch_Strom_kWh=Zählerstand-min(Only_strom$Zählerstand))


Only_strom <- Only_strom %>% mutate(MoM =Verbrauch_Strom_kWh-lag(Verbrauch_Strom_kWh))

barplot(Only_strom$Verbrauch_Strom_kWh)
ggplot(Only_strom, aes(Datum,Verbrauch_Strom_kWh))+geom_point()+ylab("Electricity Usage in kWh")
ggplot(Only_strom, aes(Datum,Verbrauch_Euro_brutto))+geom_point()+ylab("Euro_brutto")
ggplot(Only_strom, aes(Datum,MoM))+geom_point()+ylab("DIFFElectricity Usage in kWh")



Temperatures <- mutate(Temperatures, MonthYear = paste(year(Date),formatC(month(Date), width = 2, flag = "0")))
















