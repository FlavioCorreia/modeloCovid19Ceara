library(tidyverse)
library(sqldf)
library(tidyr)
library(dplyr)

# vector paises
paises <-c("BRA","CEA")

# get data
# Our World In Data. Deaths and Cases from ECDC
owd_data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",header = T) %>% 
  dplyr::select(iso_code, location,
                date,
                total_cases, new_cases,
                total_deaths, new_deaths,
                total_tests, new_tests,
                total_tests_per_thousand, new_tests_per_thousand,new_tests_smoothed_per_thousand,
                stringency_index, 
                population,
                population_density,
                aged_65_older,
                life_expectancy) %>% 
  filter(iso_code %in% paises,
         as.Date(date) <= as.Date(hoy))

#agrega datos subnacionales de Argentina



##### CEARA #####

# download, extract and import  

dataCeara <- read.csv(unzip("casos_covid19.zip",exdir="tempZip"), sep=";")
unlink("tempZip", recursive=TRUE)
# prepare data

dataCeara$dataInicioSintomas <- as.Date(dataCeara$dataInicioSintomas, format="%d-%m-%Y") #date format
dataCeara$dataObito <- as.Date(dataCeara$dataObito, format="%d-%m-%Y") #date format

firstDate <- min(dataCeara$dataInicioSintomas[is.na(dataCeara$dataInicioSintomas)==FALSE]) #first day of sequence
lastDate <- max(dataCeara$dataInicioSintomas[is.na(dataCeara$dataInicioSintomas)==FALSE]) #last day of sequence

cases <- dataCeara[is.na(dataCeara$dataInicioSintomas)==FALSE & 
                     dataCeara$resultadoFinalExame=="Positivo",]

cases <- cases %>% dplyr::group_by(dataInicioSintomas) %>% tally() 


seq <- data.frame(dateRep=seq(firstDate,lastDate, by=1))

cases <- merge(seq, cases, by.x="dateRep", by.y="dataInicioSintomas")

deaths <- dataCeara[is.na(dataCeara$dataInicioSintomas)==FALSE & 
                      dataCeara$obitoConfirmado=="True",]

deaths <- deaths %>% dplyr::group_by(dataObito) %>% tally() 
dataCeara <- merge(cases,deaths, by.x="dateRep", by.y="dataObito", all.x=TRUE)
colnames(dataCeara)[2:3] <- c("new_cases","new_deaths")
dataCeara$new_cases[is.na(dataCeara$new_cases)==TRUE] <- 0
dataCeara$new_deaths[is.na(dataCeara$new_deaths)==TRUE] <- 0

dataCeara <- dataCeara %>% mutate(total_cases=cumsum(new_cases), total_deaths=cumsum(new_deaths))
dataCeara$iso_code <- "CEA"
dataCeara$location <- "Ceara" 
dataCeara$date <- as.character(dataCeara$dateRep) 


rm(cases)
rm(deaths)
rm(seq)
rm(firstDate)
rm(lastDate)



owd_data<-union_all(owd_data,dataCeara)


# guarda input folder
# setwd("appTest")
save(owd_data, file =  "DatosIniciales/owd_data.RData")

# graphs
# gt <- owd_data %>% filter(as.Date(date) > as.Date("2020-03-01")) %>% 
#               dplyr::select(pais=location, date, total_tests_per_thousand) %>% 
#               ggplot()+ geom_step(aes(as.Date(date), 
#                                       as.integer(total_tests_per_thousand)))+
#               labs(x="Fecha", y="Test/Hab")+
#               theme_minimal() +
#               ggtitle("Test realizados (acum) cada 1000 hab.") +
#               facet_wrap(~pais)
# gsi <- owd_data %>% filter(as.Date(date) > as.Date("2020-03-01")) %>% 
#               dplyr::select(pais=iso_code, date, stringency_index) %>% 
#               ggplot()+ geom_step(aes(as.Date(date), 
#                                       as.numeric(stringency_index),
#                                       color=pais))+
#               labs(x="Fecha", y="Stringency Index")+
#               theme_minimal() +
#               ggtitle("Stringency Index")+
#               facet_wrap(~pais)
#               # ggplot2::annotate("text",x = as.Date("2020-05-01"),
#               #                   y = 30,label="Government Response Stringency Index: composite 
#               #                   measure based on 9 response indicators including 
#               #                   school closures, workplace closures, and travel bans, 
#               #                   rescaled to a value from 0 to 100 (100 = strictest response)")

