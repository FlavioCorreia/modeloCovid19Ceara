#### Actualizar contenido ####
# se actualizan insumos para app, recalculando curvas en base a últimas observaciones
# se utiilzan tres fuentes ppalmente: Ecdc (online), oms (online), owd (online), MSal (descarga manual).
# la funcionalidad de los links de descarga debe ser revisada periódicamente
# una vez organizada la info, se apica función seir.

# librerías
library(tidyverse)
library(readxl)
library(sqldf)
library(readxl)
library(zoo)
library(EpiEstim)

#### países/juris a actualizar ####
#setwd("C:/Users/Adrian/Desktop/CEARA")
hoy <<- diaActualizacion <<- as.Date("2020-11-18")
paises_actualizar <- c("CEA","BRA")

##### carga población y oms data  ####
load("DatosIniciales/poblacion_data.RData")
source("oms_data.R", encoding = "UTF-8")

##### descarga ultimos datos de msal  ####
# urlMsal <- 'https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv'
# download.file(urlMsal, "Covid19Casos.csv")

#### casos/muertes y parámetros para cada país ####
input=list()

for(p in paises_actualizar){

input$pais = p

if (substr(input$pais,1,3)=="CEA"){
  # ADOPCION CEARA
  
  library(dplyr)
  
  # download, extract and import from datasus  
  
  download.file("http://download-integrasus.saude.ce.gov.br/casos_covid19", 
                destfile = "casos_covid19.zip", 
                mode="wb")
  
  dataCeara <- read.csv(unzip("casos_covid19.zip",exdir="tempZip"), sep=";")
  unlink("tempZip", recursive=TRUE)
  
  # prepare data
  
  dataCeara$dataInicioSintomas <- as.Date(dataCeara$dataInicioSintomas, format="%d-%m-%Y") #date format
  dataCeara$dataObito <- as.Date(dataCeara$dataObito, format="%d-%m-%Y") #date format
  
  firstDate <- min(dataCeara$dataInicioSintomas[is.na(dataCeara$dataInicioSintomas)==FALSE]) #first day of sequence
  lastDate <- max(dataCeara$dataInicioSintomas[is.na(dataCeara$dataInicioSintomas)==FALSE]) #last day of sequence
  
  cases <- dataCeara[is.na(dataCeara$dataInicioSintomas)==FALSE & 
                       dataCeara$resultadoFinalExame=="Positivo",] #filter confirmed
  
  cases <- cases %>% dplyr::group_by(dataInicioSintomas) %>% tally() #group by and count 
  
  
  seq <- data.frame(dateRep=seq(firstDate,lastDate, by=1)) #sequence
  
  cases <- merge(seq, cases, by.x="dateRep", by.y="dataInicioSintomas") #format cases data.frame 
  
  deaths <- dataCeara[is.na(dataCeara$dataInicioSintomas)==FALSE & 
                        dataCeara$obitoConfirmado=="True",] #filter deaths
  
  deaths <- deaths %>% dplyr::group_by(dataObito) %>% tally() #group by and count
  
  dataCeara <- merge(cases,deaths, by.x="dateRep", by.y="dataObito", all.x=TRUE) #merge cases and deaths by date
  
  colnames(dataCeara)[2:3] <- c("new_cases","new_deaths") #column names
  
  dataCeara$new_cases[is.na(dataCeara$new_cases)==TRUE] <- 0 #na to cero
  dataCeara$new_deaths[is.na(dataCeara$new_deaths)==TRUE] <- 0 #na to cero
  
  dataCeara <- dataCeara %>% mutate(total_cases=cumsum(new_cases), total_deaths=cumsum(new_deaths)) #final data frame
  
  #enviromental cleaning
  rm(cases)
  rm(deaths)
  rm(seq)
  rm(firstDate)
  rm(lastDate)  

  dataCeara <- dataCeara %>% filter(dateRep<=diaActualizacion)
  dataEcdc <- dataCeara
} else
  
{
  dataEcdc <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                       na.strings = "", fileEncoding = "UTF-8-BOM")

  dataEcdc$dateRep <- as.Date(dataEcdc$dateRep, format = "%d/%m/%Y")
  dataEcdc$dateRep <- format(dataEcdc$dateRep, "%Y-%m-%d")
  dataEcdc<-dataEcdc %>% filter(dateRep<=Sys.Date())
  dataEcdc$dateRep<-as.Date(dataEcdc$dateRep)
  
  dataEcdc <- dataEcdc %>% filter(countryterritoryCode==input$pais)
  dataEcdc <- dataEcdc %>% dplyr::select(fecha=dateRep,countryterritoryCode, cases, deaths)
  seqFecha<-seq(min(as.Date(dataEcdc$fecha)),max(as.Date(dataEcdc$fecha)), by=1 )
  seqFecha<-data.frame(secuencia=seqFecha)
  seqFecha$secuencia<-as.Date(seqFecha$secuencia)
  
  dataEcdc<-sqldf('
      select T1.*, T2.cases,T2.deaths from seqFecha as T1
      left join dataEcdc as T2 on
      T1.secuencia=T2.fecha
      ')
  dataEcdc<-data.frame(dataEcdc %>% dplyr::select(dateRep=secuencia,cases,deaths))
  
  dataEcdc <-
    mutate(dataEcdc, deaths = ifelse(is.na(deaths), 0, deaths))
  
  dataEcdc$cases[is.na(dataEcdc$cases)] <- 0
  
  dataEcdc <- dataEcdc %>%
    mutate(total_cases = cumsum(cases)) %>%
    mutate(total_deaths = cumsum(deaths))
  
  colnames(dataEcdc)[2] <- "new_cases"
  colnames(dataEcdc)[3] <- "new_deaths"
  
  dataEcdc <-
    dataEcdc %>% filter(
      dateRep <= diaActualizacion &
        total_cases > 0
    )
}

#### parametros epidemiológicos####

## Periodo preinfeccioso promedio (días)
periodoPreinfPromedio <- 5.84

## Duración media de la infecciosidad (días)
duracionMediaInf <- 4.8

## Porcentaje de casos severos
porcentajeCasosGraves <- 0.0328

## Porcentaje de casos críticos
porcentajeCasosCriticos <- 0.0054

## Días con síntomas antes de la hospitalización.
diasSintomasAntesHosp <- 7.12

## Días de hospitalización para casos severos.
diasHospCasosGraves <- 5.0

## Días de hospitalización para casos críticos.
diasHospCasosCriticos <- 23.0

## Días de la UCI para casos críticos
diasUCICasosCriticos <- 18.0

## Tasa letalidad
tasaLetalidadAjustada <- 0.0027

## Días desde el primer informe hasta la dinámica de la muerte: nuevo modelo
diasPrimerInformeMuerte <- 7.0

## Tiempo desde el final de la incubación hasta la muerte.
diasIncubacionMuerte <- 3.0

## Retraso para el impacto de la política (días)
retrasoImpactoPolitica <- 3.0

# Camas generales atendidas enfermera por día / número de turnos
camasGeneralesEnfermeraDia <- 2.66667

## Las camas de la UCI atendieron a la enfermera por día / número de turnos
camasUCIEnfermerasDia <- 0.66667

## Camas generales atendidas por día médico / número de turnos
camasGeneralesMedicoDia <- 4.0

## Camas CC atendidas por día médico / número de turnos
camasCCMedicoDia <- 4.0

## Ventiladores por cama crítica
ventiladoresCamaCritica <- 0.654

## Cantidad de días de la proyección
cantidadDiasProyeccion <- 1000

## Día de inicio
diaInicio <- '2020-02-12'

## Expuestos
expuestos <- 1

## Infectados
infectados <- 1

## Recuperados
recuperados <- 0

## Población
poblacion<-as.numeric(poblacion_data$value[which(poblacion_data$indicator=='total' & poblacion_data$pais==input$pais)])

##### Recursos #####
# asigna recursos según país
recursos <- read.csv("recursos.csv",sep=";") %>% filter(pais==input$pais)
camasGenerales <- recursos[,"camasGenerales"]
camasCriticas <- recursos[,"camasCriticas"]
ventiladores <- recursos[,"ventiladores"]
enfermerasCamasGenerales <- recursos[,"enfermerasCamasGenerales"]
enfermerasCamasUCI <- recursos[,"enfermerasCamasUCI"]
medicosCamasGenerales <- recursos[,"medicosCamasGenerales"]
medicosCamasUCI <- recursos[,"medicosCamasUCI"]
porcentajeDisponibilidadCamasCOVID <- recursos[,"porcentajeDisponibilidadCamasCOVID"]

#ALTERACAO DIRETA CEARA
# camasGenerales <- 11139
# camasCriticas <- 1145
# ventiladores <- 1950
# enfermerasCamasGenerales <- 10466
# enfermerasCamasUCI <- 209
# medicosCamasGenerales <- 11.498
# medicosCamasUCI <- 103
# porcentajeDisponibilidadCamasCOVID <- 0.7

#### Actualizar ####

# obtiene función seir
source("seir.R", encoding = "UTF-8")

# paises con infectados segun porcentaje no detectado
paises_distintos <- c("ARG_18","CRI","SLV","JAM","PRY","ARG_50","BHS","BLZ","BRB",
                       "GUY","HTI","NIC","SUR","TTO","VEN")

# valores por default de intervención. En actualización nunca aplica trigger
default=TRUE
trigger_Porc_crit=60
trigger_R_inter=0.9
Dias_interv=30
trigger_on_app=0
fechaIntervencionesTrigger = c()

# El R0 que ingreso al comienzo no es relevante debido que
# al actualizar se se calcula, siguiendo ritmo de observado
# escenario principal
seir_update <- seir(actualiza = T,
                tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                hoy_date = hoy, 
                R0_usuario = data.frame(Comienzo=hoy, 
                                     Final=as.Date("2021-10-09"), 
                                     R.modificado=1.2))
modeloSimulado <- seir_update$modeloSimulado

# creo objetos para app
r_cori <- seir_update$r_cori
Rusuario <- data.frame(Comienzo = max(dataEcdc$dateRep),
                       Final = max(dataEcdc$dateRep)+420,
                       R.modificado=r_cori)
resumenResultados <- crea_tabla_rr(modeloSimulado = modeloSimulado)
crea_tabla_inputs()

# escenario hi
seir_update_hi <- seir(actualiza = T, variacion = .25, 
                     tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                     hoy_date = hoy, 
                     R0_usuario = data.frame(Comienzo=hoy, 
                                             Final=as.Date("2021-10-09"), 
                                             R.modificado=1.2))
modeloSimulado_hi <- seir_update_hi$modeloSimulado

# escenario low
seir_update_low <- seir(actualiza = T, variacion = -.25,
                       tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                       hoy_date = hoy, 
                       R0_usuario = data.frame(Comienzo=hoy, 
                                               Final=as.Date("2021-10-09"), 
                                               R.modificado=1.2))
modeloSimulado_low <- seir_update_low$modeloSimulado
rm(seir_update)
rm(seir_update_hi)
rm(seir_update_low)

#### guarda conjunto de datos que serán levantados en la app####
save.image(paste0("DatosIniciales/DatosIniciales_",input$pais,".RData"))
print(input$pais)
}

#### update owd_data and mapa ####
source("owd_data.R", encoding = "UTF-8")
source("map_set.R", encoding = "UTF-8")

