library(tidyverse)
library(rgdal)
library(leaflet)

# map y labels ---------------------------------------------------------------------
load("DatosIniciales/owd_data.RData")
LA_data <- owd_data %>%
                  group_by(iso_code) %>%
                  dplyr::summarise(cum_cases_millon = round(sum(new_cases,na.rm = T)/max(population)*1e6,2),
                                   cum_deaths_millon = round(sum(new_deaths,na.rm = T)/max(population)*1e6, 2)) %>%
                  mutate(ADM0_A3 = iso_code) %>%
                  as.data.frame()

# shapes países
# download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", "countries.zip")
# unzip("C:/Proyectos/covid_IECS/shapes/countries.zip")

#unzip(zipfile = "WorldMap/countries.zip", exdir = "WorldMap")
unzip(zipfile = "WorldMap/EstadosBrasileiros.zip", exdir = "WorldMap")

#World <- readOGR(dsn="WorldMap", layer="ne_10m_admin_0_countries",encoding = 'UTF-8')
World <- readOGR(dsn="WorldMap", layer="ne_10m_admin_0_countries",encoding = 'UTF-8')

LA <- World
# shapes provs
# download.file("https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_prov_datos.zip", "appTest - Cod/WorldMap/provinciasArg.zip")
# unzip(zipfile = "appTest - Cod/WorldMap/provinciasArg.zip", exdir = "appTest - Cod/WorldMap")
# Provs <- readOGR("appTest - Cod/WorldMap/pxpciadatosok.shp", encoding = 'UTF-8')
# Provs <- subset(Provs, link %in% c("18","02","50"))

# leaflet(Provs,
#         options = leafletOptions(attributionControl=FALSE,
#                                  zoomControl = FALSE)) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(stroke = F)
# Provs@data <- Provs@data[c("link","provincia")]
# Provs@data$link = as.character(Provs@data$link)
# Provs@data$provincia = as.character(Provs@data$provincia)
# Provs@data$provincia[Provs@data$link=="02"] <- "Ciudad Autónoma de Buenos Aires"
# Provs@data$provincia[Provs@data$link=="18"] <- "Corrientes"
# Provs@data$provincia[Provs@data$link=="50"] <- "Mendoza"
# Provs@data$provincia[Provs@data$link=="06"] <- "Buenos Aires"
# colnames(Provs@data) <- c("NAME","ADM0_A3")
#map_data <- rbind(LA, Provs)
map_data <- LA
map_data@data$ADM0_A3 = as.character(map_data@data$ADM0_A3)

map_data <- merge(map_data, LA_data, by = "ADM0_A3", all.x=T)
# map_data@data$ADM0_A3[map_data@data$ADM0_A3=="Corrientes"] = "ARG_18"
# map_data@data$ADM0_A3[map_data@data$ADM0_A3=="Ciudad Autónoma de Buenos Aires"] = "ARG_2"
# map_data@data$ADM0_A3[map_data@data$ADM0_A3=="Buenos Aires"] = "ARG_7"
# map_data@data$ADM0_A3[map_data@data$ADM0_A3=="Mendoza"] = "ARG_50"
# 
# map_data@data[map_data@data$ADM0_A3=="ARG_18",4:5] <- LA_data[LA_data$iso_code=="ARG_18",2:3] 
# map_data@data[map_data@data$ADM0_A3=="ARG_2",4:5] <- LA_data[LA_data$iso_code=="ARG_2",2:3]
# map_data@data[map_data@data$ADM0_A3=="ARG_7",4:5] <- LA_data[LA_data$iso_code=="ARG_7",2:3]
# map_data@data[map_data@data$ADM0_A3=="ARG_50",4:5] <- LA_data[LA_data$iso_code=="ARG_50",2:3]

# coords
coords = data.frame(pais = as.character(map_data@data$ADM0_A3),
                    lng = coordinates(map_data)[,1],
                    lat = coordinates(map_data)[,2])
coords <- rbind(coords,c("CEA",-39.45150684280856,-4.9451178752418326))
# 
# coords$lng[coords$pais=="ARG_18"] = -57.935813
# coords$lat[coords$pais=="ARG_18"] = -28.983072
# coords$lng[coords$pais=="CEA"] = -4.9451178752418326
# coords$lat[coords$pais=="CEA"] = -39.45150684280856

# test
# leaflet(map_data,
#         options = leafletOptions(attributionControl=FALSE,
#                                  zoomControl = FALSE)) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(stroke = F)

# save
save(map_data, coords, file =  "DatosIniciales/Map.RData")


