library(dplyr)

url_oms <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
oms_data <- read.csv(url_oms, encoding = "UTF-8")
colnames(oms_data)[1]<-"Date_reported"
paises_oms <-
  c(
    "Argentina",
    "Bahamas",
    "Barbados",
    "Belize",
    "Bolivia (Plurinational State of)",
    "Brazil",
    "Chile",
    "Colombia",
    "Costa Rica",
    "Dominican Republic",
    "Ecuador",
    "El Salvador",
    "Guatemala",
    "Guyana",
    "Haiti",
    "Honduras",
    "Jamaica",
    "Mexico",
    "Nicaragua",
    "Panama",
    "Paraguay",
    "Peru",
    "Suriname",
    "Trinidad and Tobago",
    "Uruguay",
    "Venezuela (Bolivarian Republic of)"
  )

oms_data <- oms_data %>% filter(oms_data$Country %in% paises_oms)
oms_data$location <- as.character(oms_data$Country)
oms_data$location[oms_data$Country=="Argentina"]<-"ARG"
oms_data$location[oms_data$Country=="Bahamas"]<-"BHS"
oms_data$location[oms_data$Country=="Barbados"]<-"BRB"
oms_data$location[oms_data$Country=="Belize"]<-"BLZ"
oms_data$location[oms_data$Country=="Bolivia (Plurinational State of)"]<-"BOL"
oms_data$location[oms_data$Country=="Brazil"]<-"BRA"
oms_data$location[oms_data$Country=="Chile"]<-"CHL"
oms_data$location[oms_data$Country=="Colombia"]<-"COL"
oms_data$location[oms_data$Country=="Costa Rica"]<-"CRI"
oms_data$location[oms_data$Country=="Dominican Republic"]<-"DOM"
oms_data$location[oms_data$Country=="Ecuador"]<-"ECU"
oms_data$location[oms_data$Country=="El Salvador"]<-"SLV"
oms_data$location[oms_data$Country=="Guatemala"]<-"GTM"
oms_data$location[oms_data$Country=="Guyana"]<-"GUY"
oms_data$location[oms_data$Country=="Haiti"]<-"HTI"
oms_data$location[oms_data$Country=="Honduras"]<-"HND"
oms_data$location[oms_data$Country=="Jamaica"]<-"JAM"
oms_data$location[oms_data$Country=="Mexico"]<-"MEX"
oms_data$location[oms_data$Country=="Nicaragua"]<-"NIC"
oms_data$location[oms_data$Country=="Panama"]<-"PAN"
oms_data$location[oms_data$Country=="Paraguay"]<-"PRY"
oms_data$location[oms_data$Country=="Peru"]<-"PER"
oms_data$location[oms_data$Country=="Suriname"]<-"SUR"
oms_data$location[oms_data$Country=="Trinidad and Tobago"]<-"TTO"
oms_data$location[oms_data$Country=="Uruguay"]<-"URY"
oms_data$location[oms_data$Country=="Venezuela (Bolivarian Republic of)"]<-"VEN"
oms_data$Date_reported<-as.character(oms_data$Date_reported)
oms_data <-
  oms_data %>% dplyr::select(
    iso_code = location,
    location = Country,
    date = Date_reported,
    total_cases = Cumulative_cases,
    new_cases = New_cases,
    total_deaths = Cumulative_deaths,
    new_deaths = New_deaths
  ) %>% filter(oms_data$Date_reported<=hoy)

save(oms_data,file="DatosIniciales/oms_data.RData")
