### recibe objetos de app y aplica modelo seir según metodología
library(tidyverse)
library(rriskDistributions)
library(dygraphs)
library(zoo)
library(EpiEstim)

camasGenerales <- 11139
camasCriticas <- 1145
ventiladores <- 1950
enfermerasCamasGenerales <- 10466
enfermerasCamasUCI <- 209
medicosCamasGenerales <- 11.498
medicosCamasUCI <- 103
porcentajeDisponibilidadCamasCOVID <- 0.7


seir <- function(tipo = "A", actualiza = F,
                 compartimentos = F,
                 variacion = 0,
                 porc_detectado = .2,
                 hoy_date = "2020-09-18",
                 R0_usuario = Rusuario, 
                 lag = 17, cantidadDiasProyeccion = 600,
                 ifr = tasaLetalidadAjustada, 
                 duracionI = duracionMediaInf, 
                 duracionE = periodoPreinfPromedio,
                 porc_gr = porcentajeCasosGraves,
                 porc_cr = porcentajeCasosCriticos,
                 trigger_on_app_ok = trigger_on_app, 
                 triggerPorcCrit = trigger_Porc_crit, diasInterv = Dias_interv, R_trigger = trigger_R_inter,
                 data = dataEcdc, # data de ecdc
                 porc_covid = porcentajeDisponibilidadCamasCOVID,
                 N = poblacion, S = N,
                 porc_uci = diasUCICasosCriticos / diasHospCasosCriticos,
                 camasCC = camasCriticas,
                 camasGG = camasGenerales,
                 ventsCC = ventiladoresCamaCritica,
                 camasGGEnfDia = camasGeneralesEnfermeraDia,
                 camasUCIEnfDia = camasUCIEnfermerasDia,
                 camasGGMedDia  = camasGeneralesMedicoDia,
                 camasUCIMedDia = camasCCMedicoDia,
                 enfCamasGG = enfermerasCamasGenerales,
                 enfCamasUCI = enfermerasCamasUCI,
                 medCamasGG = medicosCamasGenerales,
                 medCamasUCI = medicosCamasUCI){

  # variacion
  ifr_inv = ifr * (1 - variacion) # para calcular incidentes (escenario inverso por método)
  ifr = ifr * (1 + variacion)
  porc_cr = porc_cr * (1 + variacion)
  porc_uci = porc_uci * (1 + variacion)
  
  # standarizo ecdc
  inicio_date = as.Date.character("2020-02-01")
  hoy_date = as.Date.character(hoy_date)
  data <- data %>% 
    dplyr::right_join(data.frame(dateRep=seq(inicio_date,hoy_date, by="day")), 
                      by="dateRep") %>% 
    arrange(dateRep) %>% replace(is.na(.), 0)
  
  # fechas
  fin_date = inicio_date + cantidadDiasProyeccion
  fecha = seq(inicio_date, fin_date, by = "days")
  hoy = hoy_date - inicio_date + 1
  fin = fin_date - inicio_date + 1
  
  # observo y suaviz muertes
  d_obs = data$new_deaths
  d_obs_smooth = predict(loess(d_obs~seq(1,nrow(data)),span=.5)) # saco negativos

  # organizo R recibido
  R0_proy <- rep(0,fin)
  for (row in 1:nrow(R0_usuario)){
    R0_proy[fecha %in% seq.Date(R0_usuario$Comienzo[row], 
                                R0_usuario$Final[row],by = "day")] <- R0_usuario$R.modificado[row]
    }
  
  # si compratimentaliza
  if(compartimentos==T){
    # distribuye proporcionalmente
    duracionIi  <- duracionI/4.8 * 5
    duracionIg  <- duracionI/4.8 * 8 
    duracionIc <-  duracionI/4.8 * 16
    # todos contagian: internados y no internados. Consistente con promedio
    duracionI = duracionIi + duracionIg * porc_gr + duracionIc * porc_cr     
  }
  
  # resetea seir
  S = R0 = Sprop = E = I = Ii = Ig = Ic = R = D = d = BC = BC_sat = rep(0,fin)
  
  # estima nuevos infectados según tipo, y suaviza
  if(tipo == "A"){
      i_raw = d_obs_smooth/ifr_inv
      i = rollmean(i_raw, 5, fill = 0)
      i[(hoy-1):hoy] = c(mean(i_raw[(hoy-3):hoy]),mean(i_raw[(hoy-2):hoy])) #cambiaría esto, frena
      i = c(i[(lag+1):hoy],rep(0,lag))
    } else{
      i_raw = data$new_cases / porc_detectado
      i = rollmean(i_raw, 5, fill = 0)
      i[(hoy-1):hoy] = c(mean(i_raw[(hoy-3):hoy]),mean(i_raw[(hoy-2):hoy]))
    }
 
  # para trigger
  fechaVencimientoTrigger <- as.Date.character("2000-01-01")
  fechaIntervencionesTrigger <- vector()
  triggerOn = 0
    
  # seir
  for(t in 2:fin){
    # t=244
    # Expuestos según hay muertes observadas
    if(t<hoy){
      if(tipo=="B"){
        E[t] = i[t+1] * duracionE
        R0[t] = R0_proy[t]
      }
      if(tipo=="A"){
        if(t < (hoy-lag)){
          E[t] = i[t+1] * duracionE
        
        # enganche de lag
        }else if(t == (hoy-lag)){ 
          # toma promedio ult 5 R0
          R0_lag <- mean((E[(t-5):(t-1)] - E[(t-6):(t-2)] * (1-1/rep(duracionE,5))) * rep(duracionI,5) / 
                           I[(t-6):(t-2)] / Sprop[(t-6):(t-2)])
          
          if(actualiza==F){
            R0[t:(hoy-1)] = R0_lag
          }else{ #lo deja constante al actualizar
            R0[t:fin] = R0_lag
          }
          E[t]  = E[t-1] + I[t-1] * R0[t] * Sprop[t-1] / duracionI - E[t-1] / duracionE
          
        }else if(t %in% (hoy-lag+1):(hoy-1)){
          i[t]  = E[t-1]/duracionE
          E[t]  = E[t-1] + I[t-1] * R0[t] * Sprop[t-1] /duracionI - E[t-1]/duracionE
          }
        }
      }
    # seir común
    if(t>=hoy){ 
      
    # si es tipo B, requiere algun R0 por defecto. Queda constante a hoy
      if(t==hoy  & tipo=="B"){
        # toma promedio ult 5 R0
        R0_lag <- mean(
                      (E[(t-5):(t-1)] - E[(t-6):(t-2)] * (1-1/rep(duracionE,5))) * rep(duracionI,5) / 
                       I[(t-6):(t-2)] / Sprop[(t-6):(t-2)])
        if(actualiza==T){R0_proy[t:fin] = R0[t:fin] = R0_lag}
      }
      
    # resetea trigger, solo para lo proyectado
      if (triggerOn==1 & (fecha[t] > fechaVencimientoTrigger)) {triggerOn <- 0}
    # chekea si trigger
      if(triggerOn==0 & !is.na(BC_sat[t-1]) & (BC_sat[t-1]>triggerPorcCrit/100) & trigger_on_app_ok==1){
        triggerOn=1
        fechaVencimientoTrigger <- fecha[t] + diasInterv
        if (length(fechaIntervencionesTrigger) == 0) {
          fechaIntervencionesTrigger <- fecha[t]
        } else {
          fechaIntervencionesTrigger <- c(fechaIntervencionesTrigger, fecha[t])
        }
      }
      
    # R si estamos en trigger
    if(actualiza==F){
      R0[t] = ifelse(triggerOn == 1, R_trigger, R0_proy[t])
    }
    
    # desde hoy
    i[t]  = E[t-1]/duracionE
    E[t]  = E[t-1] + I[t-1] * R0[t] * Sprop[t-1]/duracionI - E[t-1]/duracionE
    }
    
    # resto de seir según seapor compartimentos
    if(compartimentos==T){
      Ii[t] = Ii[t-1] + i[t] - Ii[t-1]/duracionIi
      Ig[t] = Ig[t-1] - Ig[t-1]/duracionIg + Ii[t-1]/duracionIi*porc_gr 
      Ic[t] = Ic[t-1] - Ic[t-1]/duracionIc + Ii[t-1]/duracionIi*porc_cr
      I[t]  = Ii[t] + Ig[t] + Ic[t]
      d[t]  = Ic[t-1]/duracionIc * ifr/porc_cr
      R[t]  = R[t-1] + Ii[t-1]/duracionIi*(1-porc_gr-porc_cr)+ 
              Ig[t-1]/duracionIg + Ic[t-1]/duracionIc
    }else{
      I[t]  = I[t-1] - I[t-1]/duracionI + i[t]
      d[t]  = i[max(t-lag,1)] * ifr
      R[t]  = R[t-1] + I[t-1]/duracionI
      Ig[t] = Ig[t-1] + (i[max(t-5,1)] - i[max(t-13,1)]) * porc_gr
      Ic[t] = Ic[t-1] + (i[max(t-5,1)] - i[max(t-21,1)]) * porc_cr
    }
    D[t]  = D[t-1] + d[t]
    S[t]  = N - E[t] - I[t] - R[t]
    Sprop[t] = S[t]/N
    # uso y saturación de camas críticas para trigger
    BC[t] = Ic[t] * porc_uci
    BC_sat[t] = BC[t]/(camasCC*porc_covid)
  }
  
  # selecciona q devolver
  result <- tibble(fecha = seq(inicio_date, fin_date, by="day"),
                   i_5d_ma = i,
                   i = c(i_raw, rep(NA,fin-hoy)),
                   R = R, I=I, E=E, S=S, Sprop = Sprop,
                   RtEstimado = round(R0,2),
                   R0Usuario = round(R0,2),
                   muertesDiariasReal = c(d_obs,rep(NA,fin-hoy)),
                   muertesDiariasProyeccion = c(d_obs,d[-c(1:hoy)]),
                   muertesAcumuladasReal = cumsum(d),
                   muertesAcumuladasProyeccion = D,
                   muertes_smooth = d,
                   casosCriticos = Ic,
                   casosSeveros = Ig,
                   HHRR.generalBeds = casosSeveros + casosCriticos * (1-porc_uci),
                   HHRR.criticCareBeds = casosCriticos * porc_uci,
                   HHRR.ventilators = HHRR.criticCareBeds * ventsCC,
                   HHRR.generalNurses = HHRR.generalBeds / camasGGEnfDia,
                   HHRR.criticCareNurses = HHRR.criticCareBeds / camasUCIEnfDia,
                   HHRR.generalPhysicans = HHRR.generalBeds / camasGGMedDia,
                   HHRR.criticCarePhysicans = HHRR.criticCareBeds / camasUCIMedDia,
                   HHRR.SAT.generalBeds = HHRR.generalBeds / (camasGG * porc_covid),
                   HHRR.SAT.criticCareBeds = HHRR.criticCareBeds / (camasCC * porc_covid),
                   HHRR.SAT.ventilators = HHRR.ventilators / (ventsCC * camasCC * porc_covid),
                   HHRR.SAT.generalNurses = HHRR.generalNurses / (enfCamasGG * porc_covid),
                   HHRR.SAT.criticCareNurses = HHRR.criticCareNurses / (enfCamasUCI * porc_covid),
                   HHRR.SAT.generalPhysican = HHRR.generalPhysicans / (medCamasGG * porc_covid),
                   HHRR.SAT.criticCarePhysicans = HHRR.criticCarePhysicans / (medCamasUCI * porc_covid))

  # devuelve modelosimulados
  results <- list(modeloSimulado = result)
  results[["fechatrigger"]] <- as.Date(fechaIntervencionesTrigger)
  results[["r_cori"]] <- R0_lag
  return(results)
}


# crea tabla de resultados ------------------------------------------------

crea_tabla_rr <- function(modeloSimulado){
  CantidadMaximaNuevasInfeccionesDia <-
    as.character(format(round(max(
      modeloSimulado$i_5d_ma
    )), decimal.mark = ',', big.mark = '.'))
  fechaPicoNuevasInfecciones <-
    first(as.character(modeloSimulado$fecha[which(modeloSimulado$i_5d_ma == max(modeloSimulado$i_5d_ma))]))
  
  diasHastaPicoNuevasInfecciones <-
    if (format(round(as.numeric(
      as.Date(fechaPicoNuevasInfecciones) - Sys.Date()
    )),
    decimal.mark = ',',
    big.mark = '.') > 0)
    {
      format(round(as.numeric(
        as.Date(fechaPicoNuevasInfecciones) - Sys.Date()
      )),
      decimal.mark = ',',
      big.mark = '.')
    } else {
      paste0("Máximo valor alcanzado el ", fechaPicoNuevasInfecciones)
    }
  
  cantidadMaximaNuevasMuertesPorDia <-
    if (is.na(max(modeloSimulado$muertesDiariasProyeccion)) == TRUE)
    {
      as.character("No se esperan muertes en este escenario")
    } else
    {
      format(round(max(
        modeloSimulado$muertesDiariasProyeccion
      )), decimal.mark = ',', big.mark = '.')
      
    }
  
  fechaPicoNuevasMuertes <-
    if (is.na(max(modeloSimulado$muertesDiariasProyeccion)) == TRUE)
    {
      as.character("No se esperan muertes en este escenario")
    } else
    {
      as.character(first(modeloSimulado$fecha[which(
        modeloSimulado$muertesDiariasProyeccion == max(modeloSimulado$muertesDiariasProyeccion)
      )]))
      
    }
  
  diasHastaPicoNuevasMuertes <-
    if (format(round(as.numeric(
      as.Date(fechaPicoNuevasMuertes) - Sys.Date()
    )),
    decimal.mark = ',',
    big.mark = '.') > 0)
    {
      format(round(as.numeric(
        as.Date(fechaPicoNuevasMuertes) - Sys.Date()
      )),
      decimal.mark = ',',
      big.mark = '.')
    } else
    {
      paste0("Máximo valor alcanzado el ", fechaPicoNuevasMuertes)
      
    }
  
  modeloSimulado$HHRR.criticCareBeds<-as.numeric(modeloSimulado$HHRR.criticCareBeds)
  
  maximaUtiizacionCamasUCI <-
    format(round(max(modeloSimulado$HHRR.criticCareBeds)), decimal.mark = ',', big.mark = '.')
  
  PorcentajeMaximaUtiizacionCamasUCI <-
    paste0(format(
      round(max(modeloSimulado$HHRR.SAT.criticCareBeds) * 100),
      decimal.mark = ',',
      big.mark = '.'
    ), '%')
  fechaPicoMaximaUtiizacionCamasUCI <-
    as.character(modeloSimulado$fecha[which(modeloSimulado$HHRR.criticCareBeds ==
                                              max(modeloSimulado$HHRR.criticCareBeds))])
  diasHastaPicoUtiizacionCamasUCI <-
    if (format(round(as.numeric(
      as.Date(fechaPicoMaximaUtiizacionCamasUCI) - Sys.Date()
    )),
    decimal.mark = ',',
    big.mark = '.') > 0)
    {
      format(round(as.numeric(
        as.Date(fechaPicoMaximaUtiizacionCamasUCI) - Sys.Date()
      )),
      decimal.mark = ',',
      big.mark = '.')
    } else
    {
      paste0("Máximo valor alcanzado el ", fechaPicoMaximaUtiizacionCamasUCI)
      
    }
  
  diasHastaPicoNuevasMuertes <-
    if (format(round(as.numeric(
      as.Date(fechaPicoNuevasMuertes) - Sys.Date()
    )),
    decimal.mark = ',',
    big.mark = '.') > 0)
    {
      format(round(as.numeric(
        as.Date(fechaPicoNuevasMuertes) - Sys.Date()
      )),
      decimal.mark = ',',
      big.mark = '.')
    } else
    {
      paste0("Máximo valor alcanzado el ", fechaPicoNuevasMuertes)
      
    }

  primerDiaSaturacionCamasUCI <-
    if (max(modeloSimulado$HHRR.SAT.criticCareBeds) > 1) {
      as.character(first(modeloSimulado$fecha[which(modeloSimulado$HHRR.SAT.criticCareBeds >
                                                      1)]))
    } else {
      "Capacidad no sobrepasada"
    }
  ultimoDiaSaturacionCamasUCI <-
    if (max(modeloSimulado$HHRR.SAT.criticCareBeds) > 1) {
      as.character(last(modeloSimulado$fecha[which(modeloSimulado$HHRR.SAT.criticCareBeds >
                                                     1)]))
    } else {
      "Capacidad no sobrepasada"
    }
  
  maximaUtiizacionRespiradores <-
    format(round(max(modeloSimulado$HHRR.ventilators)), decimal.mark = ',', big.mark = '.')
  PorcentajeMaximaUtiizacionRespiradores <-
    paste0(format(
      round(max(modeloSimulado$HHRR.SAT.ventilators) * 100),
      decimal.mark = ',',
      big.mark = '.'
    ), '%')
  
  fechaPicoMaximaUtiizacionRespiradores <-
    as.character(modeloSimulado$fecha[which(modeloSimulado$HHRR.ventilators ==
                                              max(modeloSimulado$HHRR.ventilators))])
  
  diasHastaPicoUtiizacionRespiradores <-
    if (format(round(as.numeric(
      as.Date(fechaPicoMaximaUtiizacionRespiradores) - Sys.Date()
    )),
    decimal.mark = ',',
    big.mark = '.') > 0)
    {
      format(round(as.numeric(
        as.Date(fechaPicoMaximaUtiizacionRespiradores) - Sys.Date()
      )),
      decimal.mark = ',',
      big.mark = '.')
    } else
    {
      paste0("Máximo valor alcanzado el ", fechaPicoMaximaUtiizacionRespiradores)
      
    }
  
  primerDiaSaturacionRespiradores <-
    if (max(modeloSimulado$HHRR.SAT.ventilators) > 1) {
      as.character(first(modeloSimulado$fecha[which(modeloSimulado$HHRR.SAT.ventilators >
                                                      1)]))
    } else {
      "Capacidad no sobrepasada"
    }
  ultimoDiaSaturacionRespiradores <-
    if (max(modeloSimulado$HHRR.SAT.ventilators) > 1) {
      as.character(last(modeloSimulado$fecha[which(modeloSimulado$HHRR.SAT.ventilators >
                                                     1)]))
    } else {
      "Capacidad no sobrepasada"
    }
  
  resumenResultados <-
    rbind(
      c(
        "Nuevas infecciones",
        "Cantidad máxima de nuevas infecciones por día",
        CantidadMaximaNuevasInfeccionesDia
      )
      ,
      c(
        "Nuevas infecciones",
        "Fecha del pico de nuevas infecciones",
        as.character(fechaPicoNuevasInfecciones)
      )
      ,
      c(
        "Nuevas infecciones",
        "Días hasta el pico de nuevas infecciones",
        diasHastaPicoNuevasInfecciones
      ),
      c(
        "Nuevas muertes",
        "Cantidad máxima de nuevas muertes por día",
        cantidadMaximaNuevasMuertesPorDia
      ),
      c(
        "Nuevas muertes",
        "Fecha del pico de nuevas muertes",
        fechaPicoNuevasMuertes
      ),
      c(
        "Nuevas muertes",
        "Días hasta el pico de nuevas muertes",
        diasHastaPicoNuevasMuertes
      ),
      c(
        "Camas UCI",
        "Máxima utilización de camas de UCI",
        maximaUtiizacionCamasUCI
      ),
      c(
        "Camas UCI",
        "Días hasta la fecha de máxima utilización de camas UCI",
        diasHastaPicoUtiizacionCamasUCI
      ),
      c(
        "Camas UCI",
        "Porcentaje máximo de utilización de camas de UCI",
        PorcentajeMaximaUtiizacionCamasUCI
      ),
      c(
        "Camas UCI",
        "Día de máxima utilización de camas de UCI",
        fechaPicoMaximaUtiizacionCamasUCI
      ),
      c(
        "Camas UCI",
        "Primer día de saturación de camas de UCI",
        primerDiaSaturacionCamasUCI
      ),
      c(
        "Camas UCI",
        "Último día de saturación de camas de UCI",
        ultimoDiaSaturacionCamasUCI
      ),
      c(
        "Respiradores",
        "Máxima utilización de respiradores",
        maximaUtiizacionRespiradores
      ),
      c(
        "Respiradores",
        "Porcentaje máximo de utilización de respiradores",
        PorcentajeMaximaUtiizacionRespiradores
      ),
      c(
        "Respiradores",
        "Día de máxima utilización de respiradores",
        fechaPicoMaximaUtiizacionRespiradores
      ),
      c(
        "Respiradores",
        "Días hasta la fecha de máxima utilización de respiradores",
        diasHastaPicoUtiizacionRespiradores
      ),
      c(
        "Respiradores",
        "Primer día de saturación de respiradores",
        primerDiaSaturacionRespiradores
      ),
      c(
        "Respiradores",
        "Último día de saturación de respiradores",
        ultimoDiaSaturacionRespiradores
      )
    )
  
  colnames(resumenResultados) <- c("DIMENSION", "INDICADOR", "VALOR")
  return(resumenResultados)
}


# tabla de inputs ---------------------------------------------------------

crea_tabla_inputs <- function(){
  tablaInputs <<- data.frame(
                  Input= c('Periodo preinfeccioso promedio (dias)',
                           'Duracion media de la infecciosidad (dias)','Porcentaje de casos severos',
                           'Porcentaje de casos criticos','Dias con sintomas antes de la hospitalizacion',
                           'Dias de hospitalizacion para casos severos','Dias de hospitalizacion para casos criticos',
                           'Dias de la UCI para casos criticos','Tasa de letalidad (IFR)',
                           'Dias desde el primer informe hasta la dinamica de la muerte: nuevo modelo',
                           'Tiempo desde el final de la incubacion hasta la muerte',
                           'Retraso para el impacto de la politica (dias)','Camas generales atendidas enfermera por dia / numero de turnos',
                           'Las camas de la UCI atendieron a la enfermera por dia / numero de turnos',
                           'Camas generales atendidas por dia medico / numero de turnos',
                           'Camas CC atendidas por dia medico / numero de turnos',
                           'Ventiladores por cama critica','Cantidad de dias de la proyeccion',
                           'Pais seleccionado','Poblacion','Camas generales','Cama criticas','Ventiladores',
                           'Enfermeras por camas generales','Enfermeras por camas UCI','Medicos por camas generales','Medicos por camas UCI',
                           'Porcentaje de disponibilidad de camas para COVID-19'),
                  Valor=c(
                    periodoPreinfPromedio,duracionMediaInf,
                    porcentajeCasosGraves * 100,porcentajeCasosCriticos * 100,
                    diasSintomasAntesHosp,diasHospCasosGraves,diasHospCasosCriticos,
                    diasUCICasosCriticos,tasaLetalidadAjustada,diasPrimerInformeMuerte,diasIncubacionMuerte,
                    retrasoImpactoPolitica,camasGeneralesEnfermeraDia,camasUCIEnfermerasDia,camasGeneralesMedicoDia,
                    camasCCMedicoDia,ventiladoresCamaCritica,cantidadDiasProyeccion,as.character(input$pais),
                    as.numeric(poblacion),camasGenerales,camasCriticas,ventiladores,
                    enfermerasCamasGenerales,enfermerasCamasUCI,medicosCamasGenerales,medicosCamasUCI,
                    porcentajeDisponibilidadCamasCOVID * 100)
                )
}

