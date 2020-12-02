library(xts)
library(dygraphs)


# not in ------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


# format numbers ----------------------------------------------------------

fnum <- function(x, dec=0){
  format(x,big.mark=".",decimal.mark = ",", scientific = F, digits = dec)
}


# Rt estimates ------------------------------------------------------------

library(R0)
library(EpiEstim)
get_R <- function(modeloSimulado, a=2.6, b=1.5, window = 15, t=115){
  
  # Wallinga & Lipsitch (2007)
  mGT = generation.time ("gamma", c(a, b))
  Rw <- est.R0.EG (round(modeloSimulado$i_5d_ma[(t-window):t]), mGT, 
                   begin=1, end=window)
  
  # Cori et al. (2013)
  Rc <- estimate_R(incid = modeloSimulado$i_5d_ma[(t-window):t],
                   method = "parametric_si",
                   config = make_config(list(
                     mean_si=a, std_si=b,
                     t_start = 2,
                     t_end = window)))
  return(list(wallinga = c(Rw$R,Rw$conf.int), 
              cori = as.numeric(Rc$R[c(3,6,10)]))
  )
}


# get dias de duplicacion -------------------------------------------------

get_dias_dupl <- function(data, hoy, ventana = 7){
  # data
  df <- data %>% 
    dplyr::select(date,total_cases)%>% 
    mutate(date = as.Date(date)) %>% 
    filter(date %in% seq(as.Date(hoy)-ventana, as.Date(hoy)-1, by="day")) %>%
    right_join(data.frame(date=seq(as.Date(hoy)-ventana, as.Date(hoy)-1, by="day"))) %>% 
    fill(total_cases, .direction = "up") %>%
    fill(total_cases, .direction = "down") %>% 
    arrange(date) %>% 
    mutate(dia = 1:ventana,
           acumulados = total_cases, # casos acumulados
           log.acumulados = log(acumulados)) %>% 
    dplyr::select(dia, acumulados, log.acumulados)
  
  # reg lineal de crecim exp
  reglin <- lm(df, formula = log.acumulados ~ dia)
  B1 = as.numeric(reglin$coefficients[2])
  errorB1 <- summary(reglin)$coef[2, 2]
  
  # media e ICs 95%
  diasDup = log(2) / c(B1, B1-1.96*errorB1, B1+1.96*errorB1) 
  
  return(diasDup)
}

# get_dias_dupl(owd_data %>% filter(iso_code=="ARG_2"),hoy,7)


# modulo de graficos dygraph ----------------------------------------------

Grafica <- function(id, label = "Nada") {
  
  ns <- NS(id)
  
  tagList(  
    # Infecciones
    radioGroupButtons(
      inputId = ns("que_infecc"),
      label = "",
      choices = c("Diarias","Acumuladas"),
      selected = "Diarias",
      status = "success",
      checkIcon = list(yes = icon("check"))
    ),  
    p(
      style="font-size: 20px;",
      align = "center",
      textOutput(ns("titulo_infecciones"), inline=TRUE),
      tags$span(id="info_I", icon("info-circle"), 
                style=("margin-left:5px;"))
    ),
    dygraphOutput(ns("g_i")),
    fluidRow(
      column(2, align = "center", textOutput(ns("min_graf_inf"))),
      column(8,
             p(
               align = "center",
               paste("Seleccione el segmento visible del período completo de proyección")
             )
      ),
      column(2, align = "center", textOutput(ns("max_graf_inf"))
      )
    ),
    br(),br(),br(),
    
    # Defunciones
    radioGroupButtons(
      inputId = ns("que_defs"),
      label = "",
      choices = c("Diarias","Acumuladas"),
      selected = "Diarias",
      status = "success",
      checkIcon = list(yes = icon("check"))
    ),
    p(
      style="font-size: 20px;",
      align = "center",
      textOutput(ns("titulo_defunciones"), inline=TRUE),
      tags$span(id="info_D", icon("info-circle"))
    ),
    dygraphOutput(ns("g_d")),
    fluidRow(
      column(2, align = "center", textOutput(ns("min_graf_mue"))),
      column(8,
             p(
               align = "center",
               paste("Seleccione el segmento visible del período completo de proyección")
             )
      ),
      column(2, align = "center", textOutput(ns("max_graf_mue"))
      )
    ),
    
    br(),br(),br(),
    
    # camas/vents
    radioGroupButtons(
      inputId = ns("que_rrhh"),
      label = "Impacto en recursos sanitarios",
      choices = c("Camas","Ventiladores","Médicos","Enfermeras"),
      selected = "Camas",
      status = "success",
      checkIcon = list(yes = icon("check"))
    ),
    p(
      style="font-size: 20px;",
      align = "center",
      textOutput(ns("titulo_camas_vents"), inline=TRUE),
      tags$span(id="info_C", icon("info-circle"))
    ),
    
    dygraphOutput(ns("g_Beds_Vents")),
    
    fluidRow(
      column(2, align = "center", textOutput(ns("min_graf_cam"))),
      column(8,
             p(
               align = "center",
               paste("Seleccione el segmento visible del período completo de proyección")
             )
      ),
      column(2, align = "center", textOutput(ns("max_graf_cam"))
      )
      
    ),
    br(),br(),br(),
    
    # br(),br(),br(),
    # # SEIR
    # p(
    #   style="font-size: 20px;",
    #   align = "center",
    #   textOutput(ns("titulo_SEIR"), inline=TRUE),
    #   tags$span(id="info_SEIR", icon("info-circle"))
    # ),
    # dygraphOutput(ns("g_SEIR")),
    # fluidRow(
    #   column(2, align = "center", textOutput(ns("min_graf_SEIR"))),
    #   column(8,
    #          p(
    #            align = "center",
    #            paste("Seleccione el segmento visible del período completo de proyección")
    #          )
    #   ),
    #   column(2, align = "center", textOutput(ns("max_graf_SEIR"))
    #   )
    # )
  )
}


graficando <- function(input, output, session) {
  
  rango_data = paste0(month(range(modeloSimulado$fecha),label = T,abbr = F),"-",
                      year(range(modeloSimulado$fecha)))
  range_graf_select = c(min(as.Date(modeloSimulado$fecha)),as.Date("2020-12-31")) + c(60,0)
  output$min_graf_inf <- renderText({str_to_sentence(as.character(rango_data[1]))})
  output$max_graf_inf <- renderText({str_to_sentence(as.character(rango_data[2]))})
  output$min_graf_cam <- renderText({str_to_sentence(as.character(rango_data[1]))})
  output$max_graf_cam <- renderText({str_to_sentence(as.character(rango_data[2]))})
  output$min_graf_ven <- renderText({str_to_sentence(as.character(rango_data[1]))})
  output$max_graf_ven <- renderText({str_to_sentence(as.character(rango_data[2]))})
  output$min_graf_mue <- renderText({str_to_sentence(as.character(rango_data[1]))})
  output$max_graf_mue <- renderText({str_to_sentence(as.character(rango_data[2]))})
  # output$titulo_SEIR <-renderText({  paste0("Dinámica SEIR (", poblacion_data$label[poblacion_data$pais==pais_actual][1],")") })
  # output$min_graf_SEIR <- renderText({str_to_sentence(as.character(rango_data[1]))})
  # output$max_graf_SEIR <- renderText({str_to_sentence(as.character(rango_data[2]))})
  
  color_dyg = c("#3a3a3b","#ff9191","#8ae691")
  
  
  ##### Infecciones
  observe({
    
    if(input$que_infecc=="Diarias"){
      df <- cbind(modeloSimulado %>% dplyr::select(fecha, i = i_5d_ma),
                  modeloSimulado_hi %>% dplyr::select(i2 = i_5d_ma),
                  modeloSimulado_low %>% dplyr::select(i3 = i_5d_ma)) 
      labels = c("Nuevos Inf.","Peor escenario","Mejor escenario")
      addPopover(session, 
                 "info_I", "Nuevos infectados estimados totales por día", 
                 content = paste0("El número de nuevos infectados estimados totales 
                              por día incluye los detectados y los no detectados 
                              (no testeados). Esto se calcula según el modelo SEIR 
                              ajustado para todo el período de la proyección."),  
                 trigger = 'click')
      output$titulo_infecciones <-renderText({  paste0("Nuevos infectados estimados totales por día (", 
                                                       poblacion_data$label[poblacion_data$pais==pais_actual][1],")") })
    }else{
      df <- cbind(modeloSimulado %>% mutate(I = I+R) %>% dplyr::select(fecha, i = I),
                  modeloSimulado_hi %>% mutate(I = I+R) %>% dplyr::select(i2 = I),
                  modeloSimulado_low %>% mutate(I = I+R) %>% dplyr::select(i3 = I))
      labels = c("Infectados acumulados","Peor escenario","Mejor escenario")
      addPopover(session, 
                 "info_I", "Infectados estimados acumulados a cada día", 
                 content = paste0("El número de infectados acumulados 
                              incluye los detectados y los no detectados 
                              (no testeados), independientemente de si luego
                               fueron recuperados o muertos"),  
                 trigger = 'click')
      output$titulo_infecciones <-renderText({  paste0("Infectados estimados acumulados (", 
                                                       poblacion_data$label[poblacion_data$pais==pais_actual][1],")") })
    }
    if(trigger_on_app==1){df <- df %>% dplyr::select(fecha, i)}
    dg <- xts(df[,-1], order.by=as.Date(df[,1]))
    
    output$g_i <- renderDygraph({
      dy <- dygraph(dg, group = "grupo") %>% 
        dySeries(c(names(dg[,c(1)])), label = labels[1], strokeWidth = 3,  color = color_dyg[1]) %>%
        dyOptions(drawGrid = FALSE, labelsKMB=F,
                  maxNumberWidth=10, rightGap=100, digitsAfterDecimal=0) %>% 
        dyAxis("y", label = labels[1], 
               valueRange = c(0, max(dg)*1.1)) %>% 
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 1,
                    hideOnMouseOut = FALSE) %>% 
        dyRangeSelector(height = 40, strokeColor = "", dateWindow = range_graf_select)%>% 
        dyLegend(width = 400) %>% 
        dyEvent(x = hoy, label = "Hoy", labelLoc = "bottom", 
                color = "grey", strokePattern = "dashed")
      if(trigger_on_app==0){
        dy <- dy %>% 
          dySeries(c(names(dg[,c(2)])), label = labels[2], strokeWidth = 1,
                 strokePattern = "dashed",  color = color_dyg[2]) %>%
          dySeries(c(names(dg[,c(3)])), label = labels[3], strokeWidth = 1,
                   strokePattern = "dashed",  color = color_dyg[3])
      }
      if(trigger_on_app==1 & !is.null(fechaIntervencionesTrigger)){
        for(i in 1:length(fechaIntervencionesTrigger)){
          dy <- dy %>% dyShading(from = as.Date(fechaIntervencionesTrigger[i]),
                                 to = as.Date(fechaIntervencionesTrigger[i])+Dias_interv)
        }
      }
      dy
    })
  })
  

##### Defunciones
  observe({
    
    if(input$que_defs == "Diarias" | input$que_defs == "Acumuladas"){
      df <- cbind(fecha = modeloSimulado$fecha,
                  m = modeloSimulado$muertes_smooth,
                  modeloSimulado_hi %>% dplyr::select(m2 = muertesDiariasProyeccion),
                  modeloSimulado_low %>% dplyr::select(m3 = muertesDiariasProyeccion),
                  m_real = modeloSimulado$muertesDiariasReal)
      

      labels = c("Defunciones Diarias","Peor escenario","Mejor escenario","Reportadas")
      addPopover(session,
                 "info_D", "Nuevas defunciones estimadas por día",
                 content = paste0("El número de defunciones estimado para
                              cada día del período proyectado, según
                              el modelo SEIR ajustado."),
                 trigger = 'click')
      output$titulo_defunciones <-renderText({  paste0("Nuevas defunciones estimadas por día (", poblacion_data$label[poblacion_data$pais==pais_actual][1],")") })

    }

    if(input$que_defs=="Acumuladas"){
      df$m[!is.na(df$m)] <- cumsum(df$m[!is.na(df$m)])
      df$m2[!is.na(df$m2)] <- cumsum(df$m2[!is.na(df$m2)])
      df$m3[!is.na(df$m3)] <- cumsum(df$m3[!is.na(df$m3)])
      df$m_real[!is.na(df$m_real)] <- cumsum(df$m_real[!is.na(df$m_real)])
      df[which(as.Date(modeloSimulado$fecha)<hoy), 3] = NA
      df[which(as.Date(modeloSimulado$fecha)<hoy), 4] = NA

      labels = c("Defunciones Acumuladas","Peor escenario","Mejor escenario","Reportadas")
      addPopover(session,
                 "info_D_acum", "Defunciones estimadas Acumuladas",
                 content = paste0("El número de defunciones estimado
                              que se se acumula a cada día, según
                              el modelo SEIR ajustado."),
                 trigger = 'click')
      output$titulo_defunciones <-renderText({  paste0("Defunciones acumuladas estimadas (", poblacion_data$label[poblacion_data$pais==pais_actual][1],")") })
    }

    df[which(as.Date(modeloSimulado$fecha)<hoy), 3] = NA
    df[which(as.Date(modeloSimulado$fecha)<hoy), 4] = NA
    if(trigger_on_app==1){df <- df %>% dplyr::select(fecha, m, m_real)}
    
    dg <- xts(df[,-1], order.by=as.Date(df[,1]))

    output$g_d <- renderDygraph({
      dy <- dygraph(dg, group = "grupo") %>%
        dySeries(c(names(dg[,1])), label = labels[1], strokeWidth = 3,  color = color_dyg[1]) %>%
        dySeries(c(names(dg[,ifelse(trigger_on_app==1,2,4)])), label = labels[4], stepPlot = T,
                 fillGraph = TRUE, color = "#c3c3c7") %>%
        dyOptions(drawGrid = FALSE, labelsKMB=F,
                  maxNumberWidth=10, rightGap=100, digitsAfterDecimal=0) %>%
        dyAxis("y", label = labels[1],
               valueRange = c(0, max(dg,na.rm = T)*1.1)) %>%
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 1,
                    hideOnMouseOut = FALSE) %>%
        dyRangeSelector(height = 40, strokeColor = "", dateWindow = range_graf_select)%>%
        dyLegend(width = 600) %>%
        dyEvent(x = hoy, label = "Hoy", labelLoc = "bottom",
                color = "grey", strokePattern = "dashed")

      if(trigger_on_app==0){
        dy <- dy %>%
          dySeries(c(names(dg[,c(2)])), label = labels[2], strokeWidth = 1,
                   strokePattern = "dashed",  color = color_dyg[2]) %>%
          dySeries(c(names(dg[,c(3)])), label = labels[3], strokeWidth = 1,
                   strokePattern = "dashed",  color = color_dyg[3])
      }
      if(trigger_on_app==1 & !is.null(fechaIntervencionesTrigger)){
        for(i in 1:length(fechaIntervencionesTrigger)){
          dy <- dy %>% dyShading(from = as.Date(fechaIntervencionesTrigger[i]),
                                 to = as.Date(fechaIntervencionesTrigger[i])+Dias_interv)
        }
      }
      dy
    })
})

# camas / ventiladores / médicos / enfs
  observe({
    if(input$que_rrhh == "Camas"){
      df <- cbind(modeloSimulado %>% dplyr::select(fecha, v = HHRR.criticCareBeds),
                  modeloSimulado_hi %>% dplyr::select(v2 = HHRR.criticCareBeds),
                  modeloSimulado_low %>% dplyr::select(v3 = HHRR.criticCareBeds)) %>% 
        dplyr::rename(Fecha=1) %>% as.data.frame()
      labels = c("Camas Críticas","Peor escenario","Mejor escenario")
      addPopover(session, 
                 "info_C", "Utilización de camas de uso crítico - UCI", 
                 content = paste0("El número de camas de uso crítico
                              requeridas para cada día de la proyección. 
                              La ocupación se calcula
                              comparando con los parámetros de capacidad
                              especificados para cada país, que pueden 
                              modificarse en esta herramienta para
                              recalcular los resultados."), 
                 trigger = 'click')
      output$titulo_camas_vents <-renderText({  paste0("Camas UCI: utilización diaria estimada (", poblacion_data$label[poblacion_data$pais==pais_actual][1],")") })
    } else if(input$que_rrhh == "Ventiladores") {
      df <- cbind(modeloSimulado %>% dplyr::select(fecha, v = HHRR.ventilators),
                  modeloSimulado_hi %>% dplyr::select(v2 = HHRR.ventilators),
                  modeloSimulado_low %>% dplyr::select(v3 = HHRR.ventilators)) %>% 
        dplyr::rename(Fecha=1) %>% as.data.frame()
      labels = c("Ventiladores","Peor escenario","Mejor escenario")
      addPopover(session, 
                 "info_V", "Utilización de Ventiladores / Respiradores", 
                 content = paste0("El número de dispositivos de soporte respiratorio
                              requeridos para cada día de la proyección. 
                              La ocupación se calcula
                              comparando con los parámetros de capacidad
                              especificados para cada país, que pueden 
                              modificarse en esta herramienta para
                              recalcular los resultados."), 
                 trigger = 'click')
      output$titulo_camas_vents <-renderText({  paste0("Ventiladores: utilización diaria estimada (", poblacion_data$label[poblacion_data$pais==pais_actual][1],")") })
  } else if(input$que_rrhh == "Médicos") {
    df <- cbind(modeloSimulado %>% dplyr::select(fecha, v = HHRR.criticCarePhysicans),
                modeloSimulado_hi %>% dplyr::select(v2 = HHRR.criticCarePhysicans),
                modeloSimulado_low %>% dplyr::select(v3 = HHRR.criticCarePhysicans)) %>% 
      dplyr::rename(Fecha=1) %>% as.data.frame()
    labels = c("Médicas/os","Peor escenario","Mejor escenario")
    addPopover(session, 
               "info_V", "Médicas/os en cuidados críticos: dedicación diaria", 
               content = paste0("El número de médicos/as
                                requeridos para cada día de la proyección. 
                              La ocupación se calcula
                              comparando con los parámetros de capacidad
                              especificados para cada país, que pueden 
                              modificarse en esta herramienta para
                              recalcular los resultados."), 
               trigger = 'click')
    output$titulo_camas_vents <-renderText({  paste0("Médicas/os en cuidados críticos: dedicación diaria (", poblacion_data$label[poblacion_data$pais==pais_actual][1],")") })
  } else if(input$que_rrhh == "Enfermeras") {
    df <- cbind(modeloSimulado %>% dplyr::select(fecha, v = HHRR.criticCareNurses),
                modeloSimulado_hi %>% dplyr::select(v2 = HHRR.criticCareNurses),
                modeloSimulado_low %>% dplyr::select(v3 = HHRR.criticCareNurses)) %>% 
      dplyr::rename(Fecha=1) %>% as.data.frame()
    labels = c("Enfermeras/os","Peor escenario","Mejor escenario")
    addPopover(session, 
               "info_V", "Enfermeras/os en cuidados críticos: dedicación diaria", 
               content = paste0("El número de enfermeros/as
                                requeridos para cada día de la proyección. 
                                La dedicación se calcula
                                comparando con los parámetros de capacidad
                                especificados para cada país, que pueden 
                                modificarse en esta herramienta para
                                recalcular los resultados."), 
               trigger = 'click')
    output$titulo_camas_vents <-renderText({  paste0("Enfermeras/os en cuidados críticos: dedicación diaria (", poblacion_data$label[poblacion_data$pais==pais_actual][1],")") })
  }
    
    if(trigger_on_app==1){df <- df %>% dplyr::select(1:2)}
    
    # 
    
    dg <- xts(df[,-1], order.by=as.Date(df[,1]))
    
    output$g_Beds_Vents <- renderDygraph({
      
      dy <- dygraph(dg, group = "grupo") %>% 
        dySeries(c(names(dg[,c(1)])), label = labels[1], 
                 strokeWidth = 3,  color = color_dyg[1]) %>%
        dyOptions(drawGrid = FALSE, labelsKMB=F,
                  maxNumberWidth=10, rightGap=100, digitsAfterDecimal=0) %>% 
        dyAxis("y", label = labels[1], 
               valueRange =  c(0, ifelse(input$que_rrhh=="Camas",
                                         max(max(modeloSimulado_hi$HHRR.criticCareBeds),camasCriticas),
                                         ifelse(input$que_rrhh=="Ventiladores",
                                                max(max(modeloSimulado_hi$HHRR.ventilators),ventiladores),
                                         ifelse(input$que_rrhh=="Médicos",
                                               max(max(modeloSimulado_hi$HHRR.criticCarePhysicans),medicosCamasUCI),
                                               max(max(modeloSimulado_hi$HHRR.criticCareNurses),enfermerasCamasUCI))))* 1.2))%>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 1,
                    hideOnMouseOut = FALSE) %>% 
        dyRangeSelector(height = 40, strokeColor = "", dateWindow = range_graf_select)  %>% 
        dyLegend(width = 400) %>% 
        dyEvent(x = hoy, label = "Hoy", labelLoc = "bottom", 
                color = "grey", strokePattern = "dashed") %>% 
        dyLimit(ifelse(input$que_rrhh=="Camas", camasCriticas, 
                       ifelse(input$que_rrhh=="Ventiladores", ventiladores,
                              ifelse(input$que_rrhh=="Médicos", medicosCamasUCI,
                                     enfermerasCamasUCI))) * porcentajeDisponibilidadCamasCOVID, 
                color = "red", label = "Disponibles para COVID",strokePattern = "dotted")
      
      if(trigger_on_app==0){
        dy <- dy %>% 
          dySeries(c(names(dg[,c(2)])), label = labels[2], strokeWidth = 1,
                   strokePattern = "dashed",  color = color_dyg[2]) %>%
          dySeries(c(names(dg[,c(3)])), label = labels[3], strokeWidth = 1,
                   strokePattern = "dashed",  color = color_dyg[3])
      }
      if(trigger_on_app==1 & !is.null(fechaIntervencionesTrigger)){
        for(i in 1:length(fechaIntervencionesTrigger)){
          dy <- dy %>% dyShading(from = as.Date(fechaIntervencionesTrigger[i]),
                                 to = as.Date(fechaIntervencionesTrigger[i])+Dias_interv)
        }
        if(input$que_rrhh == "Camas"){
          dy <- dy %>% dyLimit(camasCriticas * porcentajeDisponibilidadCamasCOVID * trigger_Porc_crit/100, 
                               color = "#8338ec", label = "Camas de gatillo",strokePattern = "dotted")
        }
      }
      dy
    })
    
    
   
  })


  # SEIR
  # output$g_SEIR <- renderDygraph({
  # probabilisticoSEIR <- modeloSimulado %>%
  #   mutate(i = i_5d_ma,
  #          i_serieP10 = i * 0.5,
  #          i_serieP90 = i * 1.5,
  #          S = as.integer(S),
  #          S_serieP10 = S * 0.5,
  #          S_serieP90 = S * 1.5,
  #          R = as.integer(R),
  #          R_serieP10 = R * 0.5,
  #          R_serieP90 = R * 1.5,
  #          E = as.integer(E),
  #          E_serieP10 = E * 0.5,
  #          E_serieP90 = E * 1.5) %>%
  #   dplyr::select(fecha,i,S,R, E,33:40) %>%
  #   dplyr::rename(Fecha=1) %>% as.data.frame()
  # dg <- xts(probabilisticoSEIR[,-1],
  #           order.by=as.Date(probabilisticoSEIR[,1]))
  # dygraph(dg[,c("S","E","R","i")]) %>%
  #   dySeries("i", axis = "y2") %>%
  #   dySeries("E", axis = "y2") %>%
  #   dyEvent(x = hoy,label = "Hoy", labelLoc = "bottom",
  #           color = "grey", strokePattern = "dashed") %>%
  #   dyOptions(drawGrid = FALSE, labelsKMB=F,
  #             maxNumberWidth=10, rightGap=100, digitsAfterDecimal=0) %>%
  #   dyAxis("y", label = "Acum.") %>%
  #   dyAxis("y2", label = "Nuevos Inf.") %>%
  #   dyHighlight(highlightCircleSize = 5,
  #               highlightSeriesBackgroundAlpha = 1,
  #               hideOnMouseOut = FALSE)
  # })
  # addPopover(session, 
  #            "info_SEIR", "Utilización de camas de uso crítico - UCI", 
  #            content = paste0("El número de camas de uso crítico
  #                             requeridas para cada día de la proyección. 
  #                             La ocupación se calcula
  #                             comparando con los parámetros de capacidad
  #                             especificados para cada país, que pueden 
  #                             modificarse en esta herramienta para
  #                             recalcular los resultados."), 
  #            trigger = 'click')
}


# variables inputs --------------------------------------------------------

vars_inputs = data.frame(
  Input = c('Periodo preinfeccioso promedio (días)',
            'Duración media de la infecciosidad (días)',
            'Porcentaje de casos severos',
            'Porcentaje de casos críticos',
            'Días con síntomas antes de la hospitalización',
            'Días de hospitalización para casos severos',
            'Días de hospitalización para casos críticos',
            'Días de la UCI para casos críticos',
            'Tasa de letalidad (IFR)',
            'Días desde el primer informe hasta la dinámica de la muerte: nuevo modelo',
            'Tiempo desde el final de la incubación hasta la muerte',
            'Retraso para el impacto de la política (días)',
            'Camas generales atendidas enfermera por día / número de turnos',
            'Las camas de la UCI atendieron a la enfermera por día / número de turnos',
            'Camas generales atendidas por día médico / número de turnos',
            'Camas CC atendidas por día médico / número de turnos',
            'Ventiladores por cama crítica',
            'Población',
            'Camas generales',
            'Cama críticas',
            'Ventiladores',
            'Enfermeras por camas generales',
            'Enfermeras por camas UCI',
            'Médicos por camas generales',
            'Médicos por camas UCI',
            'Porcentaje de disponibilidad de camas para COVID-19'),
  variable = c("periodoPreinfPromedio",
               "duracionMediaInf",
               "porcentajeCasosGraves",
               "porcentajeCasosCriticos",
               "diasSintomasAntesHosp",
               "diasHospCasosGraves",
               "diasHospCasosCriticos",
               "diasUCICasosCriticos",
               "tasaLetalidadAjustada",
               "diasPrimerInformeMuerte",
               "diasIncubacionMuerte",
               "retrasoImpactoPolitica",
               "camasGeneralesEnfermeraDia",
               "camasUCIEnfermerasDia",
               "camasGeneralesMedicoDia",
               "camasCCMedicoDia",
               "ventiladoresCamaCritica",
               "Población",
               "camasGenerales",
               "camasCriticas",
               "ventiladores",
               "enfermerasCamasGenerales",
               "enfermerasCamasUCI",
               "medicosCamasGenerales",
               "medicosCamasUCI",
               "porcentajeDisponibilidadCamasCOVID")
)
