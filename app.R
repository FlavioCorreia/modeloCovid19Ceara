library(shiny)
library(tidyverse)
library(readxl)

if (!require("timevis"))
  #install.packages("timevis")
  remotes::install_github("daattali/timevis")
  
library(timevis)

library(dygraphs)
library(shinythemes)
library(shinydashboard)
library(DT)
library(shinyalert)
library(shinyWidgets)
library(lubridate)
library(shinyjs)
library(leaflet)
library(shinyBS)
library(shinycssloaders)
library(RColorBrewer)
library(reader)
library(kableExtra)
library(knitr)
library(sp)
library(tinytex)

#setwd("C:/Users/Adrian/Desktop/modelocovid19-cearaModel - No Map 2")

versionModelo <<- "2.6"

# funs --------------------------------------------------------------------
hoy <<- as.Date("2020-11-18")
default <<- FALSE
# setwd("appTest - Cod")
source("modulos.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
load("DatosIniciales/Map.RData")
load("DatosIniciales/oms_data.RData")
load("DatosIniciales/owd_data.RData")

# para loop sliders
cantidadMeses = 12
fechaInicio <- ymd(hoy)
listaFechas <- fechaInicio %m+% months(c(0:cantidadMeses))
tiempoInicio <- Sys.time()

# semaforo
colores = c("#FF2929","#FF4D27","#FF7025","#FF9424","#FFB822","#FFDB20",
            "#FFFF1E","#C7FF22","#8FFF26","#56FF29","#1EFF2D")

# App ---------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  tags$head(HTML('<link rel="icon", href="ISO-IECS.png", type="image/png" />')),
  tags$head(includeHTML("www/google-analytics.html")),
  titlePanel(windowTitle = "IECS: Proyecciones COVID-19", title = ""),
  fluidRow(
    column(4,
           fluidRow(
             column(12,
                    tags$a(
                      #img(src="https://integrasus.saude.ce.gov.br/assets/img/logo_integrasus.png", height = 100, width = 300),
                      img(src="https://www.iecs.org.ar/wp-content/uploads/iecslogo.png", height = 100, width = 300),
                      href="https://www.iecs.org.ar",
                      target="_blank"
                    )
             )
           ),
           fluidRow(
             column(4, style="padding-top: 15px;",
                    tags$a(
                      img(src="etslogo2.png", height = 65, width = 130),
                      href="https://www.iecs.org.ar/evaluacion-de-tecnologias-sanitarias-y-economia-de-la-salud/",
                      target="_blank"
                    ),
             ),
             column(4, style="padding-top: 11px;",
                    tags$a(
                      img(src="https://integrasusanalytics.saude.ce.gov.br/pt/assets/images/logo_integrasus_analytics.png", height = 65, width = 180),
                      href="https://integrasus.saude.ce.gov.br/",
                      target="_blank"
                    ),
             ),
             column(4, style="padding-top: 3px;",
                    tags$a(
                      img(src="CIPSlogo.png", height = 88, width = 130),
                      href="https://www.iecs.org.ar/ciips/",
                      target="_blank"
                    ),
             )
           )
    ),
    column(8, 
           h1("Impacto del COVID-19 en los sistemas de salud de Latinoamérica y el Caribe"),
           h3("Proyecciones para la toma de decisiones públicas"),
           h3("Adaptación al Estado del Ceará")
           
    )
  ),
  fluidRow(
    column(1, style="padding-top: 20px;",
           tags$a(
             img(src="bid-logo3.png", height = 42, width = 105),
             href="https://www.iadb.org/es",
             target="_blank"
           ),
    ),
    column(3,
           p(style = "font-size: 14px; margin-top: 15px;", 
             HTML(paste0("El desarrollo de la metodología que da soporte a 
                           esta herramienta fue posible gracias al apoyo del 
                           Banco Interamericano de Desarrollo (BID) bajo el 
                           Contrato RG-E1696-P001"))),
    ),
    column(8,
           # class = "text-center",
           p(style = "margin-bottom: 1px; font-style: italic;", 
             "Departamento de Evaluación de Tecnologías Sanitarias y Economía de la Salud"),
           p(style = "margin-bottom: 1px; font-style: italic;", 
             "Centro de Implementación e Innovación en Políticas de Salud"),
           p(style = "margin-bottom: 1px; font-style: italic;", 
             "Instituto de Efectividad Clínica y Sanitaria, Buenos Aires, Argentina"),
           p(style = "margin-bottom: 1px;  font-style: italic;", 
             a("Más información sobre el modelo", href="https://www.iecs.org.ar/modelocovid/", target="_blank"),
             "-",
             a("Consultas y feedback", href="https://www.iecs.org.ar/modelocovid-formulario/", target="_blank")
           )
    ),
  ),
  hr( style = "margin-bottom: 0px;"),
  
# load data and seleccionar país ------------------------------------------

    fluidRow( align = "center",
              tags$head(tags$style(HTML(".selectize-input.pais {height: 45px; width: 500px; font-size: 30px;}"))),
              selectInput(inputId = "pais","",selected = "ARG", width = "400px",
                          c(#"Argentina" = "ARG",
                            #"Bahamas" = "BHS",
                            #"Barbados" = "BRB",
                            #"Belize" = "BLZ",
                            #"Bolivia" = "BOL",
                            #"Brasil" = "BRA",
                            "Brasil - Ceara" ="CEA"
                            #"Chile" = "CHL",
                            #"Colombia" = "COL",
                            #"Costa Rica" = "CRI",
                            #"El Salvador" = "SLV",
                            #"Ecuador" = "ECU",
                            #"Guatemala" = "GTM",
                            #"Guyana" = "GUY",
                            #"Haití" = "HTI",
                            #"Honduras" = "HND",
                            #"Jamaica" = "JAM",
                            #"Mexico" = "MEX",
                            #"Nicaragua" = "NIC",
                            #"Panamá" = "PAN",
                            #"Paraguay" = "PRY",
                            #"Perú" = "PER",
                            #"República Dominicana" = "DOM",
                            #"Suriname" = "SUR",
                            #"Trinidad y Tobago" = "TTO",
                            #"Uruguay" = "URY",
                            #"Venezuela" = "VEN"
                            ))
    ),
#     fluidRow(
#       column(5, offset = 1,
#         wellPanel(
#           style = "height:330px;",
# 
#           #FLAVIO COMENTOU
#            conditionalPanel(
#              condition = "input.pais == 'ARG_2' ||
#                              input.pais == 'ARG_50' ||
#                              input.pais == 'ARG_3' ||
#                              input.pais == 'ARG_7'  ",
#              em("Ultimos datos disponibles (fuentes ",
#                 tags$a(href="https://www.ecdc.europa.eu/en", "ECDC",target="_blank"), ", ",
#                 tags$a(href="https://ourworldindata.org/", "OWD", target="_blank"),  ", ",
#                 tags$a(href="http://datos.salud.gob.ar/dataset/covid-19-casos-registrados-en-la-republica-argentina",
#                          "MSAL", target="_blank"),
#                 ")",
#                 tags$span(id="warning_data", icon("info-circle"),
#                           style=("margin-left:5px;"))
#           
#            )),
# 
#            conditionalPanel(
#              condition = "input.pais != 'ARG_2' &&
#                              input.pais != 'ARG_50' &&
#                              input.pais != 'ARG_3' &&
#                              input.pais != 'ARG_7'  ",
#              em("Ultimos datos disponibles (fuentes ",
#                 tags$a(href="https://www.ecdc.europa.eu/en", "ECDC",target="_blank"), ", ",
#                 tags$a(href="https://ourworldindata.org/", "OWD", target="_blank"), ", ",
#                 tags$a(href="https://covid19.who.int/table", "OMS", target="_blank"),
#                 ")",
#                 tags$span(id="warning_data", icon("info-circle"),
#                           style=("margin-left:5px;"))
#            )),
# 
#           br(),br(),
# 
#           fluidRow(
#             column(
#               4,
#               tags$b("Población:"),tags$p(textOutput("poblacion")),
#             ),
#             column(
#               4,
#               tags$b("Casos confirm. diarios:",
#                      tags$span(id="info_Inf_diarios", icon("info-circle"),
#                                 style=("margin-left:5px;"))),
#               tags$p(textOutput("inf_ayer")),
#             ),
#             column(
#               4,
#               tags$b("Muertes confirmadas diarias:",
#                      tags$span(id="info_Mue_diarios", icon("info-circle"),
#                                style=("margin-left:5px;"))),
#               tags$p(textOutput("muertes_ayer")),
#             )
#           ),
#           fluidRow(
#             column(
#               4,
#               tags$b("% de mayores de 65 años:"),tags$p(textOutput("poblacion65mas")),
#             ),
#             column(
#               4,
#               tags$b("Total casos confirmados:"),tags$p(textOutput("inf_acum")),
#             ),
#             column(
#               4,
#               tags$b("Total muertes confirmadas:"),tags$p(textOutput("muertes_acum")),
#             )
#           ),
#           fluidRow(
#             column(
#               4,
#               tags$b("Esperanza de vida al nacer:"),tags$p(textOutput("e0"))
#             ),
#             column(
#               4,
#               tags$b("Tests Totales c/ mill. hab."),tags$p(textOutput("test_acum"))
#             ),
#             column(
#               4,
#               tags$b("Tests diarios (últ. dato disp.)"),tags$p(textOutput("new_tests"))
#             )
#           )
#         )
#       ),
# 
#     # map some index
# 
# #FLAVIO EDITOU
# column(5,
#        leafletOutput("mymap", width = "90%", height = 330) ,
#        bsTooltip("mymap", "Mapa de calor: Defunciones acumuladas cada millón de habitantes al día de ayer:",
#                  "right", options = list(container = "body"))
#   )
# 
#     ),
  br(),
hr(),

# Gráficos ----------------------------------------------------------------

  h3(id="inicio", textOutput("pais")),
  fluidRow(
    column(6,
           h4(em(textOutput("default_o_no"))),
           p("La fecha de última actualización de datos es el", format(hoy,"%d-%m-%Y"),"."),
           br(),
           p("Con esta plataforma interactiva es posible:", style="text-decoration: underline;"),
           p(" - Personalizar la proyección y crear un escenario nuevo ", tags$a(href="#params", "aquí")),
           p(" - Seleccionar un ",
              actionLink("mostrarEscenarios", "escenario pre-definido"),":"),
           radioButtons("tipo_config",
                        label = "Con esta plataforma interactiva es posible:",
                        choices = c("Personalizar la proyección y crear un escenario nuevo." = "nuevo",
                                    "Seleccionar un escenario pre-definido" = "predefinido"),
                        selected = "",
                        width = "100%"),
           conditionalPanel("input.tipo_config=='nuevo'",
                            column(11, offset=1,
                                   tags$a(href="#params", "Configure los parámetros del nuevo escenario aquí")
                            )
                            
           ),
           conditionalPanel("input.tipo_config=='predefinido'",
                            column(11, offset=1,
                                   selectInput(inputId = "escenarioPredefinido","",selected = "constanteR_cori",
                                               width="100%",
                                               c(
                                                 "Políticas con nivel de rigurosidad estimado actual (escenario por defecto)" = "constanteR_cori",
                                                 "Políticas con nivel de rigurosidad constante, R0 1,3. (Ej. apertura de negocios y escuelas)" = "constante1,3",
                                                 "Política adaptativa basada en ocupación de camas, cuarentena al llegar al 50%" = "trigger50",
                                                 "Política adaptativa basada en ocupación de camas, cuarentena al llegar al 70%" = "trigger70",
                                                 "Política valvular intermitente, alterna cuarentena y disminución de restricciones en ciclos mensuales" = "valvular1"
                                               )
                                   ),
                                   conditionalPanel("input.escenarioPredefinido=='constanteR_cori'",
                                                    htmlOutput("R_hoy_predef")
                                   ),
                                   conditionalPanel("input.escenarioPredefinido=='constante1,3'",
                                                    p("Este escenario tiene un R0 constante de 1,3 durante toda el período
                              de proyección. Un ejemplo de este nivel de restricción son las medidas
                              de distanciamiento social con apertura de negocios y escuelas con
                              protocolos definidos.")
                                   ),
                                   conditionalPanel("input.escenarioPredefinido=='trigger50'",
                                                    p("Este escenario tiene un R0 base de 1,3 y cuando se alcanza el 50% de
                              ocupación de camas de cuidados intensivos se inicia una cuarentena
                              estricta con R0 de 1,0. La cuarentena se prolonga por 30 días y se renueva si el
                              nivel de ocupación aún no bajó del 50%. Al lograrse el descenso deseado se
                              levantan las restricciones y se vuelve al escenario base de un R0 de 1,3.")
                                   ),
                                   conditionalPanel("input.escenarioPredefinido=='trigger70'",
                                                    p("Este escenario tiene un R0 base de 1,3 y cuando se alcanza el 70% de
                              ocupación de camas de cuidados intensivos se inicia una cuarentena
                              estricta con R0 de 1,0. La cuarentena se prolonga por 30 días y se renueva si el
                              nivel de ocupación aún no bajó del 70%. Al lograrse el descenso deseado se
                              levantan las restricciones y se vuelve al escenario base de un R0 de 1,3.")
                                   ),
                                   conditionalPanel("input.escenarioPredefinido=='valvular1'",
                                                    p("Este escenario tiene alterna a lo largo de todo el período
                              proyectado períodos de 30 días con políticas restrictivas que resultan
                              en un R0 de 1,1, y 30 días con políticas mas relajadas que resultan en
                              un r0 de 1.3.")
                                   ),
                                   fluidRow(
                                      actionButton("aplicarEscenario", "Aplicar escenario",
                                                   style= "float: right; margin: 10px;"),
                                   ),
                            ),
           ),
    ),
     column(6,
            wellPanel(h5("R0 según escenario vigente en proyección:"),
                      dygraphOutput("g_R_actual", 
                                    width = "100%", height = "200px"))
    )
  ),
  br(),
  hr(),

  h4("Gráficos de resultados"),
  fluidRow(
    column(12,
           Grafica("graficos") 
    )
  ),
  
  hr(),

# Tabla Resultados --------------------------------------------------------
  h4(textOutput("titulo_tabla_resultados", inline=TRUE)),
  h5("Los resultados esperados según la proyección son los siguientes:"),
  fluidRow( class = "text-center",
    column(12,
             DTOutput("tableResults", width = 1000))
    ),
    fluidRow( 
      column(12,
             downloadButton("report", "Reporte")
  )),
  hr(),

# Parámetros --------------------------------------------------------------
  
  # tabs
  h3("Parámetros"),

    fluidRow(
    column(12,
           tags$head(tags$style("#carrito{color: #ff6262;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"
                    )
           ),
           div(
               actionButton("updateResults", "Actualizar resultados"),
               
               htmlOutput("carrito"),
               
             style="float:right")
           
           )
    ),
    # colores de semáforo
    tags$style(
      paste0(
      ".vis-item.r11 {color: #222426;background-color:",colores[1] ,";}
       .vis-item.r10 {color: #222426;background-color:",colores[2] ,";}
       .vis-item.r9  {color: #222426;background-color:",colores[3] ,";}
       .vis-item.r8  {color: #222426;background-color:",colores[4] ,";}
       .vis-item.r7  {color: #222426;background-color:",colores[5] ,";}
       .vis-item.r6  {color: #222426;background-color:",colores[6] ,";}
       .vis-item.r5  {color: #222426;background-color:",colores[7] ,";}
       .vis-item.r4  {color: #222426;background-color:",colores[8] ,";}
       .vis-item.r3  {color: #222426;background-color:",colores[9] ,";}
       .vis-item.r2  {color: #222426;background-color:",colores[10] ,";}
       .vis-item.r1  {color: #222426;background-color:",colores[11] ,";}")),

    tabsetPanel(id="params",
        tabPanel("Intervenciones de Salud Pública",
                 tags$head(tags$style("#R_hoy{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
                 ),
                 h5(em(HTML(paste0(textOutput("R_hoy", inline=TRUE),
                                   " en ",
                                   textOutput("pais2", inline=TRUE),
                                   " según ",
                                   tags$a(href="https://www.iecs.org.ar/wp-content/uploads/Modelo-COVID_metodologia.pdf",
                                          "metodología adoptada",target="_blank"),
                                   ". Otras estimaciones ",
                                   tags$a(href="https://epiforecasts.io/covid/reports.html#Americas", 
                                          "disponibles.",target="_blank"))))),
                 h5(em(HTML(paste0("Lo anterior equivale a ",
                                   textOutput("R0_hoy", inline = T), ".")))),
                 h5(em(HTML(paste0("Los días de duplicación de casos reportados se estiman en ",
                                   textOutput("dias_dupl", inline = T))))),
            br(),
            fluidRow(
              column(7,
                   radioButtons("tipo_interv",
                            label = "Definir mediante:",
                            choices = c("R0 asociados a políticas de intervención: seleccionando niveles de restricción para cada mes 
                                        de un menú de opciones simplificado." = "politicas",
                                        "Ingreso de R0 específicos: especificando el nivel de R0 para cada período, ajustable a meses, 
                                        quincenas o semanas, con herramientas avanzadas." = "R0",
                                        "Configurar intervenciones gatilladas por la ocupación de camas en UCI (Adaptive triggering)"="trigger"),
                            selected = "",
                            width = "100%")
                   )
            ), # row
            fluidRow(
                  # set R con semáforo
                  conditionalPanel("input.tipo_interv=='politicas'",
                    fluidRow(
                      column(12,
                             h4("Políticas de intervención", 
                             tags$span(id="info_politicas", icon("info-circle"), 
                                       style=("margin-left:5px;")),
                             style = "margin-left: 15px;")
                      )
                    ),
                    column(7,
                       wellPanel(
                         fluidRow(
                           column(1, 
                                  tags$span(id="sin_medidas", icon("users", "fa-3x")), align="center"),
                           column(2, 
                                  tags$span(id="barbijos", icon("medkit", "fa-3x")), align="center"),
                           column(3,
                                  tags$span(id="dist_social", icon("street-view", "fa-3x")), align="center"),
                           column(4, 
                                  tags$span(id="escuelas", icon("school", "fa-3x")), align="center"),
                           column(1, 
                                  tags$span(id="cierre_completo", icon("lock", "fa-3x")), align="center"),
                         ),
                         fluidRow( style="margin-bottom: 10px; margin-top: 5px;",
                           column(1, align="center",
                                  tags$span("Sin medidas")),
                           column(2, align="center",
                                  tags$span("Medidas mínimas")),
                           column(3, align="center",
                                  tags$span("Agregado de restricciones de reuniones y distanciamiento social")),
                           column(4, align="center",
                                  tags$span("Agregado de cierre de negocios y escuelas; uso estricto y extendido de tapabocas, ampliación del testeo y rastreo de contactos")),
                                  # bsTooltip("escuelas", "Cierre de escuelas y aislamiento selectivo",options = list(container = "body")),
                           column(1, align="center",
                                  tags$span("Cuarentena total")),
                         ),
                         fluidRow( style="margin-bottom: 10px;",
                         column(1,actionButton("r11",  HTML("<b>3,7</b>"),
                                                style=paste0("color: #222426; background-color: ",colores[1],"; border-color: #2e6da4"))),
                         column(1,actionButton("r10",  HTML("<b>3,3</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[2],"; border-color: #2e6da4"))),
                         column(1,actionButton("r9",  HTML("<b>3,0</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[3],"; border-color: #2e6da4"))),
                         column(1,actionButton("r8",  HTML("<b>2,8</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[4],"; border-color: #2e6da4"))),
                         column(1,actionButton("r7",  HTML("<b>2,1</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[5],"; border-color: #2e6da4"))),
                         column(1,actionButton("r6",  HTML("<b>1,4</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[6],"; border-color: #2e6da4"))),
                         column(1,actionButton("r5",  HTML("<b>1,2</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[7],"; border-color: #2e6da4"))),
                         column(1,actionButton("r4",  HTML("<b>1,1</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[8],"; border-color: #2e6da4"))),
                         column(1,actionButton("r3",  HTML("<b>0,9</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[9],"; border-color: #2e6da4"))),
                         column(1,actionButton("r2",  HTML("<b>0,8</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[10],"; border-color: #2e6da4"))),
                         column(1,actionButton("r1",  HTML("<b>0,6</b>"),
                                               style=paste0("color: #222426; background-color: ",colores[11],"; border-color: #2e6da4")))
                         ),
                         timevisOutput("mytime"),
                         fluidRow(
                           column(12,
                                  em("* El último valor ingresado se prolongará hasta el final de la proyección",
                                     style="float:right; font-size=9px;")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  div(actionButton("clear_timeline", "Reset"),
                                      style="float:right")
                           )
                         )
                       )
                     ),
                    column(5,
                       wellPanel(h5("R0 según intervenciones"),
                           dygraphOutput("rgraph_timeline", 
                               width = "100%", height = "200px"),
                            DTOutput("table")
                       )
                    )
                ),
                # set R con sliders
                conditionalPanel("input.tipo_interv=='R0'",
                  column(12,
                     fluidRow(
                        h4("Ingreso de R0 específicos", style = "margin-left: 15px;")
                     ),
                     fluidRow(
                       br(),
                       column(
                         3,
                         radioGroupButtons(
                           inputId = "periodos",
                           label = "Seleccione nivel de detalle para el ingreso de datos: ",
                           choices = c("Mes", "Quincena", "Semana")
                         ),
                         conditionalPanel(" input.periodos == 'Mes' ",
                            column(12, id = "sliders-mes")
                         ),
                         conditionalPanel(" input.periodos == 'Quincena' ",
                            column(12, id = "sliders-quincena") 
                         ),
                         conditionalPanel(" input.periodos == 'Semana' ",
                            column(12, id = "sliders-semana")
                         ),
                       ),
                       column(
                         5,
                         fluidRow(
                           br(),
                           column(
                             12,
                             h4("Herramientas"),
                             p("Puede configurar el parámetro del número básico de 
                       reproducción mediante estos atajos. Especificando 
                       una estrategia, los niveles esperados de R0, y 
                       el período de aplicación puede generar un patrón
                       de parámetros con pocos clicks."),
                             selectInput("atajo", "Seleccione estrategia:",
                                         c("Constante" = "co",
                                           "Valvular Intermitente" = "vi",
                                           "Reducción gradual de restricciones" = "rg"
                                         ), selected = "vi"),
                             conditionalPanel(" input.atajo == 'co' ",
                                sliderInput("sliderSimpleAtajo", label = "R0", min = 0.5, max = 4, 
                                            value = 1.1)
                              ),
                             conditionalPanel(" input.atajo == 'vi' || input.atajo == 'rg' ",
                                sliderInput("sliderRangoAtajo", label = "R0", min = 0.5, max = 4, 
                                            value = c(1.1, 1.5))
                             ),
                             selectInput("inicioAtajo", "Desde:",
                                         c("Inicio" = "inicio")),
                             selectInput("finAtajo", "hasta:",
                                         c("Fin" = "fin")),
                             actionButton("aplicarAtajo", 
                                          icon = icon("arrow-left"),
                                          "Actualizar parámetros"),
                           )
                         ),
                       ),
                       column(
                         4,
                         wellPanel(
                           "Gráfico de R0",
                           br(),br(),
                           dygraphOutput("rgraph", width = "100%", height = "250px") 
                         ),
                         wellPanel(
                           "Tabla de R0",
                           DTOutput("tablaFechas")  
                         )
                       )
                     )
                 )
              ),
              conditionalPanel("input.tipo_interv=='trigger'",
                      column(12,
                       fluidRow(
                         column(12,
                              h4("Configure un R0 al momento de alcanzar un porcentaje de saturación deseado.",
                                 tags$span(id="info_trigger", icon("info-circle"), 
                                           style=("margin-left:5px;")))
                              )),
                              br(),
                      fluidRow(
                        column(4,
                                 knobInput("trigger_Porc_crit",
                                           label = "Porcentaje de saturación de camas UCI destinadas a COVID que gatilla la intervención:",
                                           value = 70,
                                           min = 0, max = 100,post = "%",
                                           displayPrevious = TRUE, width = "100%", height = "90%",
                                           lineCap = "round",
                                           fgColor = "#d1e1f0",
                                           inputColor = "#85abcc")
                               ),
                        column(4, 
                              
                                uiOutput("trigger_R_base_ui"),
                               
                               sliderInput(
                                  "trigger_R_inter",
                                  "R0 de Intervención:", 
                                  value = 1,
                                  min = .5,
                                  max = 4)
                               ),
                        column(4,
                               numericInputIcon(inputId = "Dias_interv",
                                                label = "Días de intervención", 
                                                value = 30,
                                                icon = icon("calendar-alt","fa-3x"), size = "lg")
                              
                            )
                          )
                        )
                      )   
                  )
              
          
        ),
        tabPanel("Recursos sanitarios",
              h5("Determine los recursos de camas críticas y ventiladores, y el porcentaje destinado a covid-19:"),
              br(),
              fluidRow(
                column(3, align="center",
                  uiOutput("CC_set")
                ),
                column(3, align="center",
                  uiOutput("Vent_set")
                ),
                column(3, align="center",
                       uiOutput("Med_set")
                ),
                column(3, align="center",
                       uiOutput("Enf_set")
                )
                ),
              fluidRow(
                column(2, ""),
                column(3, align="right",
                       uiOutput("Porc_crit")
                ),
                column(2, ""),
                column(3, align="left",
                       uiOutput("Vent_por_CC")
                ),
                column(2, "")
              ),
              h5(em("Fuente: IECS. Datos recolectados y validados a partir de fuentes de cada país/jurisdicción.")),
              br(),
        ),
        tabPanel("Epidemiología",
               h5("Las proyecciones fueron realizadas con los siguientes parámetros.",
                  "Puede editar los mismos para una nueva proyección:"),
               fluidRow(
                 column(10,
                   wellPanel(
                     fluidRow( 
                       style='margin-bottom:5px;border-bottom:1px solid;',
                       column(6,
                              "Parámetro"),
                       column(6,
                              "Fuente")),
                     fluidRow( 
                      # style='margin-bottom:5px;border:1px solid; padding: 2px;color:#7F7F7F;',
                      column(6,
                             uiOutput("Valuepi1")),
                      column(6,
                             br(),br(),
                             tags$a(href="https://www.medrxiv.org/content/10.1101/2020.04.01.20050138v1.full.pdf", 
                                    "Khalili(2020). Ref: 5.84	(4.83-6.85)IC95%",target="_blank"))),
                    fluidRow( 
                      column(6,
                             uiOutput("Valuepi2")),
                      column(6,
                             br(),br(),
                             tags$a(href="https://www.medrxiv.org/content/10.1101/2020.03.05.20031088v2.full.pdf", 
                                    "Peak et al.(2020). Ref: 4.8 (1.12-10.5)IC95%",target="_blank"))),
                    fluidRow( 
                      column(6,
                             uiOutput("Valuepi3")),
                      column(6,
                             br(),br(),
                             "Análisis basado en ",
                             tags$a(href="https://jamanetwork.com/journals/jama/fullarticle/2762130", 
                                    "Wu et al.(2020) Ref: 1.27 (0.89-1.64)IC95%",target="_blank"),
                             " considerando IFR tomado como referencia."),
                              ),
                    fluidRow( 
                      column(6,
                             uiOutput("Valuepi4")),
                      column(6,
                             br(),br(),
                             tags$a(href="https://www.medrxiv.org/content/10.1101/2020.04.23.20076737v1.full.pdf", 
                                    "Auld et al.(2020) Ref: 13 (9-20)IQR ",target="_blank"))),
                    fluidRow( 
                      column(6,
                             uiOutput("Valuepi5")),
                      column(6,
                             br(),br(),
                             tags$a(href="https://www.medrxiv.org/content/10.1101/2020.04.23.20076737v1.full.pdf", 
                                    "Auld et al.(2020) Ref: 8	(4-13)IQR ",target="_blank"))),
                    fluidRow( 
                      column(6,
                             uiOutput("Valuepi6")),
                      column(6,
                             br(),br(),
                             tags$a(href="https://www.medrxiv.org/content/10.1101/2020.05.13.20101253v3",
                                    "Ionnidis et al.(2020) Ref: 0.26% (0.02%-0.86%)Rango",target="_blank"))),
                    fluidRow( 
                      column(6,
                             uiOutput("Valuepi8")),
                      column(6,
                             br(),br(),
                             "No aplica")),
                    fluidRow( 
                      column(6,
                             uiOutput("Valuepi7")),
                      column(6,
                             br(),br(),
                             "No aplica")),
                    ),
                  ),
               ),
        )
    ),
    hr(),
    fluidRow( class = "text-center",
              p(style = "margin-bottom: 2px; font-size: 15px;  color: #67c97c;", 
                HTML(paste0("<i>","Versión ", versionModelo, ". Esta herramienta presenta 
                    resultados basados en modelos 
                    y estimaciones, su utilización para la toma de decisiones
                    queda bajo responsabilidad del usuario.
                    <br>Contacto:</i> <a href=\"mailto:ciips@iecs.org.ar?
                    subject='Modelo COVID-19'\">ciips@iecs.org.ar</a>")))
    )
)

########### server
server <- function(input, output, session) {

# url me dice si quiere ir a subnacional argentino
observe({
  if(str_detect(session$clientData$url_pathname, "argentina")==T){
    updateSelectInput(session, "pais",
                      choices = c("Argentina - Ciudad Autónoma de Buenos Aires" = "ARG_2",
                                  "Argentina - AMBA" = "ARG_3",
                                  "Argentina - Buenos Aires (Partidos del AMBA)" = "ARG_7",
                                  "Argentina - Corrientes" = "ARG_18",
                                  "Argentina - Mendoza" = "ARG_50"),
                      selected = "ARG_2")
  }
})

# siempre inicia predefinido en 1.1
input.escenarioSeleccionado <- "constanteR_cori"

# estrategia acumulada a completar
estrategia_nula  <- data.frame(id = 1, content = " ", start = hoy, end = hoy, className = "Sin")
estrategia_acum <- reactiveValues(data = estrategia_nula)
estrategia_R0 <- reactiveValues(data = 0)

# proyecta políticas?
proy_politicas <- reactiveValues(x = 0)

# cambió epi?
proy_epi <- reactiveValues(x = 0)

# cambió epi?
proy_rrhh <- reactiveValues(x = 0)

# resultados a reportar - posibilidad de acumular escenarios
tabla_resultados_reporte <- reactiveValues(tabla = 0)

# tabla de inputs para reporte
tabla_rrhh_epi <- reactiveValues(x = 0)

# r de proyección para mostrar arriba, ver todo el tiempo que R tiene en los grafs
r_proyeccion_actual <- reactiveValues(Rs = 0)

# varaicion de escenarios
variacion_escenario <- reactiveValues(x = 0.25)

prop_susceptible <- reactiveValues(x = 1)

linkEscenarios <- function(input, output) {
  print("Clink!")
};

# levanta pais proyección actual -------------------------------------------------------------
observeEvent(input$pais,{
  
# browser()
    
  # start progress
    withProgress(message = "Calculando...",{
  
    incProgress(.1)

    # levanta data, según sea corrientes (hay cosas separadas) SAQUE "HND", de paises_distintos
    paises_distintos <<- c("ARG_18","CRI","SLV","JAM","PRY","ARG_50","BHS","BLZ","BRB",
                           "GUY","HTI","NIC","SUR","TTO","VEN")
    # if(input$pais %!in% paises_distintos){
    #   load("DatosIniciales/dataEcdc.RData")
    #   #Info_ecdc <- dataEcdc %>% mutate(fecha=ymd(fecha)) %>% 
    #   #                filter(fecha<hoy, countryterritoryCode==input$pais)
    #   # lag_r <<- 18
    # }
    load(paste0("DatosIniciales/DatosIniciales_",input$pais,".RData"), 
         envir = .GlobalEnv)
    
    pais_actual <<- input$pais
    
    # encode tablainput y reemplaza como global
    tablaInputs$Input = as.character(tablaInputs$Input)
    # Encoding(tablaInputs$Input)<-"UTF-8"
    tablaInputs$Valor = as.numeric(as.character(tablaInputs$Valor))
    # tablaInputs <<- tablaInputs
    
    # if(input$pais %in% paises_distintos){
    #   # Info_ecdc <- dataEcdc %>% mutate(cases=new_cases, deaths = new_deaths) %>% 
    #   #                             filter(fecha<hoy)
    #   lag_r <<- 1
    # }
    load("DatosIniciales/poblacion_data.RData", envir = .GlobalEnv)
    # datos ecdc para mostrar al inicio
    
    incProgress(.3)
    
    # avg
    
    
    if(input$pais %in% c("ARG_18", "ARG_50", "ARG_2", "ARG_3", "ARG_7")){
      owd_pais_avg <- owd_data %>% 
                          filter(iso_code==input$pais, as.Date(date) %in% seq(hoy-7,hoy-1,by="day")) %>% 
                          dplyr::summarise(new_cases = mean(new_cases),
                                           new_deaths = mean(new_deaths))
      output$inf_ayer <-renderText({ fnum(ifelse(owd_pais_avg$new_cases==0,"S/D", owd_pais_avg$new_cases)) })
      output$muertes_ayer <- renderText({ fnum(ifelse(owd_pais_avg$new_deaths==0,"S/D", owd_pais_avg$new_deaths)) })
    } else{
      oms_pais_avg <- oms_data %>% 
        filter(iso_code==input$pais, as.Date(date) %in% seq(hoy-7,hoy-1,by="day")) %>% 
        dplyr::summarise(new_cases = mean(new_cases),
                         new_deaths = mean(new_deaths))
      output$inf_ayer <-renderText({ fnum(ifelse(oms_pais_avg$new_cases==0,"S/D", oms_pais_avg$new_cases)) })
      output$muertes_ayer <- renderText({ fnum(ifelse(oms_pais_avg$new_deaths==0,"S/D", oms_pais_avg$new_deaths)) })
    }
    
    # last
    oms_pais <- oms_data %>% filter(iso_code==input$pais, as.Date(date) == (hoy-1))
    owd_pais <- owd_data %>% filter(iso_code==input$pais, as.Date(date) == (hoy-1))
    output$inf_acum <- renderText({ fnum(ifelse(owd_pais$total_cases==0, "S/D", owd_pais$total_cases)) })
    output$muertes_acum <- renderText({ fnum(ifelse(owd_pais$total_deaths==0,"S/D", owd_pais$total_deaths)) })
    output$e0 <- renderText(paste({ fnum(owd_pais$life_expectancy) }, " años"))
    output$test_acum <- renderText({ fnum(ifelse(is.na(owd_pais$total_tests_per_thousand),
                                                 "S/D", owd_pais$total_tests_per_thousand*1000),0) })
    output$new_tests <- renderText({ fnum(ifelse(is.na(owd_pais$new_tests),
                                                 "S/D", owd_pais$new_tests),0) })
    
    addPopover(session, 
               "warning_data", "Nota sobre los datos", 
               content = "En esta aplicación se releva periódicamente información de distintas fuentes, 
               cuya naturaleza puede diverger entre los países o jurisdicciones (y sus respectivas metodologías de registro) 
               en cuestiones como el suministro de la información diaria de infectados y defunciones según sea por 
               fecha de reporte, registro u ocurrencia. Para contar con insumos lo más independiente posible de 
               tal heterogeneidad se suaviza la información base a través de promedio móviles o regresiones para captar 
               las tendencias implícitas como insumo para la proyección.")
    addPopover(session, 
               "info_Inf_diarios", "Nuevos infectados", 
               content = paste0("Se muestra el promedio de nuevos infectados de los últimos 7 días."))
    addPopover(session, 
               "info_Mue_diarios", "Nuevas defunciones", 
               content = paste0("Se muestra el promedio de nuevas defunciones de los últimos 7 días."))
    incProgress(.4)
    
    # presenta país
    output$pais <- renderText({ paste0("Proyecciones de ",
                                       poblacion_data$label[poblacion_data$pais==input$pais][1]) })
    
    #PAÍS ATUAL SELECIONADO
    ##poblacion_data$label[poblacion_data$pais==input$pais][1]
    
    pais_actual_label <<- as.character(poblacion_data$label[poblacion_data$pais==input$pais][1])
    output$pais2 <-  renderText({ pais_actual_label })
    
    # browser()
    # con qué R arranca el país (nuestra mejor estimación)
    #r_cori <<- trunc(get_R(modeloSimulado, window = 14)$cori[1]*10)/10 # truncado a 1 decimal
    dias_dupl <- get_dias_dupl(owd_data %>% filter(iso_code==input$pais), hoy, 7)[1]
    output$dias_dupl <- renderText({ fnum(dias_dupl, 2) })
    
    sus_prop_hoy <- modeloSimulado$Sprop[modeloSimulado$fecha==hoy]
    rt_cori <<- round(r_cori*sus_prop_hoy,2)
    output$R_hoy <- renderText({ paste0("Rt = ", fnum(rt_cori, 3)) })
    output$R0_hoy <- renderText({ paste0("R0 = ", 
                     fnum(r_cori, 3)) })
    output$R_hoy_predef <- renderText({
      paste0(
        "Este escenario se basa en la estimación vigente Rt=",  
        fnum(rt_cori, 3),
        ", siendo equivalente a un R0=",
        fnum(r_cori, 3),
        " para toda el período de proyección.")
    })
    
    # textos para ui -----------------------------------------------------------
    
    
    #ALTERACAO DIRETA CEARA
    # camasGenerales <- 11139
    # camasCriticas <- 1145
    # ventiladores <- 1950
    # enfermerasCamasGenerales <- 10466
    # enfermerasCamasUCI <- 209
    # medicosCamasGenerales <- 11.498
    # medicosCamasUCI <- 103
    # porcentajeDisponibilidadCamasCOVID <- 0.7
    
    
    output$titulo_tabla_resultados <-renderText({  paste0("Tabla de resultados (", poblacion_data$label[poblacion_data$pais==input$pais][1],")") })
    output$poblacion <- renderText({ fnum(poblacion_data$value[poblacion_data$pais==input$pais][1]) })
    output$poblacion65mas <- renderText({ paste0(fnum(poblacion_data$value[poblacion_data$pais==input$pais][2]),"%") })
    output$camasCriticas <- renderText({ as.character(camasCriticas) })
    output$ventiladores <- renderText({ as.character(ventiladores) })
    
    # acomodo df ppal
    modeloSimulado <<- modeloSimulado %>% 
                        filter(!is.na(fecha), fecha < as.Date("2021-07-01")) %>%
                        mutate(RtEstimado = round(RtEstimado,2),
                               fecha = as.Date(as.character(fecha)))
    modeloSimulado_hi <<- modeloSimulado_hi %>% 
                        filter(!is.na(fecha), fecha < as.Date("2021-07-01")) %>%
                        mutate(RtEstimado = round(RtEstimado,2),
                               fecha = as.Date(as.character(fecha)))
    modeloSimulado_low <<- modeloSimulado_low %>% 
                        filter(!is.na(fecha), fecha < as.Date("2021-07-01")) %>%
                        mutate(RtEstimado = round(RtEstimado,2),
                               fecha = as.Date(as.character(fecha)))
  
    # grafica
    # browser()
    callModule(graficando, "graficos")
  
    incProgress(.3)
    
    # zoom new country in map
    leafletProxy("mymap") %>% 
      setView(lng = coords[coords$pais==input$pais,"lng"], 
              lat = coords[coords$pais==input$pais,"lat"], 
              zoom = 12
              )
    
    # Estrategias Base, reinicia con cada país
    estrategia_inicial <<- data.frame(
                            id      = 1,
                            content = round(r_cori,2),
                            start   = hoy,
                            end     = hoy, className = "Sin") %>% 
                          mutate(start = as.Date(start), end = as.Date(end)) %>% as.data.frame()
    estrategia_acum$data <- estrategia_inicial
    estrategia_R0$data <- 0
    
    incProgress(.1)
  
    # guardo resultados
    tabla_resultados_reporte$tabla =  rbind(
                    t(c(resumenResultados[c(2,1,3),3],"No Aplica","No Aplica")),
                    t(c(resumenResultados[c(5,4,6),3],"No Aplica","No Aplica")),
                    t(c(resumenResultados[c(10,7,8,9,11),3])),
                    t(c(resumenResultados[c(15,13,16,14,17),3]))) %>% as.data.frame() %>%  
                    mutate(Indicador = c("Nuevas Infecciones","Nuevas Defunciones","Camas","Ventiladores")) %>%
                    setNames(c("Fecha pico", "Cantidad pico","Días hasta el pico","% saturación","Primer día saturación","Indicador")) %>% 
                    dplyr::select(6,1:5)
    
    # tabla Resultados. Solo muestra
    output$tableResults <- renderDT({
      tResult <- tabla_resultados_reporte$tabla
        datatable(tResult %>% as.data.frame(), 
                  rownames = F,
                  options = list(lengthChange = FALSE,
                                 searching = F, paging = F,
                                 ordering = F, info = F,
                                 autoWidth = T,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:5))),
                  style = 'bootstrap', class = 'table-bordered')
    })
    
    
    
    incProgress(.1)
    
    # controles epi
    output$Valuepi1 <- renderUI({
      numericInput(inputId = "PerPreinfProm",label = "Período preinfeccioso promedio (días)",
                       value = tablaInputs$Valor[tablaInputs$Input=="Periodo preinfeccioso promedio (dias)"], 
                       width = "90%",min = 4.83,max = 6.85,step = .01)
    })
    output$Valuepi2 <- renderUI({
      numericInput(inputId = "DurMediaInf",label = "Duración media de la infecciosidad (días)",
                       value = tablaInputs$Valor[tablaInputs$Input=="Duracion media de la infecciosidad (dias)"], 
                       width = "90%",min = 1.12,max = 10.5,step = .01)
    })
    output$Valuepi3 <- renderUI({
      numericInput(inputId = "PorcCasosCrit",label = "Porcentaje de casos críticos",
                       value = tablaInputs$Valor[tablaInputs$Input=="Porcentaje de casos criticos"], 
                       width = "90%",min = 0.89,max = 1.64,step = .01)
    })
    output$Valuepi4 <- renderUI({
      numericInput(inputId = "DíasHospCrit",label = "Días de hospitalización para casos críticos",
                       value = tablaInputs$Valor[tablaInputs$Input=="Dias de hospitalizacion para casos criticos"], 
                       width = "90%",min = 9,max = 20,step = 1)
    })
    output$Valuepi5 <- renderUI({
      numericInput(inputId = "DíasUCICrit",label = "Días UCI para casos críticos",
                       value = tablaInputs$Valor[tablaInputs$Input=="Dias de la UCI para casos criticos"], 
                       width = "90%",min = 4,max = 135,step = 1)
    })
    output$Valuepi6 <- renderUI({
      numericInput(inputId = "IFR",label = "Tasa de letalidad (IFR). En porcentaje (%)",
                       value = as.numeric(
                                  as.character(
                                    tablaInputs$Valor[tablaInputs$Input=="Tasa de letalidad (IFR)"]))*100, 
                       width = "90%",min = 0.05,max = 0.78,step = .001)
    })
    output$Valuepi7 <- renderUI({
      numericInput(inputId = "variacion_escenario",label = "Sensibilidad de escenarios",
                       value = .25, 
                       width = "90%",min = .05,max = .5,step = .05)
    })
    output$Valuepi8 <- renderUI({
      numericInput(inputId = "prop_susceptible",label = "Población susceptible al inicio. En porcentaje (%)",
                   value = prop_susceptible$x * 100, 
                   width = "90%",min = 50,max = 100,step = 10)
    })
    
    # controles de rrhh
    output$CC_set <- renderUI({
      numericInputIcon(inputId = "CC_set",
                       label = "Cantidad de camas críticas", 
                       value = camasCriticas,
                       icon = icon("procedures","fa-5x"), width = "90%")
    })
    output$Vent_set <- renderUI({
      numericInputIcon(inputId = "Vent_set",label = "Cantidad de ventiladores", 
                       value = ventiladores,
                       icon = icon("star-of-life","fa-5x"), width = "90%")
    })
    output$Med_set <- renderUI({
      numericInputIcon(inputId = "Med_set",label = "Médicos/as camas UCI", 
                       value = medicosCamasUCI,
                       icon = icon("user-md","fa-5x"), width = "90%")
    })
    output$Enf_set <- renderUI({
      numericInputIcon(inputId = "Enf_set",label = "Enfermeras/os camas UCI", 
                       value = enfermerasCamasUCI,
                       icon = icon("plus-square","fa-5x"), width = "90%")
    })
    output$Porc_crit <- renderUI({
      knobInput(inputId = "Porc_crit",
                label = HTML("Porcentaje de camas críticas <br/> destinadas a COVID:"),
                value = round(porcentajeDisponibilidadCamasCOVID * 100,0),
                min = 0, max = 100,post = "%",
                displayPrevious = TRUE, width = "80%", height = "80%",
                lineCap = "round",
                fgColor = "#d1e1f0",
                inputColor = "#85abcc")
    })
    output$Vent_por_CC <- renderUI({
      knobInput(inputId = "Vent_por_CC",
                label = HTML("Tasa de días de uso de ventiladores <br/> por días de uso de cama crítica:"),
                value = round(ventiladoresCamaCritica * 100,0),
                min = 0, max = 100,post = "%",
                displayPrevious = TRUE, width = "80%", height = "80%",
                lineCap = "round",
                fgColor = "#c0edd2",
                inputColor = "#85abcc")
    })
    
    # tabla para reporte
    tabla_rrhh_epi$x = tablaInputs %>% slice(1,2,4,7,8,9,22,23,28,17)
    
    # guarda R actual y lo grafica al inicio
    output$g_R_actual <- renderDygraph({
      r_proyeccion_actual$Rs <- modeloSimulado %>% 
        dplyr::select(fecha, R0Usuario) %>% 
        filter(R0Usuario!=0, !is.na(fecha)) %>% as.data.frame()
      dg <- xts(r_proyeccion_actual$Rs[,2], order.by = as.Date(r_proyeccion_actual$Rs[,1]))
      dy <- dygraph(dg) %>% 
        dySeries("V1", label = "R") %>% 
        dyAxis("y", valueRange = c(.8, 3)) %>% 
        dyOptions(drawGrid = F, stepPlot = T)  
      if(trigger_on_app==1){
        dy <- dy %>% dyEvent(x = fechaIntervencionesTrigger, 
                             label = rep("Intervención",length(fechaIntervencionesTrigger)), labelLoc = "bottom", 
                             color = "grey", strokePattern = "dotted")
      }
      dy %>% dyEvent(x = hoy, label = "Hoy", labelLoc = "top", 
                     color = "grey", strokePattern = "dashed")
    })
    
    # predefinido por defecto
    disable("EscenarioActual")
    updateSelectInput(session, "escenarioPredefinido",selected = "constanteR_cori")
    
    # resetea cambios
    proy_politicas$x=0
    proy_rrhh$x=0
    proy_epi$x=0
    
  }) # progress
}) # ui

  output$default_o_no <- renderText({paste0("El escenario proyectado por default 
                                      corresponde a un R0 actual constante de ",
                                      r_cori)})
  
  # Carrito!
  observe(
  output$carrito <- renderText({
    if(sum(proy_politicas$x, proy_rrhh$x, proy_epi$x)>0){
      HTML(paste0("Se han modificado parámetros de:",
                  ifelse(proy_politicas$x==1,"<br/>- Políticas de intervención",""),
                  ifelse(proy_epi$x==1,"<br/>- Epidemiología",""),
                  ifelse(proy_rrhh$x==1,"<br/>- Recursos Sanitarios",""))
      )
    }
    
  })
  )
  
# timeline ----------------------------------------------------------------

  addPopover(session, 
           "info_politicas", "Estimación de impacto de políticas de intervención", 
           content = HTML(paste0("Se presenta un menú de opciones simplificadas que 
                            permiten tipificar las políticas y su nivel de 
                            restricción (que depende tanto de la política 
                            implementada como de su nivel de implementación). 
                            Este grupo de medidas y sus R(t) resultantes se 
                            obtuvieron utilizando la calculadora de mitigación 
                            del proyecto Epidemic Forecasting de la Universidad 
                            de Oxford del Reino Unido,  disponible ",
                            tags$a(href="http://epidemicforecasting.org", 
                                   "aquí.",target="_blank"))),  
           trigger = 'click')
  
  # agrega estrategia - semáforo
  observeEvent(input$r11, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "3.70",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r11"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r10, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "3.3",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r10"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r9, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "3.0",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r9"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r8, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "2.8",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r8"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r7, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "2.1",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r7"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r6, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "1.4",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r6"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r5, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "1.2",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r5"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r4, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "1.1",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r4"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r3, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "0.9",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r3"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r2, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "0.8",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r2"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  observeEvent(input$r1, {
    proy_politicas$x = 1
    estrategia_acum$data = rbind(estrategia_acum$data,
                                 data.frame(id = max(estrategia_acum$data[,1])+1,
                                            content = "0.6",
                                            start = max(estrategia_acum$data[,4])+1,
                                            end = max(estrategia_acum$data[,4])+31,
                                            className = "r1"))
    addItem("mytime", list(estrategia_acum$data[nrow(estrategia_acum$data),]))
  })
  
  
  # visualiza timeline
  output$mytime <- renderTimevis(timevis(data = estrategia_acum$data,
                                         showZoom = F,
                                         options = list(editable = 
                                                        list(remove=TRUE, updateTime= TRUE),  
                                                        align = "center",
                                                        end = "2021-04-01",
                                                        min = "2020-01-01",
                                                        max = "2021-06-01",
                                                        showCurrentTime=F)))
  
  # actualiza tabla si cambia a mano
  observeEvent(input$mytime_data,{
    t = input$mytime_data
    t$id = as.integer(t$id)
    t$start = as.Date(substr(t$start,1,10))
    t$end = as.Date(substr(t$end,1,10))
    t2 = estrategia_acum$data
    if(any(t$end!=t2$end) | any(t$start!=t2$start)){
      estrategia_acum$data = t  
    }
  })
  
  # visualiza tabla
  output$table <- renderDT({
    datatable(estrategia_acum$data[,2:4] %>% 
              setNames(c("R","Comienzo", "Final")), 
              rownames = F,
              editable = list(target = 'cell'),
              options = list(lengthChange = FALSE,
                             searching = F, paging = F,
                             ordering = F, info = F))
  })
  
  # grafica R de timeline
  output$rgraph_timeline <- renderDygraph({
    datam = estrategia_acum$data[,2:4] %>% 
            setNames(c("R","Comienzo", "Final")) %>% 
            mutate(
                  # Comienzo = format(as.Date(as.character(Comienzo), "%m/%d/%y"), "20%y/%m/%d"),
                  R = as.numeric(as.character(R))) %>% 
            filter(!is.na(Comienzo)) %>% as.data.frame()
    dg = xts(datam[,1],order.by = as.Date(datam[,2]))
    dygraph(dg) %>% 
      dySeries("V1", label = "R") %>% 
      dyOptions(drawGrid = F, stepPlot = T)
  })
  
  observeEvent(input$clear_timeline,{
    estrategia_acum$data = estrategia_inicial
    proy_politicas$x = 0
  })


  
# sliders R ---------------------------------------------------------------

  allMesSliderIds <- list()
  allQuincenaSliderIds <- list()  
  allSemanaSliderIds <- list()
  
  for (i in 1:cantidadMeses) {
    sliderId <- paste0("slider-mes",i)
    sliderName <- paste(format(listaFechas[i], "%B-%Y"))
    allMesSliderIds[[sliderName]] <- sliderId
    insertUI(selector = "#sliders-mes", where = "beforeEnd",
             ui = sliderInput(sliderId, sliderName,
                              min = 0.5, max = 4, 
                              value = 1)) # modeloSimulado$RtEstimado[modeloSimulado$fecha==hoy][1]))
  }
  for (i in 1:cantidadMeses) {
    for (quincena in 1:2) {
      sliderId <- paste0("slider-quincena",i,quincena)
      sliderName <- paste(format(listaFechas[i], "%B-%Y"), ", Quincena ", quincena)
      allQuincenaSliderIds[[sliderName]] <- sliderId
      insertUI(selector = "#sliders-quincena", where = "beforeEnd",
               ui = sliderInput(sliderId, sliderName,
                                min = 0.5, max = 4, 
                                value = 1)) # modeloSimulado$RtEstimado[modeloSimulado$fecha==hoy][1]))
    }
  }
  for (i in 1:cantidadMeses) {
    for (semana in 1:4) {
      sliderId <- paste0("slider-semana",i,semana)
      sliderName <- paste(format(listaFechas[i], "%B-%Y"), ", Semana ", semana)
      allSemanaSliderIds[[sliderName]] <- sliderId
      insertUI(selector = "#sliders-semana", where = "beforeEnd",
               ui = sliderInput(sliderId, sliderName,
                                min = 0.5, max = 4, 
                                value = 1)) # modeloSimulado$RtEstimado[modeloSimulado$fecha==hoy][1]))
    }
  }
  
  # Atajos para setear sliders -------------------------------------------------------------------------------------
  observeEvent(input$aplicarAtajo, ignoreInit = T,{
    
    # proy_politicas$x = 1
    
    if (input$atajo == "co") {
      if (input$periodos == "Mes") {
        desdeIndex = match( c(input$inicioAtajo), unlist(allMesSliderIds) )
        hastaIndex = match( c(input$finAtajo), unlist(allMesSliderIds) )
        for (i in 1:cantidadMeses) {
          iname <- paste0("slider-mes",i)
          if (i >= desdeIndex && i <= hastaIndex) {
            updateSliderInput(session, iname, value = input$sliderSimpleAtajo)
          }
        }
      } else if (input$periodos == "Quincena") {
        desdeIndex = match( c(input$inicioAtajo), unlist(allQuincenaSliderIds) )
        hastaIndex = match( c(input$finAtajo), unlist(allQuincenaSliderIds) )
        count <- 1
        for (i in 1:cantidadMeses) {
          for (quincena in 1:2) {
            iname <- paste0("slider-quincena",i,quincena)
            if (count >= desdeIndex && count <= hastaIndex) {
              updateSliderInput(session, iname, value = input$sliderSimpleAtajo)
            }
            count <- count + 1
          }
        }
      } else if (input$periodos == "Semana") {
        desdeIndex = match( c(input$inicioAtajo), unlist(allSemanaSliderIds) )
        hastaIndex = match( c(input$finAtajo), unlist(allSemanaSliderIds) )
        count <- 1
        for (i in 1:cantidadMeses) {
          for (semana in 1:4) {
            iname <- paste0("slider-semana",i,semana)
            if (count >= desdeIndex && count <= hastaIndex) {
              updateSliderInput(session, iname, value = input$sliderSimpleAtajo)
            }
            count <- count + 1
          }
        }
      }
    } else if (input$atajo == "vi") {
      if (input$periodos == "Mes") {
        desdeIndex = match( c(input$inicioAtajo), unlist(allMesSliderIds) )
        hastaIndex = match( c(input$finAtajo), unlist(allMesSliderIds) )
        for (i in 1:cantidadMeses) {
          iname <- paste0("slider-mes",i)
          if (i >= desdeIndex && i <= hastaIndex) {
            if (i %% 2 == 0) {
              updateSliderInput(session, iname, value = input$sliderRangoAtajo[1])
            } else {
              updateSliderInput(session, iname, value = input$sliderRangoAtajo[2])
            }
          }
        }
      } else if (input$periodos == "Quincena") {
        desdeIndex = match( c(input$inicioAtajo), unlist(allQuincenaSliderIds) )
        hastaIndex = match( c(input$finAtajo), unlist(allQuincenaSliderIds) )
        count <- 1
        for (i in 1:cantidadMeses) {
          for (quincena in 1:2) {
            iname <- paste0("slider-quincena",i,quincena)
            if (count >= desdeIndex && count <= hastaIndex) {
              if (quincena == 1) {
                updateSliderInput(session, iname, value = input$sliderRangoAtajo[1])
              } else {
                updateSliderInput(session, iname, value = input$sliderRangoAtajo[2])
              }
            }
            count <- count + 1
          }
        }
      } else if (input$periodos == "Semana") {
        desdeIndex = match( c(input$inicioAtajo), unlist(allSemanaSliderIds) )
        hastaIndex = match( c(input$finAtajo), unlist(allSemanaSliderIds) )
        count <- 1
        for (i in 1:cantidadMeses) {
          for (semana in 1:4) {
            iname <- paste0("slider-semana",i,semana)
            if (count >= desdeIndex && count <= hastaIndex) {
              if (semana == 1 || semana == 3) {
                updateSliderInput(session, iname, value = input$sliderRangoAtajo[1])
              } else {
                updateSliderInput(session, iname, value = input$sliderRangoAtajo[2])
              }    
            }
            count <- count + 1
          }
        }
      }
    } else if (input$atajo == "rg") {
      if (input$periodos == "Mes") {
        desdeIndex = match( c(input$inicioAtajo), unlist(allMesSliderIds) )
        hastaIndex = match( c(input$finAtajo), unlist(allMesSliderIds) )
        periodo = hastaIndex - desdeIndex
        incremento <- (input$sliderRangoAtajo[2] - input$sliderRangoAtajo[1]) / periodo
        acumulado <- input$sliderRangoAtajo[1]
        for (i in 1:cantidadMeses) {
          iname <- paste0("slider-mes",i)
          if (i >= desdeIndex && i <= hastaIndex) {
            updateSliderInput(session, iname, value = acumulado)
            acumulado <- acumulado + incremento
          }
        }
      } else if (input$periodos == "Quincena") {
        desdeIndex = match( c(input$inicioAtajo), unlist(allQuincenaSliderIds) )
        hastaIndex = match( c(input$finAtajo), unlist(allQuincenaSliderIds) )
        periodo = hastaIndex - desdeIndex
        incremento <- (input$sliderRangoAtajo[2] - input$sliderRangoAtajo[1]) / periodo
        acumulado <- input$sliderRangoAtajo[1]
        count <- 1
        for (i in 1:cantidadMeses) {
          for (quincena in 1:2) {
            iname <- paste0("slider-quincena",i,quincena)
            if (count >= desdeIndex && count <= hastaIndex) {
              updateSliderInput(session, iname, value = acumulado)
              acumulado <- acumulado + incremento
            }
            count <- count + 1
          }
        }
      } else if (input$periodos == "Semana") {
        desdeIndex = match( c(input$inicioAtajo), unlist(allSemanaSliderIds) )
        hastaIndex = match( c(input$finAtajo), unlist(allSemanaSliderIds) )
        periodo = hastaIndex - desdeIndex
        incremento <- (input$sliderRangoAtajo[2] - input$sliderRangoAtajo[1]) / periodo
        acumulado <- input$sliderRangoAtajo[1]
        count <- 1
        for (i in 1:cantidadMeses) {
          for (semana in 1:4) {
            iname <- paste0("slider-semana",i,semana)
            if (count >= desdeIndex && count <= hastaIndex) {
              updateSliderInput(session, iname, value = acumulado)
              acumulado <- acumulado + incremento
            }
            count <- count + 1
          }
        }
      }
    }
    readSliders(input, output)
  })
  
  # Observa sliders 
  
  observeEvent(
    input$periodos,
    {
      if (input$periodos == "Mes") {
        updateSelectInput(session,"inicioAtajo", choices = allMesSliderIds)
        updateSelectInput(session,"finAtajo", choices = allMesSliderIds,
                          selected =  sapply(allMesSliderIds, tail, 1))
      } else if (input$periodos == "Quincena") {
        updateSelectInput(session,"inicioAtajo", choices = allQuincenaSliderIds)
        updateSelectInput(session,"finAtajo", choices = allQuincenaSliderIds,
                          selected =  sapply(allQuincenaSliderIds, tail, 1))
      } else if (input$periodos == "Semana") {
        updateSelectInput(session,"inicioAtajo", choices = allSemanaSliderIds)
        updateSelectInput(session,"finAtajo", choices = allSemanaSliderIds,
                          selected =  sapply(allSemanaSliderIds, tail, 1))
      }
      delay (50, readSliders(input, output))
    }
  )
  
  lapply(
    X = 1:cantidadMeses,
    FUN = function(mes){
      observeEvent(input[[paste0("slider-mes", mes)]], {
        readSliders(input, output)
        
      })
      lapply(
        X = 1:2,
        FUN = function(quincena){
          observeEvent(input[[paste0("slider-quincena",mes,quincena)]], {
            readSliders(input, output)
          })
        }
      )
      lapply(
        X = 1:4,
        FUN = function(semana){
          observeEvent(input[[paste0("slider-semana",mes,semana)]], {
            readSliders(input, output)
          })
        }
      )
    }
      )

  readSliders <- function(input, output) {
    # habilita proy si paso mas de n segundos, salteando las llamadas iniciales del readSliders
    # al setear valores de inicio
    elapsed <- difftime(Sys.time(),tiempoInicio,units="secs")
    # print(elapsed);
    if (elapsed > 90) {
      proy_politicas$x = 1
    }

    datosPoliticas <- c()
    if (input$periodos == "Mes") {
      for (i in 1:cantidadMeses) {
        iname <- paste0("slider-mes",i)
        fechaFin <- listaFechas[i] %m+% months(1) - 1
        datosPoliticas <- c(
          datosPoliticas,c(
            i,
            format(listaFechas[i], "%D"),
            format(fechaFin, "%D"),
            input[[iname]] 
          )
        )
      }
      datam = matrix(datosPoliticas, ncol = 4, byrow=TRUE)
      colnames(datam) <- c("Mes","Fecha desde", "Fecha hasta","R0")
      estrategia_R0$data <- datam
      output$tablaFechas <- renderDT({
        datatable(datam,rownames = F,
                  editable = list(target = 'cell'),
                  options = list(lengthChange = FALSE,
                                 searching = F, paging = F,
                                 ordering = F, info = F))
      })
    } else if (input$periodos == "Quincena") {
      for (i in 1:cantidadMeses) {
        for (quincena in 1:2) {
          iname <- paste0("slider-quincena",i,quincena)
          if (quincena == 1) {
            fechaInicio <- listaFechas[i]
            fechaFin <- listaFechas[i] + 14
          } else {
            fechaInicio <- fechaFin + 1
            fechaFin <- listaFechas[i] %m+% months(1) - 1
          }
          datosPoliticas <- c(
            datosPoliticas,c(
              i,
              format(fechaInicio, "%D"),
              format(fechaFin, "%D"),
              input[[iname]] 
            )
          )
        }
      }
      datam = matrix(datosPoliticas, ncol = 4, byrow=TRUE)
      colnames(datam) <- c("Mes","Fecha desde", "Fecha hasta","R0")
      estrategia_R0$data <- datam
      output$tablaFechas <- renderDT({
        datatable(datam,rownames = F,
                  editable = list(target = 'cell'),
                  options = list(lengthChange = FALSE,
                                 searching = F, paging = F,
                                 ordering = F, info = F))
      })
    } else if (input$periodos == "Semana") {
      for (i in 1:cantidadMeses) {
        for (semana in 1:4) {
          iname <- paste0("slider-semana",i,semana)
          if (semana == 1) {
            fechaInicio <- listaFechas[i]
            fechaFin <- listaFechas[i] + 6
          } else if (semana > 1 && semana < 4) {
            fechaInicio <- fechaFin + 1
            fechaFin <- fechaInicio + 6
          } else if (semana == 4) {
            fechaInicio <- fechaFin + 1
            fechaFin <- listaFechas[i] %m+% months(1) - 1
          }
          datosPoliticas <- c(
            datosPoliticas,c(
              i,
              format(fechaInicio, "%D"),
              format(fechaFin, "%D"),
              input[[iname]] 
              )
            )
          }
        }
    }
    datam = matrix(datosPoliticas, ncol = 4, byrow=TRUE)
    colnames(datam) <- c("Mes","Fecha desde", "Fecha hasta","R0")
    estrategia_R0$data <- datam
    output$tablaFechas <- renderDT({
      datatable(datam,rownames = F,
                editable = list(target = 'cell'),
                options = list(lengthChange = FALSE,
                               searching = F, paging = F,
                               ordering = F, info = F))
    })
    
    output$rgraph <- renderDygraph({
  
      datam = matrix(datosPoliticas, ncol = 4, byrow=TRUE)
      colnames(datam) <- c("Mes","Desde", "Hasta","R0")
      datam = as.data.frame(datam) %>% dplyr::select(2,4) %>% 
        mutate(Desde = format(as.Date(as.character(Desde), "%m/%d/%y"), "20%y/%m/%d"),
               R0 = as.numeric(as.character(R0)))
      dg = xts(datam[,-1],order.by = as.Date(datam[,1]))
      dygraph(dg) %>% 
        dySeries("V1", label = "R") %>% 
        dyOptions(drawGrid = F, stepPlot = T)
    })
  }
  
# observa trigger ---------------------------------------------------------

  # slider R base que depende del de hoy
  
  output$trigger_R_base_ui <- renderUI({
    sliderInput(
      "trigger_R_base",
      "R0 de base de Proyección:", 
      value =  r_cori,
      min = .5,
      max = 4)
  })
  
  observeEvent(input$trigger_Porc_crit | input$trigger_R_inter | 
                 input$Dias_interv | input$trigger_R_base_ui | input$tipo_interv=='trigger', 
               ignoreInit = T,{
                 # browser()
        proy_politicas$x = 1
  })
  
  addPopover(session, 
             "info_trigger", "Estrategia de trigger", 
             content = paste0("Esta estrategia define como gatillo un porcentaje de ocupación de camas UCI 
                              destinadas a COVID (el sistema ya estima la cantidad de camas destinadas 
                              a COVID y no a otras patologías mediante el porcentaje asignado en la
                              solapa de parámetros de recursos sanitarios, habitualmente un 70%).
                              El escenario se proyecta con un R0 base, y cuando se alcanza la 
                              ocupación de camas de COVID gatillo se inicia una intervención 
                              con la intensidad y duración especificadas. Una vez finalizado ese 
                              lapso de tiempo y que se haya reducido el nivel de ocupación por 
                              debajo del nivel gatillo, se retoma el R0 base hasta que se alcance
                              nuevamente el nivelde ocupación gatillo."),  
             trigger = 'click')
  
# map ---------------------------------------------------------------------

  #FLAVIO COMENTOU
  output$mymap <- renderLeaflet({
    # mytext <- paste(
    #   round(map_data@data$cum_deaths_millon,1),
    #   " c/Mill.Hab.",
    #   sep="") %>%
    #   lapply(htmltools::HTML)
    # pal <- colorBin("YlOrRd", map_data@data$cum_deaths_millon)
    leaflet(map_data,
            options = leafletOptions(attributionControl=FALSE,
                                     zoomControl = FALSE,
                                     zoomControl = FALSE,
                                     minZoom = 3, maxZoom = 6)) %>%
      addProviderTiles(providers$CartoDB.Positron) 
    # %>%
    #   addPolygons(stroke = F, fillOpacity = .5, smoothFactor = .5,
    #               color = ~pal(cum_deaths_millon),
    #               label = mytext) 
    # %>%
    #   leaflet::addLegend("bottomright", pal = pal, values = ~cum_deaths_millon, opacity = .6,
    #                      title = "Muertes Acum. </br>
    #                      c/Mill. hab.")
  })

  
  # mira si proyecta rrhh en widgets
  observeEvent(input$CC_set | input$Vent_set | input$Med_set | input$Enf_set | input$Porc_crit, ignoreInit = T, {
    # browser()
    xtablaInputs <- tablaInputs %>% mutate(Valor = as.numeric(as.character(Valor)))
    if(input$CC_set != tablaInputs$Valor[tablaInputs$Input=="Cama criticas"] |
       input$Vent_set != tablaInputs$Valor[tablaInputs$Input=="Ventiladores"] |
       input$Porc_crit != tablaInputs$Valor[tablaInputs$Input=="Porcentaje de disponibilidad de camas para COVID-19"] |
       input$Med_set != tablaInputs$Valor[tablaInputs$Input=="Medicos por camas UCI"] |
       input$Enf_set != tablaInputs$Valor[tablaInputs$Input=="Enfermeras por camas UCI"]
       ){
      proy_rrhh$x=1
    }
  })
  
  # si toca input de variacion de escnarios
  observeEvent(input$variacion_escenario |
               input$prop_susceptible |
               input$PerPreinfProm |
               input$DurMediaInf |
               input$PorcCasosCrit |
               input$DíasHospCrit |
               input$DíasUCICrit |
               input$IFR, ignoreInit = T, {
    proy_epi$x=1
    })
  
# New Projection ----------------------------------------------------------
observeEvent(input$updateResults, ignoreInit = T,{
  
  # browser()
  
  if(sum(proy_politicas$x, proy_rrhh$x, proy_epi$x)>0){
    
      # browser()
    
      # proyecta
      withProgress(message = "Proyectando...",{

      # proy politicas o R0 cambia Rusuario
        if(proy_politicas$x==1 & !is.null(input$tipo_interv)){
          trigger_on_app <<- 0
          if(input$tipo_interv == "politicas"){
            Rusuario <- estrategia_acum$data[,2:4] %>% 
              mutate(R.modificado = as.numeric(as.character(content)),
                     Comienzo = as.Date(as.character(start)),
                     Final =    as.Date(as.character(end))) %>% 
              dplyr::select(Comienzo = 2, Final=3, R.modificado)
            }
          else if(input$tipo_interv == "R0"){
            Rusuario <- estrategia_R0$data %>% 
              as.data.frame() %>% 
              dplyr::rename(Comienzo=2, Final=3) %>% 
              mutate(R.modificado = as.numeric(as.character(R0)),
                     Comienzo = mdy(as.character(Comienzo)),
                     Final =    mdy(as.character(Final))) %>% 
              dplyr::select(Comienzo, Final, R.modificado)
            }
          else{
            trigger_on_app <<- 1
            Rusuario <- data.frame(
                   R.modificado = as.numeric(as.character(input$trigger_R_base)),
                   Comienzo = hoy,
                   Final =    max(as.Date(modeloSimulado$fecha[!is.na(modeloSimulado$fecha)]))) %>% 
              dplyr::select(Comienzo, Final, R.modificado)   
            
          }
        }
    
        incProgress(.1)
        Dias_interv <<- input$Dias_interv
        trigger_Porc_crit <<- input$trigger_Porc_crit
        trigger_R_inter   <<- input$trigger_R_inter
    
      # proy rrhh o epi
        if(proy_rrhh$x==1 | proy_epi$x==1){
        
          # get values from inputs
          periodoPreinfPromedio <- ifelse(is.null(input$PerPreinfProm),periodoPreinfPromedio,input$PerPreinfProm)
          duracionMediaInf <- ifelse(is.null(input$DurMediaInf),duracionMediaInf,input$DurMediaInf)
          porcentajeCasosCriticos <- ifelse(is.null(input$PorcCasosCrit),porcentajeCasosCriticos,input$PorcCasosCrit/100)
          diasHospCasosCriticos <- ifelse(is.null(input$DíasHospCrit),diasHospCasosCriticos,input$DíasHospCrit)
          diasUCICasosCriticos <- ifelse(is.null(input$DíasUCICrit),diasUCICasosCriticos,input$DíasUCICrit)
          tasaLetalidadAjustada <- ifelse(is.null(input$IFR),tasaLetalidadAjustada,input$IFR/100)
          ventiladoresCamaCritica <- ifelse(is.null(input$Vent_por_CC),
                                            ventiladoresCamaCritica,input$Vent_por_CC/100)
          # solo lo cambio si se emtió en epi (devuelve null si entra en la pestaña)
          if(proy_epi$x==1){
            variacion_escenario$x <- input$variacion_escenario
            prop_susceptible$x <- input$prop_susceptible/100
          }
        }
    
    # globales para graf
    camasCriticas <<- ifelse(is.null(input$CC_set),camasCriticas,input$CC_set)
    ventiladores <<- ifelse(is.null(input$Vent_set),ventiladores,input$Vent_set)
    medicosCamasUCI <<- ifelse(is.null(input$Med_set),medicosCamasUCI,input$Med_set)
    enfermerasCamasUCI <<- ifelse(is.null(input$Enf_set),enfermerasCamasUCI,input$Enf_set)
    porcentajeDisponibilidadCamasCOVID <<- ifelse(is.null(input$Porc_crit),
                                                  porcentajeDisponibilidadCamasCOVID,input$Porc_crit/100)
    
    
    incProgress(.1)
    
    # Proyecta
      # completo período de proyección
    Final_proy = max(as.Date(modeloSimulado$fecha[!is.na(modeloSimulado$fecha)]))
    Rusuario$Final[nrow(Rusuario)] <- Final_proy
    
    # s/ tipo
    source("seir.R", encoding = "UTF-8")
    iecs <- seir(tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                 hoy_date = hoy, 
                 R0_usuario = Rusuario)
    modeloSimulado <<- iecs$modeloSimulado %>% as.data.frame()
    modeloSimulado_hi <<- seir(tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                               hoy_date = hoy, variacion = variacion_escenario$x,
                               R0_usuario = Rusuario)$modeloSimulado %>% as.data.frame()
    modeloSimulado_low <<- seir(tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                                hoy_date = hoy,  variacion = -variacion_escenario$x,
                                R0_usuario = Rusuario)$modeloSimulado %>% as.data.frame()
    fechaIntervencionesTrigger <<- iecs$fechatrigger
    resumenResultados <<- crea_tabla_rr(modeloSimulado = modeloSimulado)
    crea_tabla_inputs()
    
    incProgress(.5)
    
    # grafica 
    callModule(graficando, "graficos")
    incProgress(.2)
    
    # textos
    output$titulo_tabla_resultados <-renderText({  paste0("Tabla de resultados (", poblacion_data$label[poblacion_data$pais==input$pais][1],")") })
    output$poblacion <- renderText({ fnum(poblacion_data$value[poblacion_data$pais==input$pais][1]) })
    output$poblacion65mas <- renderText({ paste0(fnum(poblacion_data$value[poblacion_data$pais==input$pais][2]),"%") })
    output$camasCriticas <- renderText({ as.character(camasCriticas) })
    output$ventiladores <- renderText({ as.character(ventiladores) })
    
    # le comunica qué proyectó
    output$default_o_no <- renderText({
      # browser()
      HTML(paste0("Proyección según los parámetros especificados."
                  # ,
                  # ifelse(proy_politicas$x==1,"Políticas de intervención",""),
                  # ifelse(proy_epi$x==1,"Epidemiología",""),
                  # ifelse(proy_rrhh$x==1,"Recursos Sanitarios",""))
      ))
    })  
    
    # para informe
    tabla_resultados_reporte$tabla =  rbind(
                t(c(resumenResultados[c(2,1,3),3],"No Aplica","No Aplica")),
                t(c(resumenResultados[c(5,4,6),3],"No Aplica","No Aplica")),
                t(c(resumenResultados[c(10,7,8,9,11),3])),
                t(c(resumenResultados[c(15,13,16,14,17),3]))) %>% as.data.frame() %>%  
                mutate(Indicador = c("Nuevas Infecciones","Nuevas Defunciones",
                                     "Camas","Ventiladores")) %>%
                setNames(c("Fecha pico", "Cantidad pico","Días hasta el pico",
                           "% saturación","Primer día saturación","Indicador")) %>% 
                dplyr::select(6,1:5) 
    
    # tabla Resultados
    output$tableResults <- renderDT({
      tResult <- tabla_resultados_reporte$tabla
        datatable(tResult %>% as.data.frame(), 
                rownames = F,
                options = list(lengthChange = FALSE,
                               searching = F, paging = F,
                               ordering = F, info = F,
                               autoWidth = T,
                               columnDefs = list(list(className = 'dt-center', targets = 0:5))),
                style = 'bootstrap', class = 'table-bordered')
    })
    
    # grafica R resultante de proyeccion
    output$g_R_actual <- renderDygraph({
      r_proyeccion_actual$Rs <- modeloSimulado %>% 
        dplyr::select(fecha, R0Usuario) %>% 
        filter(R0Usuario!=0, !is.na(fecha)) %>% as.data.frame()
      dg <- xts(r_proyeccion_actual$Rs[,2], order.by = as.Date(r_proyeccion_actual$Rs[,1]))
      dy <- dygraph(dg) %>% 
        dySeries("V1", label = "R") %>% 
        dyAxis("y", valueRange = c(.8, 3)) %>% 
        dyOptions(drawGrid = F, stepPlot = T)  
      if(trigger_on_app==1){
        dy <- dy %>% dyEvent(x = fechaIntervencionesTrigger, 
                             label = rep("Intervención",length(fechaIntervencionesTrigger)), labelLoc = "bottom", 
                             color = "grey", strokePattern = "dotted")
      }
      dy %>% dyEvent(x = hoy, label = "Hoy", labelLoc = "top", 
                     color = "grey", strokePattern = "dashed")
    })
    
    incProgress(.1)
    
    # resetea cambios
    proy_politicas$x=0
    proy_rrhh$x=0
    proy_epi$x=0
    
    # predefinido por defecto
    disable("EscenarioActual")
    updateSelectInput(session, "escenarioPredefinido",selected = "constanteR_cori")

    # move to see results
    shinyjs::runjs('document.getElementById("inicio").scrollIntoView();')
  })
    }else{
          showNotification("No ha modificado ningún parámetro.",
                           type = "warning")
    }
})
  
  

# escenario predefinido ---------------------------------------------------

observeEvent(input$aplicarEscenario | input$EscenarioActual, ignoreInit = T,{
  
  # proyecta
  withProgress(message = "Proyectando...",{
    
  # browser()
  trigger_on_app <<- 0
  
  # update parameteres on predefined selection
  if(input$escenarioPredefinido=='constanteR_cori'){
    Rusuario <- data.frame(
      Comienzo = hoy,
      Final =    max(as.Date(modeloSimulado$fecha[!is.na(modeloSimulado$fecha)])),
      R.modificado = r_cori)
  }else if(input$escenarioPredefinido=='constante1,3'){
    Rusuario <- data.frame(
      Comienzo = hoy,
      Final =    max(as.Date(modeloSimulado$fecha[!is.na(modeloSimulado$fecha)])),
      R.modificado = 1.3)
  }else if(input$escenarioPredefinido=='trigger50'){
    trigger_on_app <<- 1
    Rusuario <- data.frame(
                  Comienzo = hoy,
                  Final =    max(as.Date(modeloSimulado$fecha[!is.na(modeloSimulado$fecha)])),
                  R.modificado = 1.3)  

    updateSliderInput(session, "trigger_R_inter", value = 1.1)
    updateSliderInput(session, "trigger_R_base", value = 1.3)
    updateKnobInput(session, "trigger_Porc_crit", value = 50)
    updateNumericInputIcon(session, "Dias_interv", value = 30)
    
    Dias_interv <<- 30
    trigger_Porc_crit <<- 50
    trigger_R_inter   <<- 1.1
    
  }else if(input$escenarioPredefinido=='trigger70'){
    trigger_on_app <<- 1
    Rusuario <- data.frame(
      Comienzo = hoy,
      Final =    max(as.Date(modeloSimulado$fecha[!is.na(modeloSimulado$fecha)])),
      R.modificado = 1.3)  

    updateSliderInput(session, "trigger_R_inter", value = 1.1)
    updateSliderInput(session, "trigger_R_base", value = 1.3)
    updateKnobInput(session, "trigger_Porc_crit", value = 70)
    updateNumericInputIcon(session, "Dias_interv", value = 30)
    
    Dias_interv <<- 30
    trigger_Porc_crit <<- 70
    trigger_R_inter   <<- 1.1
    
  }else if(input$escenarioPredefinido=='valvular1'){
    fecha_final =  max(as.Date(modeloSimulado$fecha[!is.na(modeloSimulado$fecha)]))
    Rusuario <- 
          tibble(Comienzo = seq(hoy, hoy+800, 30),
                 Final = Comienzo + 29,
                 R.modificado = rep(c(1.1,1.3),
                                    length.out=length(Comienzo))) %>% 
          filter(Final < fecha_final) %>% as.data.frame()
  }
  
  # proy rrhh o epi
  if(proy_rrhh$x==1 | proy_epi$x==1){
    
    # get values from inputs
    # get values from inputs
    periodoPreinfPromedio <- ifelse(is.null(input$PerPreinfProm),periodoPreinfPromedio,input$PerPreinfProm)
    duracionMediaInf <- ifelse(is.null(input$DurMediaInf),duracionMediaInf,input$DurMediaInf)
    porcentajeCasosCriticos <- ifelse(is.null(input$PorcCasosCrit),porcentajeCasosCriticos,input$PorcCasosCrit/100)
    diasHospCasosCriticos <- ifelse(is.null(input$DíasHospCrit),diasHospCasosCriticos,input$DíasHospCrit)
    diasUCICasosCriticos <- ifelse(is.null(input$DíasUCICrit),diasUCICasosCriticos,input$DíasUCICrit)
    tasaLetalidadAjustada <- ifelse(is.null(input$IFR),tasaLetalidadAjustada,input$IFR/100)
    ventiladoresCamaCritica <- ifelse(is.null(input$Vent_por_CC),
                                      ventiladoresCamaCritica,input$Vent_por_CC/100)
    
    # solo lo cambio si se emtió en epi (devuelve null si entra en la pestaña)
    if(proy_epi$x==1){
      variacion_escenario$x <- input$variacion_escenario
      prop_susceptible$x <- input$prop_susceptible/100
    }
  }
  
  # globales para graf
  camasCriticas <<- ifelse(is.null(input$CC_set),camasCriticas,input$CC_set)
  ventiladores <<- ifelse(is.null(input$Vent_set),ventiladores,input$Vent_set)
  medicosCamasUCI <<- ifelse(is.null(input$Med_set),medicosCamasUCI,input$Med_set)
  enfermerasCamasUCI <<- ifelse(is.null(input$Enf_set),enfermerasCamasUCI,input$Enf_set)
  porcentajeDisponibilidadCamasCOVID <<- ifelse(is.null(input$Porc_crit),
                                                porcentajeDisponibilidadCamasCOVID,input$Porc_crit/100)
  
  incProgress(.2)
  
  Final_proy = max(as.Date(modeloSimulado$fecha[!is.na(modeloSimulado$fecha)]))
  Rusuario$Final[nrow(Rusuario)] <- Final_proy
  
  # s/ tipo
  source("seir.R", encoding = "UTF-8")
  iecs <- seir(tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
               hoy_date = hoy, 
               R0_usuario = Rusuario)
  modeloSimulado <<- iecs$modeloSimulado
  modeloSimulado_hi <<- seir(tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                             hoy_date = hoy, variacion = variacion_escenario$x,
                             R0_usuario = Rusuario)$modeloSimulado
  modeloSimulado_low <<- seir(tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                              hoy_date = hoy,  variacion = -variacion_escenario$x,
                              R0_usuario = Rusuario)$modeloSimulado
  fechaIntervencionesTrigger <<- iecs$fechatrigger
  resumenResultados <<- crea_tabla_rr(modeloSimulado = modeloSimulado)
  crea_tabla_inputs()
  
  incProgress(.5)
  
  # grafica 
  callModule(graficando, "graficos")
  
  incProgress(.2)
  
  # textos
  output$titulo_tabla_resultados <-renderText({  paste0("Tabla de resultados (", poblacion_data$label[poblacion_data$pais==input$pais][1],")") })
  output$poblacion <- renderText({ fnum(poblacion_data$value[poblacion_data$pais==input$pais][1]) })
  output$poblacion65mas <- renderText({ paste0(fnum(poblacion_data$value[poblacion_data$pais==input$pais][2]),"%") })
  output$camasCriticas <- renderText({ as.character(camasCriticas) })
  output$ventiladores <- renderText({ as.character(ventiladores) })
  
  # le comunica qué proyectó
  output$default_o_no <- renderText({
    # browser()
    HTML(paste0("Proyección según los parámetros especificados."
                # ,
                # ifelse(proy_politicas$x==1,"Políticas de intervención",""),
                # ifelse(proy_epi$x==1,"Epidemiología",""),
                # ifelse(proy_rrhh$x==1,"Recursos Sanitarios",""))
    ))
  })  
  
  # tabla Resultados
  output$tableResults <- renderDT({
    tResult <- rbind(
      t(c(resumenResultados[c(2,1,3),3],"No Aplica","No Aplica")),
      t(c(resumenResultados[c(5,4,6),3],"No Aplica","No Aplica")),
      t(c(resumenResultados[c(10,7,8,9,11),3])),
      t(c(resumenResultados[c(15,13,16,14,17),3]))
    ) %>% as.data.frame() %>%  
      mutate(Indicador = c("Nuevas Infecciones","Nuevas Defunciones",
                           "Camas","Ventiladores")) %>%
      setNames(c("Fecha pico", "Cantidad pico","Días hasta el pico",
                 "% saturación","Primer día saturación","Indicador")) %>% 
      dplyr::select(6,1:5)
    datatable(tResult %>% as.data.frame(), 
              rownames = F,
              options = list(lengthChange = FALSE,
                             searching = F, paging = F,
                             ordering = F, info = F,
                             autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = 0:5))),
              style = 'bootstrap', class = 'table-bordered')
  })
  
  # tabla inputs para reporte
  tabla_rrhh_epi$x = tablaInputs %>% slice(1,2,4,7,8,9,22,23,28,17)
  
  # grafica R resultante de proyeccion
  output$g_R_actual <- renderDygraph({
    r_proyeccion_actual$Rs <- modeloSimulado %>% 
                              dplyr::select(fecha, R0Usuario) %>% 
                              filter(R0Usuario!=0, !is.na(fecha)) %>% as.data.frame()
    dg <- xts(r_proyeccion_actual$Rs[,2], order.by = as.Date(r_proyeccion_actual$Rs[,1]))
    dy <- dygraph(dg) %>% 
            dySeries("V1", label = "R") %>% 
      dyAxis("y", valueRange = c(.8, 3)) %>% 
            dyOptions(drawGrid = F, stepPlot = T)  
    if(trigger_on_app==1){
      dy <- dy %>% dyEvent(x = fechaIntervencionesTrigger, 
                           label = rep("Intervención",length(fechaIntervencionesTrigger)), labelLoc = "bottom", 
                           color = "grey", strokePattern = "dotted")
    }
    dy %>% dyEvent(x = hoy, label = "Hoy", labelLoc = "top", 
                    color = "grey", strokePattern = "dashed")
  })
  
  incProgress(.1)
  
  # resetea cambios
  proy_politicas$x=0
  proy_rrhh$x=0
  proy_epi$x=0
  
  # habilitar constante en escenario por defecto
  enable("EscenarioActual")
  
  # move to see results
  shinyjs::runjs('document.getElementById("inicio").scrollIntoView();')

  })
})
  
# reporte -----------------------------------------------------------------

  
  output$report <- downloadHandler(
    
    filename = paste0("Report_",hoy,".pdf"),
    
    content = function(file) {

      id <- showNotification("Generando reporte...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      # report_path <- tempfile(fileext = ".Rmd")
      # file.copy("report.Rmd", report_path, overwrite = TRUE)

      params <- list(pais = pais_actual_label,
                     hoy = hoy,
                     tabla_results = tabla_resultados_reporte$tabla,
                     tabla_inputs = tabla_rrhh_epi$x,
                     data = modeloSimulado)
      
      rmarkdown::render("report.Rmd",
                        output_file = file,
                        output_format = "pdf_document",
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

##### Run
hoy <- as.Date("2020-11-18")
shinyApp(ui = ui, server = server)

##### Deploy
# setwd("appTest/")
# library("rsconnect")
# rsconnect::setAccountInfo(name='iecs',
#                           token='9A0732889DEC27389E9B36601BDFA488',
#                           secret='O1M8egrg5PYXJvqewshgNBsOiStMfmVufZ9KrH6W')
# deployApp(appName = "app_test2", forceUpdate = T, account = "iecs")

