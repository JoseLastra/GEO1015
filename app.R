##Author: José Lastra - Macarena Pérez 
## July 2020
##Choroplet maps for GEO1015

library(shiny)
library(shinythemes)
library(shinyalert)
library(sf)
library(dplyr)
library(cartography)
library(RColorBrewer)
library(tmap)
library(DT)
#########################################################
##Functions
source('plot.R')
#######################################################
# User interface ----
  ui <- navbarPage(title = 'GEO1015',theme =shinytheme("spacelab") ,
  tabPanel(title = 'Panel principal',
    sidebarPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }",
                 "* { font-family: Arial Narrow; }"
      ),
      actionButton("infoProject", label = 'Sobre la página'),hr(),
      #subida de arhivo
            selectInput('region',
                  label = 'Seleccione una región',
                  choices=c('REGIÓN DE LA ARAUCANÍA',
                            "REGIÓN DEL LIBERTADOR GENERAL BERNARDO O''HIGGINS",
                            'REGIÓN DE VALPARAÍSO','REGIÓN DEL MAULE',
                            'REGIÓN METROPOLITANA DE SANTIAGO')),
      fileInput('target_upload', 'Suba su tabla',accept = c('text/csv',
                                                              'text/comma-separated-values',
                                                                '.csv')),
      #selector de separador
      radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
      uiOutput("datos"),#salida de campos elegibles
      selectInput("breaks", #tipo de quiebres 
                  label = "Tipo de clasificación de histograma",
                  choices = c("Cuantiles", "Quiebres naturales",
                              "Geométricos", "Desviación estándar",
                              'Intervalos iguales'),
                  selected = "Quiebres naturales"),
      numericInput('nBreaks','N°de clases',value = 5),
      selectInput("paleta", #tipo de quiebres 
                  label = "Acá deberían elegir paleta",
                  choices = c("YlOrRd" = 'YlOrRd',
                              "Spectral" = 'Spectral',
                              "YlGnBu" = 'YlGnBu',
                              "RdBu" = 'RdBu'),
                  selected = "YlOrRd"),
      fixedRow(column(checkboxInput('reverse',label = 'Inventir paleta',FALSE),width = 2),
      column(checkboxInput('label',label = 'Etiquetas',FALSE),offset=2,width = 2)),
      actionButton("graf", label = 'Desplegar información'),hr(),
      fixedRow(column(HTML("<img src='logo1.png' class='img-responsive' width='220' height='220'/>"),
                      width = 4,offset = 1),
               column(HTML("<img src='logo2.png' class='img-responsive' width='220' height='220'/>"),
                      width = 4,offset = 1)),useShinyalert(),width = 3
      ),
     mainPanel(plotOutput("map",height = '800px'),br(),dataTableOutput(outputId = "df"))
  ),
  tabPanel('Instrucciones',
           sidebarLayout(sidebarPanel(#Credits section
             h5(strong("Generado en base a:")),
             h5(a("R Project",href="https://www.r-project.org/"), 
                "| Statistical Computing"),
             h5(a("shiny (v1.4.0)",href=" https://CRAN.R-project.org/package=shiny"), 
                "| Web Application Framework for R"),
             h5(a(href="https://CRAN.R-project.org/package=choroplethr", 
                  "choropletr (v3.6.3)"), "| Simplify the Creation of Choropleth Maps in R"),
             h5(a(href="https://CRAN.R-project.org/package=sf", 
                  "sf (v0.9-4)"), "| Simple Features for R"),hr(),
             h5(strong('Desarrollo:')),h5('MSc. José Lastra.'),
             h5(p('Contacto: jose.lastra@pucv.cl')),
             h5(strong('Docente GEO1015:')),h5('Matías Olea'),h5(p('Contacto: matias.olea@pucv.cl')),
             h5(strong('Ayudantes GEO1015: ')),h5('Emilio Bustos – Ignacio López.'),
             ), 
             mainPanel(h5(strong('Instrucciones')),div(style="text-align:justify",p("La aplicación", strong(em("GEO1015 v0.5")), "requiere que el usuario escoja una de las 5 regiones político-administrativas disponibles: Valparaíso, Metropolitana, O’Higgins, Maule y La Araucanía."),
             HTML('<center><img  src="inst1.png" class="img-responsive"  width="400" height="300" align="middle" ></center>'),
             br(),
             p("El siguiente paso es subir a la plataforma una matriz de datos en formato de texto separado por comas (.*csv) y el punto (.) como separador de decimales. Sin perjuicio de lo anterior, la plataforma es capaz de reconocer si su matriz no se encuentra separa por comas (,) sino por algún otro separador, ya sea dos puntos (:) o punto y coma (;). Debe tener en consideración que esta plataforma trabaja con los códigos oficiales de la División Política Administrativa de Chile, por lo que para que se una la matriz a la fuente de datos espaciales (mapa de la región), su matriz debe poseer una columna llamada “COD_COMUNA” que contenga los códigos respectivos en formato de número, como se muestra en la siguiente imagen."),
             HTML('<center><img  src="inst2.png" class="img-responsive"  width="350" height="280" align="middle" ></center>'),
             br(),
             p("Una vez la región haya sido seleccionada y la matriz correctamente cargada, se activará una nueva opción donde debe escoger que columna de su matriz desea representar en su mapa. Adicionalmente, tiene la opción de cambiar algunos parámetros de visualización como por ejemplo la clasificación de los histogramas de sus datos, el numero de clases o quiebres, el color de su paleta de colores, la posibilidad de invertir dicha paleta y agregar etiquetas de nombres para las comunas."),
             HTML('<center><img  src="inst3.png" class="img-responsive" width="400" height="300" align="middle" ></center>'),
             br(),
             HTML('<center><img  src="inst4.png" class="img-responsive"   align="middle" ></center>'),
             h6(em('Mapa resultado ejemplo desplegado por la aplicación'),align='center')
        )
      ) 
    )
  )
)

# Server logic ----
server <- function(session,input, output) {
  
  #reads shape
  capaOriginal<-read_sf('data/Regiones_EC_MC1.shp')
  capaOriginal$AREA<-st_area(capaOriginal)
  shp<-function(){
    capa<-filter(capaOriginal,NOM_REGION==input$region)
    }
  datos <- reactive({
    req(input$target_upload)
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
   
    return(df)
  })
  
  #Límites disclaimer
 shinyalert(text = "Los mapas publicados en esta página, que se refieren o se relacionan con los límites y fronteras de Chile, no comprometen en modo alguno al Estado de Chile, de acuerdo al Artículo 2°, letra g del DFL N°83 de 1979, del Ministerio de Relaciones Exteriores.",type = 'info')
  
  #modals section for info buttons
  observeEvent(input$infoProject, {
    showModal(modalDialog(
      title = strong("Información general"),#Project summary
      h4("La aplicación GEO1015 v0.5 está diseñada para el mapeo de variables cuantitativas cuya unidad mínima de análisis (UMA) corresponde a la comuna. Por otra parte, este sistema permite la toma de decisiones a la hora de clasificar cada uno de los quiebres de nuestros datos para obtener resultados que se asemejen a la realidad."),
      h4("Por ahora cuenta la disponibilidad de realizar mapeos en las regiones de: Valparaíso, Metropolitana, O’Higgins, Maule y La Araucanía, como pretexto de la evaluación clave del curso de Métodos Cuantitativos en Geografía 1 del año 2020.")
      ,footer = NULL,easyClose = T,size = "m",fade = T))
  })
  
    #cod as character
    #df()$COD_COMUNA <- as.character(df()$COD_COMUNA)
   
    output$datos<- renderUI({
      req(input$target_upload)
      df1<-datos()
      shp1<-shp()
      df1$COD_COMUNA <- as.character(df1$COD_COMUNA)
      tabla_pob<-merge(shp1, df1, by.x = "COMUNA", by.y = "COD_COMUNA")
      data.num<-select_if(tabla_pob, is.numeric)
      selectInput(
        "columna",
        "Seleccione un campo",
        colnames(data.num), multiple = F 
      )

    })
  
#Coropleta
      output$map<-renderPlot({
        req(input$target_upload)
        req(input$graf)
        #cod as character
        df2<-datos()
        df2$COD_COMUNA <- as.character(df2$COD_COMUNA)
        #join data and shape
        shp1<-shp()
        shp1$label<-shp1$NOM_COMUNA
        tabla_pob<-function(){
          tab<-merge(shp1, df2, by.x = "COMUNA", by.y = "COD_COMUNA")
          
          }
        # input parameters for plot choroplet
        campo<- input$columna
        etiqueta<-input$label
        quiebres <- switch(input$breaks, 
                           "Cuantiles" = 'quantile',
                           "Quiebres naturales" = 'fisher-jenks',
                           "Geométricos" = 'geom',
                           "Desviación estándar" = 'sd',
                           "Intervalos iguales"='equal')
        paleta <- switch(input$paleta, 
                           "YlOrRd" = 'YlOrRd',
                           "Spectral" = 'Spectral',
                           "YlGnBu" = 'YlGnBu',
                           "RdBu" = 'RdBu')
        
        nClases<-input$nBreaks
        revertir<-input$reverse
        #plot function
        coropleta(campo=campo,quiebres=quiebres,
                  paleta=paleta,nClases=nClases,
                  revertir = revertir,shp=tabla_pob(),etiqueta=etiqueta)
        
      },execOnResize = T,height = 800,res=100)

      output$df<-renderDataTable({
        req(input$graf)
                datos()
      })
      
}


#shinyapp

shinyApp(ui = ui, server = server)



