options(shiny.maxRequestSize=100*1024^2)
library(shiny)
library(tidyverse)
library(googleVis) ## Version >= 0.3.2 required
library(knitr)
library(markdown)
library(RCurl)
library("rvest")
library(shinydashboard)
library(shinycssloaders)
library(quantmod)
library(DT)
    
ui <- dashboardPage(
                           
            dashboardHeader(title = "Propuesta JIRD"),
            dashboardSidebar(
                sidebarMenu(
                    menuItem("Vistazo Rápido", tabName = "page2", icon = icon("home")),
                    menuItem("Análisis dinámico", tabName = "Analisis", icon = icon("calendar")),
                    uiOutput('style_tag'))
                ),
            dashboardBody(
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 plotOutput("Test") %>% withSpinner(color="#0dc5c1"),
                                 tags$div("Estamos preparando todo, por favor espere...",id="loadmessage")
                                 
                ),
                tabItems(
                    tabItem(tabName = "Analisis",
                            tags$h1("Análisis de población estudiantil y el acceso a TIC's"),
                            # Horizontal line ----
                            tags$hr(),
                            # Input: Select a file ----
                            fileInput("file1", "Seleccione el archivo a procesar",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            tags$hr(),
                            uiOutput('ui.action'),
                            # Horizontal line ----
                            tags$hr(),
                            downloadButton("report", "Generar reporte"),
                    ),#FIN TAB 1
                    tabItem(tabName = "page2", 
                            tags$h1('Analisis de datos!'),
                            
                            selectInput("variable", "Archivo a analizar:",
                                        c("2019" = "https://raw.githubusercontent.com/adalrada/JIRD/main/enaho19_hogares_df_challenge.csv",
                                          "2020" = "https://raw.githubusercontent.com/adalrada/JIRD/main/enaho20_hogares_df_challenge.csv")),
                                    helpText("Selecciona la zona y cantidad de dispositivos"),
                            frow1 <- fluidRow(
                                valueBoxOutput("value1")#CANTIDAD DE ESTUDIANTES
                                ,valueBoxOutput("value2")#CANTIDAD DE HOGARES
                                ,valueBoxOutput("value3")#ACCESO A TICS
                            ),
                            tabsetPanel(
                                tabPanel("Resumen de datos", DTOutput('tbl')),
                                tabPanel("Graficos",
                                         selectInput("variableanalisis", "Generar grafico distribucion de variable:",
                                                     c("Tenencia de Tics" = "2",
                                                       "Cantidad de telefonos" = "3",
                                                       "Cantidad tablets"="4",
                                                       "Cantidad Pc escritorio"="5",
                                                       "Cantidad laptop"="6")),
                                         plotOutput("plot1", click = "plot_click"),
                                         verbatimTextOutput("info")
                                )
                                
                            )
                              
                    )#FIN TAB 2
                    
                )#FIN TABS
                
                ))##CIERRE DE UI

server <- function(input, output) {
        #GRAFICO MUESTRA
        mydata <- reactive({
            inFile <- input$variable
            if (is.null(inFile))
               return(NULL)
            tbl <- read.csv(input$variable,header = TRUE,sep =";",dec=",",check.names= F,fileEncoding="latin1",stringsAsFactors=TRUE)
            TotalEstudiantes<-sum(as.integer(tbl$estudiantes))
            TOTALHOGARES <-nrow(tbl)
            
            
            ACCESOTICS<-sum(tbl$tenenciaTIC=="Sí")
            dt<-tbl[,c(3,21,22,23,24,25)]

            return(list(dt,TotalEstudiantes,TOTALHOGARES,ACCESOTICS))
        })
        
        datatables <- reactive({
            inFile <- input$variable
            if (is.null(inFile))
                return(NULL)
            tbl <- read.csv(input$variable,header = TRUE,sep =";",dec=",",check.names= F,fileEncoding="latin1",stringsAsFactors=TRUE)
            dt<-tbl[,c(3,21,22,23,24,25)]
            
            return(dt)
        })
        
        output$tbl = renderDT(
            datatables(),options = list(pageLength = 5,autoWidth = TRUE,lengthChange = FALSE)
        )
        output$plot1 <- renderPlot({
                xval <- datatables()[,as.integer(input$variableanalisis)]
                df<-cbind(datatables(), xval)
                str(df)
                ggplot(df, aes(x = factor(xval),fill=xval)) +
                    geom_bar(stat="count",fill="steelblue")+
                    stat_count(geom = "text", colour = "white", size = 4.5,
                               aes(label = ..count..),position=position_stack(vjust=0.5))+
                    labs(title = "",
                         x = "CATEGORIA",
                         y = "TOTAL",
                         caption = "INEC")
                
                #barplot(c(1,2), col=c("#009999", "#0000FF"))
        })
        #KPI
        output$value1 <- renderValueBox({
            valueBox(
                formatC(mydata()[2], format="d", big.mark=',')
                ,paste('Total de estudiantes')
                ,icon = icon("stats",lib='glyphicon')
                ,color = "purple")  
        })
        output$value2 <- renderValueBox({ 
            valueBox(
                format(mydata()[3], format="d", big.mark='.'),
                'ToTal de hogares'
                ,icon = icon("home")
                ,color = "green")  
        })
        
        output$value3 <- renderValueBox({ 
            valueBox(
                format(mydata()[4], format="d", big.mark='.'),
                'Acceso a TIC'
                ,icon = icon("fas fa-brain")
                ,color = "yellow")  
        })
        #REPORT
        url <- "https://github.com/adalrada/JIRD/blob/c6498def7529593393d2296798521ce809da127a/MBC_MDJIRD.csv"
        pagina <- read_html(url, encoding = "UTF-8")
        tables <- html_table(pagina, fill = TRUE)
        datafe <- tables[[1]]
        datafe$X2[6]<-"    html_document: default"
        datafe$X2[8]<-"    n: NA"
        write.table(datafe$X2,"MBC_MDJIRD.Rmd",na ="", row.names=FALSE,col.names = FALSE, append=FALSE, sep='\t', quote=FALSE)
        
        #Estado cargando del panel condicional 
        output$plot <- renderPlot({
            Sys.sleep(2); hist(runif(input$n)) 
            })
        output$info <- renderText({
            paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
        })
        
        #carga del archivo
        output$contents <- renderTable({
            req(input$file1)
            tryCatch(
                {
                    df <- read.csv(input$file1$datapath,
                                   header = input$header,
                                   sep = input$sep,
                                   quote = input$quote)
                },
                error = function(e) {
                    stop(safeError(e))
                }
            )
            if(input$disp == "head") {
                return(head(df))
            }
            else {
                return(df)
            }
            
        })
        #DESCARGA DE ARCHIVO
        output$report <- downloadHandler(
            filename = "JIRDreport.html",
            content = function(file) {
                Sys.sleep(2)
                tempReport <- file.path(getwd(), "MBC_MDJIRD.Rmd")
                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                rmarkdown::render(tempReport, output_file = file,
                                  params = list(n=input$file1$datapath),
                                  envir = new.env(parent = globalenv())
                )
            }
        )
        
}

shinyApp(ui, server)

