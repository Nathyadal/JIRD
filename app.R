options(shiny.maxRequestSize=100*1024^2)
library(googleVis) ## Version >= 0.3.2 required
library(knitr)
library(markdown)
library(RCurl)
library("rvest")
library('xml2')
shinyApp(
    ui = fluidPage(
        tags$style(type="text/css",
        "#loadmessage {
             position: fixed;
             top: 0px;
             left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 100%;
             color: #000000;
             background-color: #CCFF66;
             z-index: 105;
           }"),
        titlePanel("Propuesta JIRD!"),
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
        downloadButton("report", "Generate report"),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Generando reporte, por favor espere...",id="loadmessage")
                    
        )
    ),
    server = function(input, output) {
        url <- "https://github.com/Nathyadal/JIRD/blob/07ab4db47c521d4a0e110697460a487de1dcaa27/MBC_MDJIRD.csv"
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
)
