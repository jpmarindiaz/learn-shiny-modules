library(shiny)
library(tidyverse)

## Data upload module

dataInputUI <- function(id,
                        dataInputChoices =
                          list("Copy & Paste"="pasted",
                               "File Upload"="fileUpload",
                               "Sample Data"="sampleData"),
                        selected = "pasted"
){
  # UI
  ns <- NS(id)
  tagList(
    radioButtons(ns("dataInput"), "upload Data",
                 choices = dataInputChoices, selected = selected),
    uiOutput(ns("dataInputControls"))
  )
}
dataInput <- function(input,output,session, sampleFiles = c()){
  output$dataInputControls <- renderUI({
    ns <- session$ns
    message("\n\n",input$dataInput,"\n\n")
    if(input$dataInput == "sampleData"){
      if(!all(map_lgl(sampleFiles,file.exists)))
        stop("All Sample Files must exist")
    }
    dataInputControls <- list(
      "pasted" = textAreaInput(ns("inputDataPasted"),label = "Paste",
                               placeholder = "placeholder",
                               rows = 5),
      "fileUpload" =  fileInput(ns('inputDataUpload'), 'Choose CSV File',
                                accept=c('text/csv',
                                         'text/comma-separated-values,text/plain',
                                         '.csv','.xls')),
      "sampleData" = selectInput(ns("inputDataSample"),"Seleccione Datos de Muestra",
                                 choices = sampleFiles)
    )
    dataInputControls[[input$dataInput]]
  })

  inputData <- reactive({
    inputType <- input$dataInput
    #readDataFromInputType(inputType)
    if(inputType == "pasted"){
      if(input$inputDataPasted == "")
        return()
      df <- read_tsv(input$inputDataPasted)
    }
    if(inputType ==  "fileUpload"){
      if(is.null(input$inputDataUpload)) return()
      old_path <- input$inputDataUpload$datapath
      path <- file.path(tempdir(),input$inputDataUpload$name)
      file.copy(old_path,path)
      df <- rio::import(path)
    }
    if(inputType ==  "sampleData"){
      file <- input$inputDataSample
      df <- read_csv(file)
    }
    return(df)
  })

  inputData
}


ui <- fluidPage(
  dataInputUI("dataIn", dataInputChoices =
                list(
                  "Copiar y pegar"="pasted",
                  "File Upload"="fileUpload"
                )),
  verbatimTextOutput("debug")
)

server <- function(input,output,session){
  inputData <- callModule(dataInput, "dataIn",
                          sampleFile =
                            list("File1"="sample1.csv","File2"="sample2.csv"))
  output$debug <- renderPrint({
    inputData()
  })
}
shinyApp(ui,server)





