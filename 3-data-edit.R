library(shiny)
library(tidyverse)
library(rhandsontable)

# Data edit module

dataEditUI <- function(id){
  ns <- NS(id)
  tagList(
    column(3,
           uiOutput(ns("dataControls"))
    ),
    column(9,
           rHandsontableOutput(ns("dataInputPreview"))
    )
  )
}

dataEdit <- function(input, output, session,
                     inputData,
                     addColSelect = TRUE,
                     addRowFilters = FALSE,
                     addCtypes = FALSE){

  output$dataControls <- renderUI({
    if(is.null(inputData())) return()
    ns <- session$ns
    d <- inputData()

    colSelect <- selectizeInput(ns("selectedCols"),"Select Cols",
                                 choices = names(d),
                                 selected = names(d),
                                 multiple = TRUE,
                                 options = list(plugins = list("drag_drop", "remove_button")))

    rowFilters <- list(
      checkboxInput(ns("dataAddFilters"),"Add filters"),
      conditionalPanel(paste0("input[['",ns("dataAddFilters"),"']]"),
                       p("Here goes the filters"))
    )
    ctypes <- list(
      checkboxInput(ns("dataAddColTypes"), "Add ColTypes"),
      conditionalPanel(paste0("input[['",ns("dataAddColTypes"),"']]"),
                       p("Here goes the ctypes"))
    )
    if(!addColSelect) colSelect <- NULL
    if(!addRowFilters) rowFilters <- NULL
    if(!addCtypes) ctypes <- NULL

    tagList(
      colSelect,
      rowFilters,
      ctypes
    )
  })

  output$dataInputPreview <- renderRHandsontable({
    d <- inputData()
    selectedCols <- input$selectedCols
    #d <- d %>% select_(.dots = selectedCols)
    d <- d[selectedCols]
    if(is.null(inputData()))
      return()
    h <- rhandsontable(d, useTypes = FALSE, readOnly = FALSE,
                       width = "100%",height = 500) %>%
      hot_table(stretchH = "none") %>%
      hot_cols(manualColumnMove = TRUE)
    h
  })

  data <- reactive({
    if(is.null(input$dataInputPreview))
      return()
    as_tibble(hot_to_r(input$dataInputPreview))
  })
  data
}


ui <- fluidPage(
  dataEditUI("dataEdit"),
  verbatimTextOutput("debug")
)

server <- function(input,output,session){
  inputData <- reactive({
    mtcars
  })

  data <- callModule(dataEdit, "dataEdit", inputData = inputData)
  output$debug <- renderPrint({
    data()
  })
}
shinyApp(ui,server)







