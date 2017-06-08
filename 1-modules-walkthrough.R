
library(shiny)

## APP
ui <- fluidPage(
  sliderInput("slider","Slide Me", 0, 100 , 1),
  textOutput("num")
)
server <- function(input,output){
  output$num <- renderText({
    input$slider
  })
}
shinyApp(ui,server)


## put UI in a fun
sliderInputTextUI <- function(){
  # UI
  tagList(
  sliderInput("slider","Slide Me", 0, 100 , 1),
  textOutput("num")
  )
}
ui <- fluidPage(
  sliderInputTextUI()
)
server <- function(input,output){
  output$num <- renderText({
    input$slider
  })
}
shinyApp(ui,server)



## Try to reuse the UI component - doesn't work. Same id
sliderInputTextUI <- function(){
  # UI
  tagList(
    sliderInput("slider","Slide Me", 0, 100 , 1),
    textOutput("num")
  )
}
ui <- fluidPage(
  sliderInputTextUI(),
  sliderInputTextUI()
)
server <- function(input,output){
  output$num <- renderText({
    input$slider
  })
  output$num <- renderText({
    input$slider
  })
}
shinyApp(ui,server)


## Make sure you give different id's

sliderInputTextUI <- function(sliderId, textId){
  # UI
  tagList(
    sliderInput(sliderId,"Slide Me", 0, 100 , 1),
    textOutput(textId)
  )
}
ui <- fluidPage(
  sliderInputTextUI("slider1","num1"),
  sliderInputTextUI("slider2","num2")
)
server <- function(input,output){
  output$num1 <- renderText({
    input$slider1
  })
  output$num2 <- renderText({
    input$slider2
  })
}
shinyApp(ui,server)


## Make sure you give different id's

sliderInputTextUI <- function(sliderId, textId){
  # UI
  tagList(
    sliderInput(sliderId,"Slide Me", 0, 100 , 1),
    textOutput(textId)
  )
}
ui <- fluidPage(
  sliderInputTextUI("slider1","num1"),
  sliderInputTextUI("slider2","num2")
)
server <- function(input,output){
  output$num1 <- renderText({
    input$slider1
  })
  output$num2 <- renderText({
    input$slider2
  })
}
shinyApp(ui,server)

###

ns <- NS("hello")
ns("world")

### Using modules

sliderInputTextUI <- function(id){
  # UI
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"),"Slide Me", 0, 100 , 1),
    textOutput(ns("text"))
  )
}
ui <- fluidPage(
  sliderInputTextUI("first"),
  sliderInputTextUI("second")
)

sliderInputText <- function(input,output,session){
  # Server
  ## Do not use ns in the server side
  output$text <- renderText({
    input$slider
  })
}
server <- function(input,output){
  callModule(sliderInputText, "first") ### Same id of the ui component
  callModule(sliderInputText, "second") ### Same id of the ui component
}
shinyApp(ui,server)


### Using modules with arguments

sliderInputTextUI <- function(id, label = "Slide me"){
  # UI
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"),label, 0, 100 , 1),
    textOutput(ns("text"))
  )
}
ui <- fluidPage(
  sliderInputTextUI("first",label = "First slide"),
  sliderInputTextUI("second")
)

sliderInputText <- function(input,output,session, add = 0){
  # Server
  ## Do not use ns in the server side
  output$text <- renderText({
    input$slider + add
  })
  sliderVal <- reactive(input$slider)
}
server <- function(input,output){
  callModule(sliderInputText, "first") ### Same id of the ui component
  callModule(sliderInputText, "second", add = 10) ### Same id of the ui component
}
shinyApp(ui,server)


### Using modules with reactives

sliderTextUI <- function(id, label = "Slide me"){
  # UI
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"),label, 0, 100 , 1),
    textOutput(ns("text"))
  )
}
sliderText <- function(input,output,session, show){
  output$text <- renderText({
    if(show()) input$slider
    else NULL
  })
}
ui <- fluidPage(
  checkboxInput("display","Show Value"),
  sliderTextUI("module")
)
server <- function(input,output){
  display <- reactive({ input$display})
  callModule(sliderText,"module",display)
}
shinyApp(ui,server)

## Reactive output from module

sliderTextUI <- function(id, label = "Slide me"){
  # UI
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"),label, 0, 100 , 1),
    textOutput(ns("text"))
  )
}
sliderText <- function(input,output,session){
  output$text <- renderText(input$slider)
  reactive(input$slider)
}
ui <- fluidPage(
  sliderTextUI("module"),
  h2(textOutput("value"))
)
server <- function(input,output){
  val <- callModule(sliderText, "module")
  output$value <- renderText({
    val()
    })
}
shinyApp(ui,server)



### Using modules with reactives

sliderInputTextUI <- function(id, label = "Slide me"){
  # UI
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"),label, 0, 100 , 1),
    textOutput(ns("text"))
  )
}
ui <- fluidPage(
  sliderInputTextUI("first",label = "First slide"),
  sliderInputTextUI("second")
)

sliderInputText <- function(input,output,session, add = 0){
  # Server
  ## Do not use ns in the server side
  output$text <- renderText({
    input$slider + add
  })
  reactive(input$slider)
}
server <- function(input,output){
  callModule(sliderInputText, "first") ### Same id of the ui component
  x <- callModule(sliderInputText, "second", add = 10) ### Same id of the ui component
  observe({
    message(x())
  })
}
shinyApp(ui,server)






