library(shiny)

names = c('bloop', 'scoop', 'doop')



ui <- fluidPage(
  sliderInput("slide", NULL, 0, 10, 1),
  numericInput("num", NULL, 1),
  lapply(1:3, function(j){
    lapply(names, function(i){
      h3(i)
    })
  })
  # }
  
  
)

server <- function(input, output, session) {
  observe({
    updateSliderInput(session, "slide", value = input$num)
  })
  
  observe({
    updateNumericInput(session, "num", value = input$slide)
  })
}

shinyApp(ui, server)