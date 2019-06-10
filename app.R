library(shiny)
library(clmp)



ui <- fluidPage(
  tags$head(tags$style(HTML('textarea{font-family: monospace;}'))),
  titlePanel("clmp: clustering with Markov-modulated Poisson processes"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        inputId="newick", 
        label="Input tree",
        height='400px',
        value=write.tree(structSIR)
        ),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput(outputId="clmpPlot")
    )
  )
)

server <- function(input, output, session) {
  #output$clmpPlot <- renderPlot()
}

shinyApp(ui, server)