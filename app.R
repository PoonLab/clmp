library(shiny)
library(clmp)



ui <- fluidPage(
  titlePanel("clmp: clustering with Markov-modulated Poisson processes"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        inputId="newick", 
        label="Input tree",
        value=""
        )
    ),
    mainPanel(
      plotOutput(outputId="ggtree")
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)