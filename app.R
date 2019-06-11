library(shiny)
library(clmp)



ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        'textarea{font-family: monospace; font-size: 9px;}'
        )
      )
    ),
  
  titlePanel("clmp: clustering with Markov-modulated Poisson processes"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Do not submit trees labeled with any potentially 
        identifying information."),
      textAreaInput(
        inputId="newick", 
        label="Input tree (Newick format)",
        height='400px',
        value=write.tree(structSIR)
        ),
      actionButton(inputId="actButton", label="Submit"),
      br(),
      h5("If you use clmp in your work, please cite:"),
      helpText("McCloskey RM, Poon AF. A model-based clustering method to detect 
        infectious disease transmission outbreaks from sequence variation. 
        PLoS Comput Biol. 2017 Nov 13;13(11):e1005868.")
    ),
    
    mainPanel(
      plotOutput(outputId="clmpPlot"),
      h5("Log-likelihood of 1-class model"),
      textOutput("loglik1"),
      h5("Log-likelihood of 2-class model"),
      textOutput("loglik2")
    )
  )
)

server <- function(input, output, session) {
  #output$treelength <- renderText({
  #  phy <- read.tree(text=input$newick)
  #  phy$Nnode
  #})
  observeEvent(input$actButton, {
    phy <- read.tree(text=input$newick)
    output$clmpPlot <- renderPlot({
      ggtree(phy)
    })
    
    output$loglik1 <- renderText({
      res1 <- clmp(phy, nrates=1)
      res1$loglik
    })
    
    output$loglik2 <- renderText({
      res2 <- clmp(phy, nrates=2)
      res2$loglik
    })
  })
}

shinyApp(ui, server)