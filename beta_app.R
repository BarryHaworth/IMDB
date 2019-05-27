# Shiny Dashboard to illustrate beta distribution

library(rmutil)
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("The Beta Distribution",windowTitle = "Beta Distribution"),
  sliderInput("a",
              label="parameter A",
              min=0,
              max=10,
              step=0.01,
              value=1
  ),
  sliderInput("b",
              label="parameter B",
              min=0,
              max=10,
              step=0.01,
              value=1
  ),
  mainPanel(
    plotOutput(outputId = "betaplot",
               width ="600px",
               height="600px")
)
)

server <- function(input, output) {
  betadist <- reactive({
    x <- seq(0,1,by=0.01)
    beta <- dbeta(x,input$a,input$b)
    return(data.frame(x,beta))
    })
  betamean <- reactive({
    mean <- input$a/(input$a+input$b)
    return(mean)
  })
  betaplot <- reactive({
    betaplot <- ggplot(data=betadist(),
                       aes(x=x, y=beta))+
      geom_vline(mapping=aes(xintercept=betamean(),colour="blue"))+
      geom_line()+
      ggtitle(paste("Beta Distribution with a=",input$a,"b=",input$b))
    return(betaplot)
  })
  output$betaplot <- renderPlot(betaplot())
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)  