# Shiny Dashboard to illustrate beta binomial distribution

library(rmutil)
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("The Beta Binomial Distribution",windowTitle = "Beta Distribution"),
  sliderInput("m",
              label="Success Probability",
              min=0,
              max=1,
              step=0.01,
              value=0.5
  ),
  sliderInput("s",
              label="Overdispersion Parameter",
              min=0.01,
              max=50,
              step=0.01,
              value=2
  ),
  mainPanel(
    plotOutput(outputId = "betaplot",
               width ="600px",
               height="400px")
)
)

server <- function(input, output) {
  betadist <- reactive({
    x <- seq(0,9,by=1)
    beta <- dbetabinom(x,9,input$m,input$s)
    return(data.frame(x,beta))
    })
  betaplot <- reactive({
    betaplot <- ggplot(data=betadist(),
                       aes(x=x, y=beta))+
      geom_vline(mapping=aes(xintercept=9*input$m,colour="blue"))+
      geom_bar(stat="identity",fill="gray")+
      ggtitle(paste("Beta Binomial Distribution with m=",input$m,"s=",input$s))
    return(betaplot)
  })
  output$betaplot <- renderPlot(betaplot())
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)  