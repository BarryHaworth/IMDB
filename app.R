# Shiny Dashboard for Beta/Binary model
# Based on code from Datacamp Shiny web app course
# Created repository on Git.  Make a minor change to test commitment

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(tools)
library(stringr)
library(data.table)
library(rmutil)

options(shiny.reactlog = TRUE)

load("./Data/Vote.model.RData")
load("./Data/metascores.RData")
load("./Data/basics.RData")

# Create Beta percentages for parameters a & b
beta_pct <- function(a,b){
  beta_pct=rep(0,10)
  beta_pct[1] = pbeta(0.1,a,b)
  for( i in 2:10) beta_pct[i]=pbeta(i/10,a,b)-pbeta((i-1)/10,a,b)
  return(beta_pct)
}

# Combine data for the dashboard
# Latest model info for each movie
model <- vote.model %>% 
         group_by(tconst) %>% 
         filter(Date == max(Date))

# Latest metascore info for each movie
meta <- metascores %>% 
  group_by(tconst) %>% 
  filter(Date == max(Date))

#combine model with movie details
model <- left_join(model,
                   basics[,!(names(basics) %in% c("originalTitle","endYear"))],
                   by="tconst")

# combine model with metascore
model <- left_join(model,
                   meta[,c("tconst","metascore")],
                   by="tconst")

model           <- as.data.frame(model)
model$isAdult   <- as.factor(model$isAdult)
model$titleType <- as.factor(model$titleType)
model$startYear <- as.integer(model$startYear)

# generate genre data frame for filtering
genre <- model %>%
  mutate(genre = strsplit(genres,",")) %>%
  unnest(genre) %>%
  select(c("tconst","genre"))

genre$genre <- as.factor(genre$genre)

# Define UI for application that plots features of movies 
ui <- fluidPage(
  titlePanel("The IMDB Beta/Binomial model",windowTitle = "IMDB Model"),
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("All Movies",
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      width = 3,
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("IMDB Mean"= "mean", 
                              "Median Score"="median", 
                              "Beta Mean" =  "beta.mean", 
                              "Binary Mean" =  "binary.mean", 
                             "Polarity"=  "polarity",
                             "Start year" = "startYear",
                             "Number of Votes" = "Vote_sum",
                            "Run Time (minutes)" =  "runtimeMinutes",
                            "Metacritic Score"=  "metascore"), 
                  selected = "mean"),
      # Y Axis scalar or logarithmic
      selectInput(inputId = "ylog", 
                  label = "Y Scale:",
                  choices = c("Linear"= "1", 
                              "Logarithmic"="2"),
                  selected = "1"
                  ),
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("IMDB Mean"= "mean", 
                              "Median Score"="median", 
                              "Beta Mean" =  "beta.mean", 
                              "Binary Mean" =  "binary.mean", 
                              "Polarity"=  "polarity",
                              "Number of Votes" = "Vote_sum",
                              "Start year" = "startYear",
                              "Run Time (minutes)" =  "runtimeMinutes",
                              "Metacritic Score"=  "metascore"), 
                  selected = "beta.mean"),
      # X Axis scalar or logarithmic
      selectInput(inputId = "xlog", 
                  label = "X Scale:",
                  choices = c("Linear"= "1", 
                              "Logarithmic"="2"),
                  selected = "1"
      ),
      # Select input for colour
      selectInput(inputId = "z",
                  label = "Colour by:",
                  choices = c("Is Adult"= "isAdult",
                            "Release Year"=  "startYear",
                            "Number of Votes" = "Vote_sum",
                            "Type"=  "titleType",
                            "Genre" = "genres",
                            "Polarity" = "polarity"),
                  selected ="titleType"),
      selectizeInput("genre",
                     label="Genre Filter",
                     sort(unique(as.character(genre$genre))),
                     selected=NULL,multiple=TRUE
      ),
      selectizeInput("titletype",
                     label="Media Type",
                     sort(unique(as.character(model$titleType))),
                     selected=NULL,multiple=TRUE
                     ),
      sliderInput("yearrange",
                  label="Year Range",
                  min=1900,
                  max=2018,
                  value=c(1900,2018)
                  ),
      sliderInput("voterange",
                  label="Vote Range",
                  min=1000,
                  max=2000000,
                  value=c(1000,2000000)
                  ),
      sliderInput("polarity",
                  label="Polarity Range",
                  min=0,
                  max=1,
                  value=c(0,1)
                  )
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot",
                 brush = "plot_brush",
                 hover = hoverOpts("plot_hover",delay=100,delayType="debounce"),
                 uiOutput("hover_info"),
                 width ="600px",
                 height="600px"),
      uiOutput("hover_info"),
      br(),                 # Single line break for a little bit of visual separation
      h5(textOutput("description")), # Fifth level header: Description
      # Show the Data Table
      h4("Brushed Selection"), # Fifth level header: Description
      DT::dataTableOutput(outputId ="brushedtable"),

      h4("Full Data Table"), # Fifth level header: Description
      DT::dataTableOutput(outputId ="moviestable")
      )
  )
),
tabPanel("Single Movie",
         mainPanel(
           selectizeInput("movie.id",
                        label="Movie ID",
                        sort(unique(as.character(model$tconst))),
                        selected="tt4154664",multiple=FALSE),
           plotOutput(outputId = "singleplot",width ="600px",height="400px"),
           fluidRow(
             splitLayout(cellWidths = c("33%", "33%","33%"), 
                         plotOutput(outputId = "betaplot",width ="200px",height="200px"),
                         plotOutput(outputId = "binaryplot",width ="200px",height="200px"),
                         plotOutput(outputId = "residualplot",width ="200px",height="200px"))
             ),
           DT::dataTableOutput(outputId ="ratingcounts"),
           plotOutput(outputId = "votetime",width ="600px",height="600px"),
           plotOutput(outputId = "meantime",width ="600px",height="600px"),
           DT::dataTableOutput(outputId ="latestvotes"),
           DT::dataTableOutput(outputId = "allvotes")
         )
)
)
)
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # filtered data frame of movies 
  movies <- reactive({
    movies <- model %>%
      filter(startYear >= input$yearrange[1] ) %>%
      filter(startYear <= input$yearrange[2] ) %>%
      filter(Vote_sum  >= input$voterange[1] ) %>%
      filter(Vote_sum  <= input$voterange[2] ) %>%
      filter(polarity  >= input$polarity[1] ) %>%
      filter(polarity  <= input$polarity[2] ) 
    if (is.null(input$genre) == FALSE){
      genre_id <- genre %>% filter(genre %in% input$genre)
      genre_id <- unique(genre_id$tconst)
      movies <- movies %>% filter(tconst %in% genre_id)
    }
    if (is.null(input$titletype) == FALSE){
      movies <- movies %>% filter(titleType %in% input$titletype)
        }
    return(movies)
  })

  # movie name and year for the selected movie
  title.basics <- reactive({
    title.basics <- basics %>%
      filter(tconst == input$movie.id) %>%
       select(c('tconst','primaryTitle','startYear')) 
    return(title.basics)
  })
  
  movie.name <- reactive(title.basics()$primaryTitle)
  
  # Votes over time for the selected movie
  title.votes <- reactive({
    title.votes <- vote.model %>%
      filter(tconst == input$movie.id) 
    title.votes <- left_join(title.basics(),title.votes,by="tconst")
    return(title.votes)
  })
  # Table of votes over time for the selected movie
  output$allvotes <- DT::renderDataTable(title.votes())
  
  # Latest votes for the selected movie
  title.latest <- reactive({
    title.latest <- title.votes() %>%
      filter(Date == max(title.votes()$Date))
    return(title.latest)
  })
  # Table of latest votes for the selected movie.
  output$latestvotes <- DT::renderDataTable(title.latest())
  
  # Means for Labelling Rating plot
  title.means <- reactive({
    title.means <- data.frame(rating=c(title.latest()$mean,
                                       title.latest()$median,
                                       title.latest()$beta.mean,
                                       title.latest()$binary.ppn*10),
                              label=c("Mean","Median","Beta Mean","Binary Mean"))
    return(title.means)
  })
  
  # Rating counts for Rating Plot
  rating.counts <- reactive({
    title.m        <- title.latest()$m
    title.s        <- title.latest()$s
    title.scale    <- title.latest()$scale
    title.polarity <- title.latest()$polarity
    title.binary   <- title.latest()$binary.ppn
    title.Vote_sum <- title.latest()$Vote_sum
    
    single.votes <- melt(title.latest()[c("tconst",
                                        "Vote_01","Vote_02","Vote_03","Vote_04","Vote_05",
                                        "Vote_06","Vote_07","Vote_08","Vote_09","Vote_10")],
                         id=("tconst"))
    single.votes$rating <- as.numeric(substr(single.votes$variable,6,7))
    single.votes        <- single.votes %>% select(-'variable') 
    
    single.votes$votes <- single.votes$value
    single.votes       <- single.votes %>% select(-'value') 
    
    # single.votes$beta        <- beta_pct(title.a,title.b)*title.Vote_sum*title.binary
    single.votes$beta        <- title.Vote_sum*title.scale * dbetabinom(0:9,9,title.m,title.s)
    single.votes$beta        <- round(single.votes$beta,digits=1)
    single.votes$residual    <- single.votes$votes - single.votes$beta 
    single.votes$residual[1] <- 0
    single.votes$residual[10] <- 0
    
    single.votes$binary      <- 0
    single.votes$binary[1]   <- single.votes$votes[1] - single.votes$beta[1]
    single.votes$binary[10]  <- single.votes$votes[10] - single.votes$beta[10]
    single.votes$binary      <- round(single.votes$binary,digits=1)
    return(single.votes)
  })
  # Table of latest votes for the selected movie.
  output$ratingcounts <- DT::renderDataTable({rating.counts() })
  
  # Create the Rating Plot
  rating.plot <- reactive({
    rating.plot <- ggplot(data=rating.counts(),aes(x=rating,y=votes))+
      geom_bar(stat="identity",fill="red")+
      scale_x_discrete(name="Rating",limits=c(1:10))+
      geom_vline(data=title.means(), mapping=aes(xintercept=rating), color="blue") +
      geom_text(data=title.means(), mapping=aes(x=rating, y=0, label=label), size=4, angle=90, vjust=0, hjust=-2) +
      ggtitle(paste("Votes for",movie.name()))
    return(rating.plot)
  })
  output$singleplot <- renderPlot({rating.plot()})
  
  # Create the Beta plot
  beta.plot <- reactive({
    beta.plot <- ggplot(data=rating.counts(),aes(x=rating,y=beta))+
      geom_bar(stat="identity",fill="green")+
      scale_x_discrete(name="Rating",limits=c(1:10))+
      ggtitle(paste("Beta Model for",movie.name()))
    return(beta.plot)
  })
  output$betaplot <- renderPlot({beta.plot()})
  
  # Create the Binary Plot
  binary.plot <- reactive({
    binary.plot <- ggplot(data=rating.counts(),aes(x=rating,y=binary))+
      geom_bar(stat="identity",fill="orange")+
      scale_x_discrete(name="Rating",limits=c(1:10))+
      ggtitle(paste("Binary Model for",movie.name()))
    return(binary.plot)
  })
  output$binaryplot <- renderPlot({binary.plot()})
  
  # Create the Residuals plot
  residual.plot <- reactive({
    residual.plot <- ggplot(data=rating.counts(),aes(x=rating,y=residual))+
      geom_bar(stat="identity",fill="gray")+
      scale_x_discrete(name="Rating",limits=c(1:10))+
      ggtitle(paste("Residuals for",movie.name()))
    return(residual.plot)
  })
  output$residualplot <- renderPlot({residual.plot()})
  
  # Plot of number of votes over time for the selected movie
  timevote <- reactive({
    timevote <- ggplot(data=title.votes(),aes_string(x="Date"))+
      geom_line(aes(y=Vote_sum)) +
      geom_point(aes(y=Vote_sum))+
      ylab("Votes")+
      scale_y_continuous(limits=c(0,NA))+
      ggtitle(paste("Number of Votes over time for",movie.name()))
    return(timevote)
  })
  output$votetime <- renderPlot({timevote()})
  # Plot of polarity over time for the selected movie
  
  # Plot of means over time for the selected movie.
  timemean <- reactive({
    timemean <- ggplot(data=title.votes(),aes_string(x="Date"))+
      geom_line(aes(y=mean, color = "Mean"))+
      geom_step(aes(y=median, color="Median"))+
      geom_line(aes(y=beta.mean, color="Beta Mean"))+
      geom_line(aes(y=binary.mean, color="Binary Mean"))+
      geom_line(aes(y=polarity*10, color="Polarity"))+
      theme(legend.position="bottom") +
      ylim(0,10)+
      ylab("Rating")+
      ggtitle(paste("Ratings over time for",movie.name()))
    return(timemean)
  })
  output$meantime <- renderPlot({timemean()})
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })

    scatter <- reactive({
    scatter <- ggplot(data = movies(), aes_string(x = input$x, y = input$y,
                                    color = input$z)) +
      geom_point()
    if (input$xlog=="1") {
      scatter <- scatter + scale_x_continuous()
    } else{
      scatter <- scatter + scale_x_log10()+
        annotation_logticks(sides="b")
    }
    if (input$ylog=="1") {
      scatter <- scatter + scale_y_continuous()
    } else{
      scatter <- scatter + scale_y_log10()+
        annotation_logticks(sides="l")
    }
    return(scatter)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({ scatter()
  })
  # Print Data Table of selected movies
  
  output$moviestable <- DT::renderDataTable({
    DT::datatable(
      movies() %>%
      # brushedPoints(movies(),input$plot_brush,allRows=FALSE)  %>%
        select(tconst,primaryTitle,startYear,Date,Vote_sum,metascore,mean,median,beta.mean,binary.mean,polarity,genres) ) %>%
      formatRound(c("mean","beta.mean","binary.mean","polarity"),4)
  })    
  
  output$brushedtable <- DT::renderDataTable({
    DT::datatable(
      # movies() %>%
      brushedPoints(movies(),input$plot_brush,allRows=FALSE)  %>%
      select(tconst,primaryTitle,startYear,Date,Vote_sum,metascore,mean,median,beta.mean,binary.mean,polarity,genres) ) %>%
      formatRound(c("mean","beta.mean","binary.mean","polarity"),4)
  })
  
  #  Tool Tip
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(movies(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct  <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px  <- hover$range$top  + top_pct * (hover$range$bottom - hover$range$top)

    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> ID: </b>"         , point$tconst, "<br/>",
                    "<b> Title: </b>"      , point$primaryTitle, "<br/>",
                    "<b> Votes: </b>"      , point$Vote_sum, "<br/>",
                    "<b> Metascore: </b>"  , point$metascore, "<br/>",
                    "<b> Mean: </b>"       , format(point$mean,digits=4), "<br/>",
                    "<b> Median: </b>"     , point$median, "<br/>",
                    "<b> Beta Mean: </b>"  , format(point$beta.mean,digits=4), "<br/>",
                    "<b> Polarity: </b>"   , format(point$polarity,digits=4), "<br/>",
                    "<b> Binary Mean: </b>", format(point$binary.mean,digits=4), "<br/>")))
    )
  })
  # Create description of plot
    output$description <- renderText({
      paste("The plot above shows the relationship between",
            x(),
            "and",
            y(),
            "for",
            nrow(movies()),
            "movies.")
    })    
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)