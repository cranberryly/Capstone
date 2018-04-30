library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coursera JHU Data Science Specialization"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    
    sidebarPanel(
      
      
      h4("Capstone Project Data Product"),
      h4("Next Word Predictor"),
      br(),
      h5("Enter your text to see the next word(s) prediction:"),
      textInput(inputId = "userSentence", 
                label = "", 
                value = ""  
      ),
      
      sliderInput("numPredicted", "Number of candidate words you want in prediction (max. 20):", 
                  min=1, max=20, value=4)
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Prediction",
          h4("The predicted word (with highest probability) is:"),
          h3(textOutput("predictedWordMain")),
          br(),
          #h4(textOutput("kText")),
          #hr(),
          #div(dataTableOutput("predictionTable")) 
          tableOutput("predictionTable")
        )
        ,
        tabPanel("Documentation",
                 includeMarkdown("Documentation.Rmd")
        )
      )
    )
  )
))