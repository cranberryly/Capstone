library(shiny)
library(dplyr)



# Define server logic required for Ngram prediction
shinyServer(function(input, output) {
  
  output$predictedWordMain <- renderText({
    userPred <- NgramPred(input$userSentence)
    as.vector(userPred[1,1])
  })
  
  output$predictionTable <- renderTable({
    userPred <- NgramPred(input$userSentence)
    #   Provide table from prediction algorithm for output.
    userPred <- as.data.frame(userPred)
    k = input$numPredicted
    #userPred$Probability <- round(userPred$Probability,digits=4)
    head(userPred,k)
    
  })
  
  
})
















