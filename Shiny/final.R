library(dplyr)

myStopWords <- stopwords(kind = "en")

removeStopWords=FALSE
doStemming=FALSE
cleanSentence <- function(testSentence) {
  if(removeStopWords) {
    testSentence <- removeWords(testSentence,myStopWords)
  }
  testSentence <- stripWhitespace(testSentence)
  testSentence <- tolower(testSentence)
  testSentence <- removeNumbers(testSentence)
  testSentence <- removePunctuation(testSentence, preserve_intra_word_dashes = TRUE)
  if(doStemming) {
    testSentence <- stemDocument(testSentence)
  }
  return(testSentence)
}

getwd()
setwd("C:/Users/i58197/Desktop/")
K1words <- readRDS(con <- file("K1words.rds"))
close(con)
K2words <- readRDS(con <- file("K2words.rds"))
close(con)
K3words <- readRDS(con <- file("K3words.rds"))
close(con)
K4words <- readRDS(con <- file("K4words.rds"))
close(con)


PredNext <- function(myinput,dictionary){
  locs<-grep(myinput,dictionary$pred)
  if(length(locs)==0){
    return(NULL)
  }else{
    result_df <- data.frame(dictionary$res[locs],dictionary$Prob[locs])
    colnames(result_df) <- c('res','Prob')
    return(result_df[order(-result_df$Prob),])
  }
}


NgramPred <- function(myinput){
  cleaned <-cleanSentence(myinput)
  split_in <- unlist(strsplit(cleaned," "))
  indexes <- which(split_in == "")
  if(length(indexes) > 0){
    split_in <- split_in[-indexes]
  }
  len <-length(split_in)
  
  #Extract the cleaned input into predictor Ngrams
  if(len>=3){
    clean_pred1 <- paste("^",paste(split_in[len-2],split_in[len-1],split_in[len]),"$",sep="")
    myRes1 <- PredNext(clean_pred1,K4words)
    clean_pred2 <- paste("^",paste(split_in[len-1],split_in[len]),"$",sep="")
    myRes2 <- PredNext(clean_pred2,K3words)
    myRes2$Prob <- 0.4*myRes2$Prob
    clean_pred3 <- paste("^",split_in[len],"$",sep="")
    myRes3 <- PredNext(clean_pred3,K2words)
    myRes3$Prob <- 0.4*0.4*myRes3$Prob
    myRes <- rbind(myRes1,myRes2,myRes3)
    if(is.null(myRes)){
      
      myRes_ordered <- K1words[order(-K1words$Prob),c('res','Prob')]
    }
    myRes_ordered <- myRes[order(-myRes$Prob),]
    names(myRes_ordered)[1]<-'Prediction'
    names(myRes_ordered)[2]<-'Probability'
    myRes_new <- myRes_ordered %>% distinct(Prediction, .keep_all = TRUE)
    return(myRes_new)
    
  }
  
  if(len==2){
    clean_pred2 <- paste("^",paste(split_in[len-1],split_in[len]),"$",sep="")
    myRes2 <- PredNext(clean_pred2,K3words)
    clean_pred3 <- paste("^",split_in[len],"$",sep="")
    myRes3 <- PredNext(clean_pred3,K2words)
    myRes3$Prob <- 0.4*myRes3$Prob
    myRes <- rbind(myRes2,myRes3)
    if(is.null(myRes)){
      
      myRes_ordered <- K1words[order(-K1words$Prob),c('res','Prob')]
    }
    myRes_ordered <- myRes[order(-myRes$Prob),]
    names(myRes_ordered)[1]<-'Prediction'
    names(myRes_ordered)[2]<-'Probability'
    myRes_new <- myRes_ordered %>% distinct(Prediction, .keep_all = TRUE)
    return(myRes_new)
    return(myRes_ordered)
  }
  
  if(len==1){
    clean_pred3 <- paste("^",split_in[len],"$",sep="")
    myRes3 <- PredNext(clean_pred3,K2words)
    if(is.null(myRes)){
      
      myRes_ordered <- K1words[order(-K1words$Prob),c('res','Prob')]
    }
    myRes_ordered <- myRes3[order(-myRes3$Prob),]
    names(myRes_ordered)[1]<-'Prediction'
    names(myRes_ordered)[2]<-'Probability'
    myRes_new <- myRes_ordered %>% distinct(Prediction, .keep_all = TRUE)
    return(myRes_new)
    return(myRes_ordered)
  }
  
}









