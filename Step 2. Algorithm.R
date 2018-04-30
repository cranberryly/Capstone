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



#Keep single words occuring more than 2 times.
K1words<-data.frame(wordSort_1[wordSort_1>2])
K1words<-mutate(K1words,res=names(wordSort_1[wordSort_1>2]))
names(K1words)[1]<-"Num"
K1words$Prob <- K1words$Num/sum(K1words$Num)
head(K1words)



#Keep BiGrams occuring more than x2 times.
x2<-1
K2words<-data.frame(wordSort_2[wordSort_2>x2])
K2words<-mutate(K2words,BiGram=names(wordSort_2[wordSort_2>x2]))
names(K2words)[1]<-"Num"

tempPreds<-strsplit(K2words$BiGram," ")
part1<-unlist(lapply(tempPreds, function(l) l[[1]]))
part2<-unlist(lapply(tempPreds, function(l) l[[2]]))
K2words<-mutate(K2words,pred=part1,res=part2)
K2words <- K2words %>% group_by(pred) %>% mutate(Prob = Num/sum(Num))
head(K2words)

write.csv(K2words , "K2words.csv")


#Keep TriGrams occuring more than x3 times.
x3<-1
K3words<-data.frame(wordSort_3[wordSort_3>x3])
K3words<-mutate(K3words,TriGram=names(wordSort_3[wordSort_3>x3]))
names(K3words)[1]<-"Num"

tempPreds<-strsplit(K3words$TriGram," ")
part1<-unlist(lapply(tempPreds, function(l) l[[1]]))
part2<-unlist(lapply(tempPreds, function(l) l[[2]]))
part3<-unlist(lapply(tempPreds, function(l) l[[3]]))
part12<-paste(part1,part2)
K3words<-mutate(K3words,pred=part12, res=part3)
K3words <- K3words %>% group_by(pred) %>% mutate(Prob = Num/sum(Num))
tail(K3words)


#Keep FourGrams occuring more than x4 times.
x4<-1
K4words<-data.frame(wordSort_4[wordSort_4>x4])
K4words<-mutate(K4words,FourGram=names(wordSort_4[wordSort_4>x4]))
names(K4words)[1]<-"Num"

tempPreds<-strsplit(K4words$FourGram," ")
part1<-unlist(lapply(tempPreds, function(l) l[[1]]))
part2<-unlist(lapply(tempPreds, function(l) l[[2]]))
part3<-unlist(lapply(tempPreds, function(l) l[[3]]))
part4<-unlist(lapply(tempPreds, function(l) l[[4]]))
part123<-paste(part1,part2,part3)
K4words<-mutate(K4words,pred=part123, res=part4)
K4words <- K4words %>% group_by(pred) %>% mutate(Prob = Num/sum(Num))
head(K4words)


savelist<-list(K1words,K2words,K3words,K4words)
saveRDS(savelist, "listwords.rds")
saveRDS(K1words, "K1words.rds")
saveRDS(K2words, "K2words.rds")
saveRDS(K3words, "K3words.rds")
saveRDS(K4words, "K4words.rds")



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

