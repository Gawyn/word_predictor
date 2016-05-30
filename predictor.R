predict <- function (sentence){
  sentence <- tolower(sentence)
  sentence <- removeMostPunctuation(sentence)
  sentence <- stripWhitespace(sentence)
  
  nwords <- length(strsplit(sentence, " ")[[1]])
  fresults <- NULL

  for (i in 1:min(length(ngrams), nwords)){
    ngram <- ngrams[[i+1]]
    length <- nwords - (i - 1)
    
    checker <- paste(word(sentence, length:nwords), collapse = " ")
    results <- ngram[ngram$parameter == checker,][, c("prediction", "freq")]
    results$points <- results$freq * (i * i)
    results$freq <- NULL
    
    if(is.null(fresults)){
      fresults <- results
    } else{
      fresults <- rbind(fresults, results)
    }
  }
  
  fresults <- aggregate(points ~ prediction, data = fresults, FUN = sum)
  return(fresults[order(-fresults$points),][1:10,]$prediction)
}