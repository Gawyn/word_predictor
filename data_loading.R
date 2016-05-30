library(gtools)
library(stringr)

filenames <- mixedsort(list.files("data/frequency_sets", pattern = "*.csv", full.names = TRUE))
ngrams <- list()

for (i in 1:length(filenames)){
  df <- read.csv(filenames[i])
  ngrams[[i]] <- df
}

for (i in 1:length(ngrams)){
  ngram <- ngrams[[i]]
  ngram$prediction <- word(as.character(ngram$word), -1)
  ngram$parameter <- gsub("\\s*\\w*$", "", as.character(ngram$word))
  ngrams[[i]] <- ngram
}