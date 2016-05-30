library("tm")
library("RWeka")
set.seed(42)
options(mc.cores=1)
# setwd("/Users/cplanas/Code/R/final_project")

tweets <- readLines("data/en_us/en_US.twitter.txt", encoding = "UTF-8")
tweets <- sample(tweets, size = 20000)

blogs <- readLines("data/en_us/en_US.blogs.txt", encoding = "UTF-8")
blogs <- sample(blogs, size = 20000)

news <- readLines("data/en_us/en_US.news.txt", encoding = "UTF-8")
news <- sample(news, size = 20000)

text_samples <- c(tweets, blogs, news)
text_samples <- sapply(text_samples, function(x) iconv(enc2utf8(x), sub = "byte"))

tweets <- NULL
blogs <- NULL
news <- NULL

removeMostPunctuation <- function(x){
  gsub("[^[:alnum:][:space:]']", "", x)
}

freq_frame <- function(tdm){
  # Helper function to tabulate frequency
  freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  freq_frame <- data.frame(word=names(freq), freq=freq)
  return(freq_frame)
}

review_source <- VectorSource(text_samples)
corpus <- Corpus(review_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removeMostPunctuation))
corpus <- tm_map(corpus, stripWhitespace)

frequency_set <- list()
for (i in 1:10){
  tokenizer_function <- function(x) NGramTokenizer(x, Weka_control(min = i, max = i, delimiters = ' \r\n\t.,;:"()?!'))
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = tokenizer_function))
  tdm <- removeSparseTerms(tdm, 0.9999)
  frq <- freq_frame(tdm)
  frequency_set[[i]] <- frq
}

for (i in 1:length(frequency_set)){
  filename <- paste("data/frequency_sets/", i, "gram_frequency.csv", sep = "")
  write.csv(frequency_set[[i]], file = filename)
}