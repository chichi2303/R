library(googleVis)
library("twitteR")
library("ROAuth")
library(tm); library(SnowballC)
library(wordcloud)

setup_twitter_oauth(t.api.key, t.api.secret, access_token=NULL,
                    access_secret=NULL) # version 1.1.8

# To Test
start<-getUser("cnnbrk") # Users
start$description

#6 stocks, 3 largest gainer(loser) stocks for the day. 
#Gainer 
NAV <- searchTwitter('$NAV', n = 50)
WINS <- searchTwitter('$WINS', n = 50)
PTLA <- searchTwitter('$PTLA', n = 50)
gainers <- c(NAV, WINS, PTLA )
  
#loser
UBIA <- searchTwitter('$UBIA', n = 50)
FDS <- searchTwitter('$FDS', n = 50)
SLCA <- searchTwitter('$SLCA', n = 50)
losers <- c(UBIA, FDS, SLCA )

#Create separate (two) data corpora for the above two sets of tweets.
getCorpus <- function (tweets) {
  tweets.text <- lapply(tweets, function(t) {t$getText()})
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
  return (data.corpus)
}
gainers.corpus <- getCorpus(gainers)
losers.corpus <- getCorpus(losers)

#Data pre-processing
removeURL <- function(x) {
  gsub("(http[^ ]*)", "", x) # Remove the URLs from the tweets
}
removeNumberWords <- function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])*", "", x)
}
getTransCorpus <- function (data.corpus) {
  data.corpus <- tm_map(data.corpus, content_transformer(removeURL))
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  english.stopwords <- stopwords("en")
  data.corpus <- tm_map(data.corpus,content_transformer(removeWords),english.stopwords)
  data.corpus <- tm_map(data.corpus, content_transformer(removeNumberWords))
  data.corpus <- tm_map(data.corpus,content_transformer(stemDocument))
  data.corpus <- tm_map(data.corpus,content_transformer(stripWhitespace))
  return (data.corpus)
}
gainers.Trans.corpus <- getTransCorpus(gainers.corpus)
losers.Trans.corpus2 <- getTransCorpus(losers.corpus)

#Create the term-document matrix for each groups
gainers.tdm <- TermDocumentMatrix(gainers.Trans.corpus) 
losers.tdm <- TermDocumentMatrix(losers.Trans.corpus2) 


#Compare the frequent terms from each group
FFT.gainers <- findFreqTerms(gainers.tdm, lowfreq=3)
FFT.losers <- findFreqTerms(losers.tdm, lowfreq=3) 
wordFreq.gainers <- rowSums(as.matrix(gainers.tdm))
wordFreq.losers <- rowSums(as.matrix(losers.tdm ))
FFT.gainers
FFT.losers

#Show word cloud for each group
par(mfrow=c(1,2))
palette <- brewer.pal(8,"Dark2")
set.seed(137)
wordcloud(words=names(wordFreq.gainers), freq=wordFreq.gainers, min.freq=3, 
          random.order=F,colors=palette)
text(x=0.5, y=1.05, "Gainers")
wordcloud(words=names(wordFreq.losers), freq=wordFreq.losers, min.freq=3, 
          random.order=F,colors=palette)
text(x=0.5, y=1.05, "Losers")

#Using the positive and negative word lists, compute the sentiment score 
sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p == 0 & n == 0)
    return(NA)
  else
    return(p-n)
}

# Lexicons
pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')

head(pos.words)
head(neg.words)

texts.gainers <- 
  lapply(gainers, 
         function(t) {
           iconv(t$getText(), 
                 "latin1", "ASCII", sub="")
         })
texts.losers <- 
  lapply(losers, 
         function(t) {
           iconv(t$getText(), 
                 "latin1", "ASCII", sub="")
         })

scores.gainers <-sapply(texts.gainers, sentiment, pos.words, neg.words)
scores.losers <-sapply(texts.losers, sentiment, pos.words, neg.words)

#create table for each group

table.gainers <-table(scores.gainers)
table.losers <-table(scores.losers)

#draw bar plot for each group
#Gainers
barplot(table.gainers, xlab="Gainers", ylab="Count", main ="Scores For Gainers' Tweets",
        col=rainbow(5),ylim=c(0,40))
#Losers
barplot(table.losers, xlab="Losers", ylab="Count", main ="Scores For Losers' Tweets",
        col=rainbow(5),ylim=c(0,40))

#Google vis
table.gainers <- as.data.frame(table.gainers)
table.losers <- as.data.frame(table.losers)
chart1 <- gvisPieChart(table.gainers, options = list(title="Gainers's Stock"))
chart2 <- gvisPieChart(table.losers, options = list(title="Losers's Stock"))
chart.merge <- gvisMerge(chart1, chart2, horizontal = TRUE)
plot(chart.merge)

stocks.data <- data.frame(
  Stocks <- c('WINS', 'UBIA'),
  Open <- c(240, 87.40),
  Close <- c(218.00, 87.00),
  High <- c(325, 115.00),
  Low <- c(46,4)
)

colnames(stocks.data) <- c('Stocks', 'Open', 'Close', 'High', 'Low')
stocks.data
chart3 <- gvisBarChart(stocks.data, options = list(legend = 'Top'))
plot(chart3)
