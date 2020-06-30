install.packages("rtweet")
library("rtweet")
library("twitteR")
library(base64enc)
library(httpuv)
library("ROAuth")
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(devtools)
cred<-OAuthFactory$new(consumerKey='BagGgBbanzbdpPNNp8Uy6TQBP', # Consumer Key (API Key)
                       consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa', #Consumer Secret (API Secret)
                       requestURL='https://api.twitter.com/oauth/request_token',                accessURL='https://api.twitter.com/oauth/access_token',
                       authURL='https://api.twitter.com/oauth/authorize')
save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")


#Access Token Secret

setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", # Consumer Key (API Key)
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", #Consumer Secret (API Secret)
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  # Access Token
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")  #Access Token Secret  #Access Token Secret
Tweets<-userTimeline('narendramodi',n=1000)
tweetdf<-twListToDF(Tweets)
write.csv(tweetdf,'Tweets.csv')
getwd()

Tweet<-read.csv("E://Assignment//text mining//assignment//Tweets.csv")
View(Tweet)
str(Tweet)


# Build Corpus and DTM/TDM

corpus<-Tweet$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Convert the text to lower case
corpus <- tm_map(corpus, tolower)
inspect(corous[1])

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
# Remove punctuations
corpus <- tm_map(corpus, removePunctuation)

# Remove english common stopwords
corpus <- tm_map(corpus, removeWords, stopwords('english'))
# Remove your own stop word
# specify your stopwords as a character vector
corpus <- tm_map(corpus, removeWords, c("our", "we","the","will"))
#striping white spaces
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1])
# Text stemming
corpus<-lemmatize_words(corpus)
corpus <- tm_map(Corpus, stemDocument)

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(corpus, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('the','can'))
cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')
# the barplot pulls both page and pages as separate words. this should be 
# counted as one.

inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5]

 #Term Document Matrix :
# Convert the unstructured data to structured data :

tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 2547 words and 1000 documents(# of tweets) in this TDM
# Sparsity is 100% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]


w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))        

# Word Cloud :
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

# Sentiment Analysis for tweets:


# install.packages("syuzhet")

# Read File

Tweetdata <- read.csv("E://Assignment//text mining//assignment//Tweets.csv", header = TRUE)
tweetsdata <- as.character(Tweetdata$text)
class(tweetsdata)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(tweetsdata)
head(s)
tweetsdata[3]
#The above tweet has value 1 for anger,1 for negative 1 for joy 1 for trust 2 for positive

get_nrc_sentiment('pretending')
# Pretend has one value of negative and one value for anger
get_nrc_sentiment('can learn') #1 for positive

# barplot 
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Narendramodi Tweets')
