
#cycloneockhi


#install.packages('ROAuth')
#install.packages('RCurl')
#install.packages('twitteR')
# package twitteR
library("twitteR")
library(ROAuth)
library(RCurl)
# all this info is obtained for the Twitter developer account


###  Authentication


# "keys and access token" tab in your developers account


key <- "f22U8M4D1wx1OULA6K5RtXWG4"


secret <- "rukqNHe6XIT3Ixi9Fwai68UmtaykHrqPz48KPuI6TctQNO0BKC"


secrettk <- "NU6Xw68shQZUmTiRdDpl4jOLrkA9X8Du6PTsB55BnESS0"


mytoken <- "834784139819757569-CfKppPVRLfBKyEcOFI0NRRRfbDxFzcI"


# we need these 2 packages
library("twitteR")
library("httr")


# we are using the setup_twitter_oauth function

setup_twitter_oauth(key, secret, mytoken, secrettk)
# (1) choose direct authentication

# Twitter scraping

# Lets check the latest tweets of cycloneockhi using searchtwitter package

# arguments: since and until are for time specifications
# lang: for languge specification
# geocode: for location specification

# we are now scraping 1k tweekts for cycloneockhi, and we als specify our certificate
cycloneockhitweets = searchTwitter("#CycloneOckhi",lang = 'en', n=5000)
#storing tweets analysed to a text file
capture.output(cycloneockhitweets,file ='cycloneockhitweets.txt')

# check the tweets different parameters
class(cycloneockhitweets)
length(cycloneockhitweets)
head(cycloneockhitweets)

library("tm")

cycloneockhilist <- sapply(cycloneockhitweets, function(x) x$getText()) # initiating a function
# encoding the string to utf-8
cycloneockhilist <- lapply(cycloneockhilist, function(x) iconv(x, "latin1", "ASCII", sub=""))

cycloneockhicorpus <- Corpus(VectorSource(cycloneockhilist)) # use the corpus function
# a corpus is the text body consisting of all the text including the meta info


cycloneockhicorpus <- tm_map(cycloneockhicorpus, tolower) # putting text to lower case

cycloneockhicorpus <- tm_map(cycloneockhicorpus, removePunctuation) # remove punctuation

cycloneockhicorpus <- tm_map(cycloneockhicorpus,
                            function(x)removeWords(x,stopwords())) # remove stopwords (meaningless words)



#install.packages('wordcloud')
library("wordcloud")

wordcloud(cycloneockhicorpus, min.freq=4, scale=c(5,1), 
          random.color=F, max.word=100, random.order=T)

# changing to a tdm
cycloneockhitdm <- TermDocumentMatrix(cycloneockhicorpus)

# a DocumentTermMatrix structures the text in a matrix where each term is organized in a column
# each row is a document and the number represents the counts of that term

cycloneockhitdm

# frequent terms
frequenttemrs<-findFreqTerms(cycloneockhitdm, lowfreq=11)


# Lets get a dendrogram to see related terms

# Remove sparse (infrequently used) terms from the term-document matrix
cycloneockhi2tdm <-removeSparseTerms(cycloneockhitdm, sparse=0.9)

# Lets scale the data
cycloneockhi2tdmscale <- scale(cycloneockhi2tdm)

# distance matrix
cycloneockhidist <- dist(cycloneockhi2tdmscale, method = "euclidean")

# hierarchical clustering
cycloneockhifit <- hclust(cycloneockhidist)

# Visualize the result
plot(cycloneockhifit)

# to calculate a certain number of groups
cutree(cycloneockhifit, k=2)

# we can even color the 6 groups and plot them
rect.hclust(cycloneockhifit, k=6, border="purple")




#################################################

# SENTIMENT ANALYSIS

# Sentiment Lexicon: a list of words which you are using to compare your scraped txt with

# list of pos and negative words - manually created - approx. 6800 
# import positive and negative words
pos = readLines("positive-words.txt")
neg = readLines("negative-words.txt")

# function score.sentiment 
score.sentiment = function(cycloneockhilist, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation - using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

testsentiment = score.sentiment(cycloneockhilist, pos, neg)

class (testsentiment)
testsentiment$score

library("stringr")
library("plyr")

# add variables to data frame

testsentiment$very.pos = as.numeric(testsentiment$score >= 2)
testsentiment$very.neg = as.numeric(testsentiment$score <= -2)
testsentiment
# how many very positives and very negatives
positive = sum(testsentiment$very.pos)
negative = sum(testsentiment$very.neg)

                                                            #getting location of tweets
# #install.packages('dismo')
#install.packages('ggmap')
library(dismo)
library(ggmap)
tweetFrame <- twListToDF(cycloneockhitweets)  # Convert to a nice dF
userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF
locatedUsers <- !is.na(userFrame$location)  # Keep only users with location info
locations <- geocode(userFrame$location[locatedUsers])  # approximate lat/lon from textual location data.
#using with function to extract lattitude and longitude
with(locations, plot(lon, lat))
worldMap <- map_data("world")  # Easiest way to grab a world map shapefile
#Pottting with ggplot 
twitloc<- ggplot(worldMap)
twitloc <- twitloc + geom_path(aes(x = lon, y = lat, group = group),  # Draw map
                               colour = gray(2/3), lwd = 1/3)
twitloc <- twitloc + geom_point(data = locations,  # Add points indicating users
                                aes(x = lon, y = lat),
                                colour = "RED", alpha = 1/2, size = 1)
twitloc <- twitloc + coord_equal()  # Better projections are left for a future post
twitloc <- twitloc + theme_minimal()  # Drop background annotations
print(twitloc)

#COUNT OF RETWEETS and favourite counts

nretweet=sum(tweetFrame$retweetCount)
nfavourite=sum(tweetFrame$favoriteCount)
