                      #MumbaiRains
#install.packages('ROAuth')
#install.packages('RCurl')
# to be able to scrape data from Twitter we need a standard Twitter account
# and we need to update it to a developer account
#install.packages('twitteR')
# package twitteR
library("twitteR")
library(ROAuth)
library(RCurl)


#Authentication

# "keys and access token" tab in your developers account


key <- "f22U8M4D1wx1OULA6K5RtXWG4"


secret <- "rukqNHe6XIT3Ixi9Fwai68UmtaykHrqPz48KPuI6TctQNO0BKC"


secrettk <- "NU6Xw68shQZUmTiRdDpl4jOLrkA9X8Du6PTsB55BnESS0"


mytoken <- "834784139819757569-CfKppPVRLfBKyEcOFI0NRRRfbDxFzcI"


# we need these 2 packages
library("twitteR")
library("httr")


# we are using the setup_twitter_oauth function
?setup_twitter_oauth


# keep this order of arguments
setup_twitter_oauth(key, secret, mytoken, secrettk)
# (1) choose direct authentication


#library("twitteR")

# searchTwitter is the main function of the package

?searchTwitter

# arguments: since and until are for time specifications
# lang: for languge specification
# geocode: for location specification

# we are now scraping 1k tweekts for mumbairains, and we als specify our certificate
mumbairainstweets = searchTwitter("#MumbaiRains",lang='en', n=5000)
# extracting list in a text file
capture.output(mumbairainstweets, file = "MUMBAIRAINSTWEETS.txt")
# as you can see, scraping that data is quite time consuming - your machine limits the
# the efficiency and speed of your mining 
# if you are plan to scrape a lot in the future 64bit systems and high RAM is desireable

class(mumbairainstweets)
length(mumbairainstweets)
head(mumbairainstweets)

library("tm")

mumbairainslist <- sapply(mumbairainstweets, function(x) x$getText()) # initiating a function
# in depth info about the apply family and functions in the course "R Level 1"
mumbairainslist <- lapply(mumbairainslist, function(x) iconv(x, "latin1", "ASCII", sub=""))

mumbairainscorpus <- Corpus(VectorSource(mumbairainslist)) # use the corpus function
# a corpus is the text body consisting of all the text including the meta info


mumbairainscorpus <- tm_map(mumbairainscorpus, tolower) # putting text to lower case
#mumbairainstdm1<- TermDocumentMatrix(mumbairainscorpus)
mumbairainscorpus <- tm_map(mumbairainscorpus, removePunctuation) # remove punct.
#mumbairainstdm2<- TermDocumentMatrix(mumbairainscorpus)
mumbairainscorpus <- tm_map(mumbairainscorpus,
                      function(x)removeWords(x,stopwords())) # remove stopwords (meaningless words)
#mumbairainstdm3<- TermDocumentMatrix(mumbairainscorpus)
# there is a link to a stop word list in the link lecture

# Lets see which other transformations tm offers
?getTransformations

# to trasform to plain text which wordcloud can use
#mumbairainscorpus <- tm_map(mumbairainscorpus, PlainTextDocument)
#mumbairainstdm4<- TermDocumentMatrix(mumbairainscorpus)
#install.packages('wordcloud')
library("wordcloud")

? wordcloud

wordcloud(mumbairainscorpus, min.freq=4, scale=c(10,1), 
          random.color=T, max.word=100, random.order=F,colors='red',ordered.colors = T, vfont=c("sans serif","plain"))

# changing to a tdm
mumbairainstdm <- TermDocumentMatrix(mumbairainscorpus)

# a DocumentTermMatrix structures the text in a matrix where each term is organized in a column
# each row is a document and the number represents the counts of that term

mumbairainstdm

# frequent terms
findFreqTerms(mumbairainstdm, lowfreq=11)

?findFreqTerms

# associations
findAssocs(mumbairainstdm, 'mumbai', 0.60)

# Lets get a dendrogram to see related terms

# Remove sparse (infrequently used) terms from the term-document matrix
mumbairains2tdm <-removeSparseTerms(mumbairainstdm, sparse=0.9)

# Lets scale the data
mumbairains2tdmscale <- scale(mumbairains2tdm)

# distance matrix
mumbairainsdist <- dist(mumbairains2tdmscale, method = "euclidean")

# hierarchical clustering
mumbairainsfit <- hclust(mumbairainsdist)

# Visualize the result
plot(mumbairainsfit)

# to calculate a certain number of groups
cutree(mumbairainsfit, k=3)

# we can even color the 6 groups and plot them
rect.hclust(mumbairainsfit, k=2, border="red")


                        



#################################################

# SENTIMENT ANALYSIS

# Sentiment Lexicon: a list of words which you are using to compare your scraped txt with

# list of pos and negative words - manually created - approx. 6800 
# import positive and negative words
pos = readLines("positive-words.txt")
neg = readLines("negative-words.txt")

# function score.sentiment 
score.sentiment = function(mumbairainslist, pos.words, neg.words, .progress='none')
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

testsentiment = score.sentiment(mumbairainslist, pos, neg)

class (testsentiment)
testsentiment$score

library("stringr")
library("plyr")

# add variables to data frame
testsentiment$very.pos = as.numeric(testsentiment$score >= 2)
testsentiment$very.neg = as.numeric(testsentiment$score <= -2)
# how many very positives and very negatives
positive = sum(testsentiment$very.pos)
negative = sum(testsentiment$very.neg)



                                          #getting location of tweets
# #install.packages('dismo')
#install.packages('ggmap')
library(dismo)
library(ggmap)
tweetFrame <- twListToDF(mumbairainstweets)  # Convert to a nice dF
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




