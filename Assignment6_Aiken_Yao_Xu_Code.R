#Packages----
library(twitteR)
library(ROAuth)
library(base64enc)
library(httr)
library(httpuv)
library(curl)
library(devtools)
library(bit)
library(RTextTools)
library(topicmodels)
library(RCurl) 
library(tm)
library(RTextTools)
library(SnowballC)
library(ggmap)
library(ggplot2)
library(rworldmap)
library(mapproj)
library(maps)
library(sp)
library(sf)
library(wordcloud)

#Twitter Set up----
ConsumerKey <- ""
ConsumerSecret <- ""
AccessToken <- ""
AccessSecret <- ""
twitteR::setup_twitter_oauth(ConsumerKey,ConsumerSecret,AccessToken,AccessSecret)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

#Download Tweets----
# Get Tweets
PhillyVM <- searchTwitter("vaccine mandate philly OR vaccine mandate philadelphia", lang="en", n=10000)

# Make data frame
PVM_df <- do.call("rbind", lapply(PhillyVM, as.data.frame))

#Clean Tweets----
# Run Corpus
myCorpus <- Corpus(VectorSource(PVM_df$text));

# Remove special characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
myCorpus <- tm_map(myCorpus, toSpace, "@")
myCorpus <- tm_map(myCorpus, toSpace, "/")
myCorpus <- tm_map(myCorpus, toSpace, "]")
myCorpus <- tm_map(myCorpus, toSpace, "$")

# Remove Punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

# Remove non-ascii characters
myCorpus <- tm_map(myCorpus, function(x) iconv(x, "latin1", "ASCII", sub=""))

# Remove English stop words
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))

# Remove our own stop words
myCorpus <- tm_map(myCorpus, removeWords,c("ggkkp1vcj2", "yylxtnd3am", "gregprice11", "jclarknbc", "Philadelphia", "Philly", "By", "the", "may", "will", "ttp", "qhb...", "https", "http...", "for", "tco"))

# Look at first Corpus result
strwrap(myCorpus[[1]])

# Remove whitespace characters
myCorpus <- tm_map(myCorpus, stripWhitespace)

#Visualizations----
#Creating a Document Term Matrix (DTM)
dtm <- DocumentTermMatrix(myCorpus)
dim(dtm)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)

# Make wordcloud
wordcloud(names(freq), freq, min.freq=50, colors=brewer.pal(6, "Dark2"))

# Remove the columns replyToSID, replyToUID and replyToSN from data frame df
PVM_df$replyToSID <- NULL
PVM_df$replyToUID <- NULL
PVM_df$replyToSN <- NULL
head(PVM_df)
dim(PVM_df)

sum(is.na(PVM_df$longitude))

# How many are missing coordinates?
PVM_df.geocoded=na.omit(PVM_df)
dim(PVM_df.geocoded)

#Mean coordinate?
mean(PVM_df.geocoded$latitude)
mean(PVM_df.geocoded$longitude)

write.csv(PVM_df.geocoded, file = "PVM_df_geocoded.csv")
geocoded.points <- read.csv("PVM_df_geocoded.csv")

# Plot tweets on a world map!
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") +
  geom_point(data=geocoded.points, aes(x=longitude, y=latitude), inherit.aes=FALSE)