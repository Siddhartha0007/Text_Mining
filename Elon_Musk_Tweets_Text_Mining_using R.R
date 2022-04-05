
#Text mining and Emotion classification of Elon Musk data

# Load the Libreries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("textstem")
print('Done Importing Library')

#load dataset

data <- read.csv ('R_dataset/Elon_musk.csv')
# Load the data as a corpus

TextDoc  <- Corpus(VectorSource(data$Text))

length(data)
colnames(data)
head(data)

#Text Preprocessing


TextDoc<- tm_map(TextDoc, content_transformer(tolower))

TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
#TextDoc <- tm_map(TextDoc, stripWhitespace)

inspect(head(TextDoc))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "http[[:alnum:][:punct:]]*")
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)


#TextDoc <- tm_map(TextDoc, stem_words)
TextDoc <- tm_map(TextDoc, stemCompletion)

inspect(head(TextDoc))


# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)

dtm_v <- sort(rowSums(dtm_m))
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

head(dtm_d)

library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 


# This is the barchart representation of the Top 5 Words most appeared
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col =coul, main =" 5 most frequent words",
        ylab = "Word frequencies")




#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


# Find associations 
findAssocs(TextDoc_dtm, terms = c("book","stori","read"), corlimit = 0.25)



# Regular Sentiment score using get_sentiment() function


syuzhet_vector <- get_sentiment(data$Text, method="syuzhet")

head(syuzhet_vector)
# see summary 
summary(syuzhet_vector)


# bing method
bing_vector <- get_sentiment(data$Text, method="bing")
head(bing_vector)
summary(bing_vector)


#affin method
afinn_vector <- get_sentiment(data$Text, method="afinn")
head(afinn_vector)
summary(afinn_vector)


#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)


# Emotion Classification

#Emotion classification is built on the NRC Word-Emotion Association Lexicon (aka EmoLex).
#The NRC Emotion Lexicon is a list of English words and their associations with
#eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust)
#and two sentiments (negative and positive).



d<-get_nrc_sentiment(data$Text)

head (d,10)




td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Observation

#This bar chart demonstrates that words associated with the various  emotions with their counts

library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 


#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE,col=coul, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)
#Observation

#This bar chart demonstrates that words associated with the various  emotions with Percentage
