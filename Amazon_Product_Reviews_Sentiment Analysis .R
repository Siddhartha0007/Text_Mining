#Text Mining and Sentiment analysis
# Reviews of  The Amazon  Kindle book given by the Users


# Import Libreries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
print('Done Importing Library')

# import dataset

data <- read.csv('../Downloads/preprocessed_kindle_review .csv')
TextDoc  <- Corpus(VectorSource(data$review))

length(data)

colnames(data)

head(data)


#Text Preprocessing

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")




TextDoc <- tm_map(TextDoc, content_transformer(tolower))


TextDoc <- tm_map(TextDoc, removeNumbers)


TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))


TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team"))


TextDoc <- tm_map(TextDoc, removePunctuation)



TextDoc <- tm_map(TextDoc, stripWhitespace)



TextDoc <- tm_map(TextDoc, stemDocument)



#Building the term document matrix



TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)

dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

head(dtm_d)


#  bar plots

barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

# Creating Pie Chart of Top 5 Most word appeared

x <- c(15696, 11329, 10921,6580,6019)
labels <- c("book", "stori", "read","like","one")


pie(x, labels, main = "Pie Chart of Top 5 Most word appeared", col = rainbow(length(x)))
legend("topright", c("book","stori","read","like","one"), cex = 0.8,fill = rainbow(length(x)))


#Generating the Word Cloud


set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


#Word Association


# Find associations 
findAssocs(TextDoc_dtm, terms = c("book","stori","read"), corlimit = 0.25)


#Sentiment Scores


syuzhet_vector <- get_sentiment(data$reviewText, method="syuzhet")

head(syuzhet_vector)

summary(syuzhet_vector)


# bing method
bing_vector <- get_sentiment(data$reviewText, method="bing")
head(bing_vector)
summary(bing_vector)


#affin method
afinn_vector <- get_sentiment(data$reviewText, method="afinn")
head(afinn_vector)
summary(afinn_vector)




#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)




#Emotion Classification
#Emotion classification is built on the NRC Word-Emotion Association Lexicon (aka EmoLex)

d<-get_nrc_sentiment(data$reviewText)


head (d,10)



#transpose
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






#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, col = "blue",
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)


#Conclusion:
# From the plots and analysis emotions of "trust","anticipation","joy" have higher percentage than negetive emotions 
