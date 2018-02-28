#Loading the data
library(readr)
rangoon = read_csv("rangoontweets.csv")
#loading libraries
library('stringr')
library('readr')
library('wordcloud')
library('tm')
library('SnowballC')
library('RWeka')
library('RSentiment')
library(DT)

#extracting relevant data
r1 = as.character(rangoon$text)
#Data Preprocessing
set.seed(100)
sample = sample(r1, (length(r1)))
corpus = Corpus(VectorSource(list(sample)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, stemDocument)
dtm_up = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_up <- colSums(as.matrix(dtm_up))
#Calculating Sentiments
sentiments_up = calculate_sentiment(names(freq_up))
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]

DT::datatable(sent_pos_up)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(sent_pos_up$text,sent_pos_up$freq,min.freq=10,colors=brewer.pal(6,"Dark2"))


DT::datatable(sent_neg_up)
plot.new()
set.seed(100)
wordcloud(sent_neg_up$text,sent_neg_up$freq, min.freq=10,colors=brewer.pal(6
                                                                           ,"Dark2")

          
          
          #Approach 2 - using the 'syuzhet' package
          text = as.character(rangoon$text) 
          ##removing Retweets
          some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",text)
          ##let's clean html links
          some_txt<-gsub("http[^[:blank:]]+","",some_txt)
          ##let's remove people names
          some_txt<-gsub("@\\w+","",some_txt)
          ##let's remove punctuations
          some_txt<-gsub("[[:punct:]]"," ",some_txt)
          ##let's remove number (alphanumeric)
          some_txt<-gsub("[^[:alnum:]]"," ",some_txt)
          
          
          
          #visual
          library(ggplot2) # Data visualization
          library(syuzhet)
          mysentiment<-get_nrc_sentiment((some_txt))
          # Get the sentiment score for each emotion
          mysentiment.positive =sum(mysentiment$positive)
          mysentiment.anger =sum(mysentiment$anger)
          mysentiment.anticipation =sum(mysentiment$anticipation)
          mysentiment.disgust =sum(mysentiment$disgust)
          mysentiment.fear =sum(mysentiment$fear)
          mysentiment.joy =sum(mysentiment$joy)
          mysentiment.sadness =sum(mysentiment$sadness)
          mysentiment.surprise =sum(mysentiment$surprise)
          mysentiment.trust =sum(mysentiment$trust)
          mysentiment.negative =sum(mysentiment$negative)
          
          
          # Create the bar chart
          yAxis <- c(mysentiment.positive,
                     + mysentiment.anger,
                     + mysentiment.anticipation,
                     + mysentiment.disgust,
                     + mysentiment.fear,
                     + mysentiment.joy,
                     + mysentiment.sadness,
                     + mysentiment.surprise,
                     + mysentiment.trust,
                     + mysentiment.negative)
          
          
          xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
          colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
          yRange <- range(0,yAxis) + 1000
          barplot(yAxis, names.arg = xAxis, 
                  xlab = "Emotional valence", ylab = "Score", main = "Twitter sentiment for Movie Rangoon 2017", sub = "Feb 2017", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")
          colSums(mysentiment)
          
          

cat("We have far lower negative Sentiments: ",sum(sent_neg_up$freq_up)," than positive: ",sum(sent_pos_up$freq_up))