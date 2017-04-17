library(tm)
library(cluster)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
setwd("C:/Users/kkara/Desktop")

data <- read.csv("C:/Users/kkara/Desktop/Big1.csv",header = T,stringsAsFactors = F)

data <- iconv(data,"latin1", "ASCII", sub="")

corp <- Corpus(VectorSource(data))

corp <- tm_map(corp,removePunctuation)



for(j in seq(corp))
{
  corpus[[j]] <- gsub("/", " ", corp[[j]])
  corpus[[j]] <- gsub("@", " ", corp[[j]])
  corpus[[j]] <- gsub("\\|", " ",corp[[j]])
}

corp <- tm_map(corp,removeNumbers)

corp <- tm_map(corp,tolower)

st <- c("rt",stopwords("english"))

corp <- tm_map(corp,removeWords,st)

corp <- tm_map(corp,stemDocument)

# Removing white spaces in the corpus
corp <- tm_map(corp,stripWhitespace)
corp <- tm_map(corp,PlainTextDocument)
dtm <- TermDocumentMatrix(corp)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
#write.csv(d,"freq.csv",row.names = T)

myd<-read.csv("freq.csv",header = T)
myd$word<-NULL
View(myd)
kfit<-kmeans(myd,4)
#set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 50,
         max.words=152, random.order=FALSE, rot.per=0.35, 
         colors=brewer.pal(8, "Dark2"))

