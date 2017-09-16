# Load
library("NLP")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")

data <- read.csv(file="F:/DA/demonetization.csv", header=TRUE, sep=",")
data <- subset(data,retweetCount>50)
data[6] <- data[6]/10

#writing to a text file for clean up

l <- data$text
i <- 1
 sink("tweets.txt")
 while(i<=nrow(data))
{
cat(rep(toString(l[i]),data[i,6]))
cat("\n")
i <- i + 1
}
sink()

file.show("tweets.txt")

text <- readLines("tweets.txt")

# Load the data as a corpus
docs <- Corpus(VectorSource(text))
 
#inspect the contents of the document
#inspect(docs)

 # Replace @UserName
usrName <- content_transformer(function (text) gsub("@\\w+", "", text))
docs <- tm_map(docs, usrName)

#Replacing “/”, and “|” with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "[ |\t]{2,}")
docs <- tm_map(docs, toSpace, "RT")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "#")
#inspect(docs)



#links
links <- content_transformer(function (text) gsub("https", "", text))
docs <- tm_map(docs, links)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#inspect(docs)


# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#inspect(docs)

#eduu
usrName <- content_transformer(function (text) gsub("eduu\\w+", "", text))
docs <- tm_map(docs, usrName)

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("Dear","dear","tco"))
 

# Text stemming: removes suffixes to make it simple and to get common origin
#docs <- tm_map(docs, stemDocument)
#inspect(docs)

#to build a term document matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

