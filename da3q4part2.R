# Load
library("NLP")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")

data <- read.csv(file="F:/DA/demonetization.csv", header=TRUE, sep=",")
data <- subset(data,retweetCount>50)

#writing to a text file for clean up

l <- data$text
i <- 1
 sink("tweets.txt")
 while(i<=nrow(data))
{
cat(toString(l[i]))
cat("\n")
i <- i + 1
}
sink()

#file.show("tweets.txt")

text <- readLines("tweets.txt")

#convert all text to lower case
tweets.text <- tolower(text)

# Replace blank space (“rt”)
 tweets.text <- gsub("rt", "", text)

 # Replace @UserName
 tweets.text <- gsub("@\\w+", "", text)

 # Remove punctuation
 tweets.text <- gsub("[[:punct:]]", "", text)

 # Remove links
 tweets.text <- gsub("http\\w+", "", text)

 # Remove tabs
 tweets.text <- gsub("[ |\t]{2,}", "", text)

 # Remove blank spaces at the beginning
 tweets.text <- gsub("^ ", "", text)

# Remove blank spaces at the end
 tweets.text <- gsub(" $", "", text)

library("tm")
#create corpus
tweets.text.corpus <- Corpus(VectorSource(text))

#clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))

library("wordcloud")

#generate wordcloud
wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)