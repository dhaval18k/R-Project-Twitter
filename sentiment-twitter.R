getwd()
setwd('/Users/dhaval/Downloads/R-Project-Twitter-master/')


# Read the File

# apple <- read.csv("/Users/dhaval/Downloads/R-Project-Twitter-master/apple.csv",header = TRUE)

apple <- read.csv(file.choose(),header = TRUE)  # provision to select the file

# structure of the data

str(apple)


## build Corpus (Corpus is a collection of text document over which we would apply text mining) each tweet treated as document

install.packages("tm")  # text mining library 
install.packages("NLP") # Natural Language Processing library 

library(NLP)
library(tm) # tm require NLP

corpus <- iconv(apple$text, to = "utf-8-mac")

corpus <- Corpus(VectorSource(corpus))

inspect(corpus[1:3])    # just to see what are the tweets are there


## Data(Text) Cleaning 

#1 Convert the tweets into lower case

corpus <- tm_map(corpus,tolower)
inspect(corpus[1:3])

#2 Remove the Punctuation like colon, full stop, comma etc ..

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:3])

#3 Remove all the numbers 

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:3])

#4 Remove some english word that they are so common and do not add so much value 

cleanset <- tm_map(corpus,removeWords,stopwords('english'))
inspect(cleanset[1:3])

#5 Remove URL words like http etc


removeURL <- function(x) gsub('http[[:alnum:]]*', '',x)
 


cleanset <- tm_map(cleanset,content_transformer(removeURL))
inspect(cleanset[1:10])

#6 If something is removed from tweet then it leaves blank space we need to get rid of that too 

cleanset <- tm_map(cleanset,removeWords, c('aapl','apple')) # take out the commnon words (run after line no : 75)

cleanset <- tm_map(cleanset, gsub, pattern='stocks',replacement='stock')

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])


## Term Document Matrix (text data like tweets is unstructured data if we want to do analysis we need to convert it into structured data to rows and cols)

tdm <- TermDocumentMatrix(cleanset)
tdm

tdm <- as.matrix(tdm) # convert into matrix 

tdm[1:10, 1:20]     # 1:10 (first 10 words) # 1:20 (first 20 tweets)  


## Bar plot

# find how often each word appears in this dataset 

w <- rowSums(tdm)

w <- subset(w, w >= 25) # frequency of word w is >= 25 will only be shown

barplot(w, las=2, col=rainbow(50))  # las =2 all the words listed vertically on x axis


## create the wordcloud

install.packages("wordcloud")
install.packages("RColorBrewer")
library(RColorBrewer)
library(wordcloud)

w <- sort(rowSums(tdm),decreasing = TRUE)

set.seed(222)

wordcloud(words = names(w),freq = w,max.words = 150,random.order = FALSE,colors = brewer.pal(7,'Dark2'))


## create the interesting wordcloud2
install.packages("wordcloud2")
library(wordcloud2)

w <- data.frame(names(w),w)

colnames(w) <- c('word','frequency')

head(w)

wordcloud2(w,size = 0.8,shape = 'circle')

wordcloud2(w,size = 0.8,shape = 'star',rotateRatio = 0.5,minSize = 1)

wordcloud2(w,size = 0.8, shape = 'triangle')

# letterCloud(w,word = "D",wordSize = 1)


## Sentiment analysis of Tweets

install.packages("syuzhet")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("reshape2")
install.packages("dplyr")


library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#1 read the file
apple <- read.csv(file.choose(),header = TRUE)

tweets <- iconv(apple$text,to ='utf-8-mac')

#2 obtain sentiment score


s <- get_nrc_sentiment(tweets)
head(s)

tweets[4]

get_nrc_sentiment('ugly')
get_nrc_sentiment('delay')


## Create bar plot
barplot(colSums(s),las=2,col = rainbow(10),ylab = 'Count',main = 'Sentiment Scores for Apple Tweets')












