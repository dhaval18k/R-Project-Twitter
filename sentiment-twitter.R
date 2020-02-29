getwd()
setwd('I:/Degree/sem-8/R-programming/project/sentiment-twitter')


# Read the File

# apple <- read.csv("I:/Degree/sem-8/R-programming/project/sentiment-twitter/apple.csv",header = TRUE)

apple <- read.csv(file.choose(),header = TRUE)  # provision to select the file

# structure of the data

str(apple)


## build Corpus (Corpus is a collection of text document over which we would apply text mining) each tweet treated as document

install.packages("tm")  # text mining library 
install.packages("NLP") # Natural Language Processing library 

library(NLP)
library(tm) # tm require NLP

corpus <- iconv(apple$text, to = "utf-8")

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


removeURL <- function(x) gsub('http[[:graph:]]*',' ',x)
 


cleanset <- tm_map(cleanset,content_transformer(removeURL))
inspect(cleanset[1:10])

#6 If something is removed from tweet then it leaves blank space we need to get rid of that too 

cleanset <- tm_map(cleanset,removeWords, c('aapl','apple')) # take out the commnon words (run after line no : 75)

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

barplot(w, las=2, col=rainbow(20))  # las =2 all the words listed vertically on x axis


























