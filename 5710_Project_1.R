#PSYC 5710: Introduction to Machine Learning and Data Mining--Assignment 1: Text Mining

library(tm)
library(SnowballC)
library(ggplot2)
library(qgraph)
library(plotly)
library(dplyr)
library(readr)

beatles <- read.csv("~/Desktop/beatles.csv",
header = TRUE, stringsAsFactors = FALSE) 

#Question 1: 

str(beatles)
corpus <- Corpus(VectorSource(beatles[, 3]))
corpus [[1]]$content

#Lowercase transformation
corpus <- tm_map(corpus, tolower)
corpus 

#Removing numbers and punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus [[1]]$content

#Removing stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus [[1]]$content

#Creating document-term matrix
dtm <- DocumentTermMatrix(corpus)
dtm
corpus [[1]]$content

#Question 2:
#2.1: 187 documents means 187 different columns  
#2.2: 1719 means songs in rows
#2.3: non-sparse referrs to unique words/ sparse means the frequency of certain words within the dataset
#2.4: Sparsity 98% means that 98% of the words are unique 

#3: Stemming the words reduces sparsity without altering the maximum level. 

#Decreasing Sparsity to 0.99
dtm <- removeSparseTerms(dtm, 0.99)
dtm
#Sparsity= 96% 187 doc terms: 776; 6057/139055

#Decreasing Sparsity to 0.98
dtm <- removeSparseTerms(dtm, 0.98)
dtm
#doc: 187, terms: 412, sparsity = 93%; 5225/71819

#Decreasing Sparsity to 0.97
dtm <- removeSparseTerms(dtm, 0.97)
dtm
#doc: 187, terms: 274, sparsity = 91%; 4621/46617

#Decreasing Sparsity to 0.96
dtm <- removeSparseTerms(dtm, 0.96)
dtm
#doc: 187, terms: 219, sparsity = 90%; 4267/36686

#Question 4: As the level of sparsity descreases, the document terms matrices also decreases.

#Question 5: 
dtm.beatles <- DocumentTermMatrix(corpus)
dtm.beatles <- removeSparseTerms(dtm, 0.90)
dtm.beatles
#docs: 187, terms: 72; 2515/10949; sparsity: 81%
beatles.lyrics <- as.data.frame(as.matrix(dtm.beatles))
head(beatles.lyrics)

#Question 6: Frequency plots

freq.dtm <- sort(colSums(beatles.lyrics),decreasing=TRUE) 
freq.data <- data.frame(word = names(freq.dtm),freq=freq.dtm)

library(ggplot2)
freq.plot <- ggplot(freq.data, aes(reorder(word, freq), freq)) + 
  geom_col() + xlab(NULL) + coord_flip() + ylab("Frequency")+ 
  theme(text = element_text(size = 13))
freq.plot
#6.1: Most frequent words are love, know, dont.
#6.2: Least frequent words are turn, show, hear.

#Question 7:
library(qgraph)
library(plotly)
library(dplyr)
beatles.cor.html <- cor_auto(beatles.lyrics)
a <- list(showticklabels = TRUE, tickangle = -45)
plot.cor <- plot_ly(x = colnames(beatles.cor.html), y = colnames(beatles.cor.html),
                    z = beatles.cor.html, type = "heatmap") %>%
  layout(xaxis = a, showlegend = FALSE, margin = list(l=100,b=100,r=100,u=100))
plot.cor  

#7.1: Love and Need are the most strongly positively correlated with value of 0.862. 
