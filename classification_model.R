# Load required libraries
library(tm)
library(NLP)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(stringr)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tidyr)
library(dplyr)
library(slam)
library(text)
library(glmnet)
library(caret)
library(e1071)
library(igraph)
library(ggraph)
library(visNetwork)
library(LDAvis)
library(textmineR)
library(stringi)
library(pdftools)
library(rvest)
library(caret)
library(rsample)

#loading files
folder <- "C:\\Users\\Pc\\OneDrive - Higher Education Commission\\text_data_analysis\\economic_updates_2023_24"
folder
#Reading Files from Folder
filelist <- list.files(path = folder)
filelist <- paste(folder, "\\" ,filelist, sep="")
typeof(filelist)

#corpus
a <- lapply(filelist, FUN = readLines) # for readin line 
corpus <- lapply(a, FUN = paste, collapse= " ")

corpus2 <-gsub(pattern = "\\W", replace= " ",corpus) # to get rid of puntuations ,dots ets
corpus2 <-gsub(pattern = "\\d", replace= " ",corpus2) # for digits
corpus2 <- tolower(corpus2)


# stopwords
corpus2 <- removeWords(corpus2, stopwords("english"))

# naw we remove single later words like  in document when view , there is alot of c d like words
corpus2 <- gsub(pattern = "\\b[A-z]\\b[1]", replace= " ",corpus2)

#naw geting rid of white spaces
corpus2 <- stripWhitespace(corpus2)

dtm <- DocumentTermMatrix(corpus2)
dtm <- removeSparseTerms(dtm, 0.99)


# convert into data Frame
data <- as.data.frame(as.matrix(dtm))

# Example: Creating a dummy label column
data$label <- sample(c(0, 1), nrow(data), replace = TRUE)


#Split the Data into Training and Testing Sets:

set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data$label, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[trainIndex,]
dataTest  <- data[-trainIndex,]


# Using Naive Bayes classifier as an example
model <- naiveBayes(label ~ ., data = dataTrain)

#Make Predictions:
predictions <- factor(predictions, levels = c(0, 1))
dataTest$label <- factor(dataTest$label, levels = c(0, 1))

#Evaluate the Model:

confusion_mat <- confusionMatrix(predictions, dataTest$label)
print(confusion_mat)


