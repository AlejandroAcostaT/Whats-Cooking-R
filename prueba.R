install.packages("jsonlite")
install.packages("rpart.plot")
install.packages("tm")
install.packages("e1071")
install.packages("SnowballC")

library("jsonlite")
library("rpart")
library("rpart.plot")
library("tm")
library("class")


######################################
#             Read files             #
######################################
train <- fromJSON("train.json", flatten = TRUE)
test <- fromJSON("test.json", flatten = TRUE)

#add dependent variable
test$cuisine <- NA
#combine data set
combi <- rbind(train, test)

######################################
#         Preprocess Data            #
######################################

#create corpus
corpus <- Corpus(VectorSource(combi$ingredients))

#convert text to lowercase
corpus <- tm_map(corpus, tolower)
corpus[[1]]

#remove punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]

#remove stopwords
corpus <- tm_map(corpus, removeWords, c(stopwords('english')))
corpus[[1]]

#remove whitespaces
corpus <- tm_map(corpus, stripWhitespace)
corpus[[1]]

#perform stemming
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]
corpus <- tm_map(corpus, PlainTextDocument)

#document matrix
frequencies <- DocumentTermMatrix(corpus) 
frequencies

#remove sparse terms
sparse <- removeSparseTerms(frequencies, 1 - 3/nrow(frequencies))
dim(sparse)

#create sparse as data frame
newsparse <- as.data.frame(as.matrix(sparse))
dim(newsparse)

#check if all words are appropriate
colnames(newsparse) <- make.names(colnames(newsparse))

#check for the dominant dependent variable
table(train$cuisine)

#add cuisine
newsparse$cuisine <- as.factor(c(train$cuisine, rep('italian', nrow(test))))

#split data 
mytrain <- newsparse[1:nrow(train),]
mytest <- newsparse[-(1:nrow(train)),]

######################################
#             RPART TREE             #
######################################

set.seed(9347)
cartModelFit <- rpart(cuisine ~ ., data = mytrain, method = "class")
## Plot the tree
prp(cartModelFit)

#Confusion Matrix
cartPredict <- predict(cartModelFit, newdata = mytest, type = "class")

cuisine <-c()
for(i in cartPredict){
  cuisine<-c(cuisine, i)
}

df <- data.frame(test$id,cuisine)
write.csv(df, file = "Output.csv")
