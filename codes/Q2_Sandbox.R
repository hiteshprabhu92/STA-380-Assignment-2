### Question 2 : Assignment 2 ##

library(tm)
library(foreach)

# Defining function
readerPlain = function(fname){
  readPlain(elem = list(content = readLines(fname)), 
            id =  fname, language = 'en') }

## Reading in all authors' filepaths and author's names
author_dirs = Sys.glob('C:/Users/Hitesh Prabhu/OneDrive/Documents/MSBA Workspace/Study Material/Summer/James Scott/STA380-master/STA380-master/data/ReutersC50/C50train/*')
file_list = NULL
labels = NULL
for(author in author_dirs) 
{
  author_name = substring(author, first = 145)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

# Creating training corpus
my_corpus = Corpus(VectorSource(all_docs))
names(my_corpus) = file_list

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

# Creating DTM
DTM = DocumentTermMatrix(my_corpus)
DTM # some basic summary statistics

# Removing sparse items
DTM = removeSparseTerms(DTM, 0.975)
DTM

# Now a dense matrix
X = as.matrix(DTM)
smooth_count = 1/nrow(X)

# Naive Bayes: the training sets for all the authors
w = list()
smooth_count = 1/nrow(X)
j = 1
for (i in seq(1,length(file_list),50) )
{
  w[[j]] = colSums(X[i:(i+49),] + smooth_count)/sum(colSums(X[i:(i+49),] 
                                                            + smooth_count))
  j = j + 1
}


### TEST ###

readerPlain = function(fname){
  readPlain(elem = list(content = readLines(fname)), 
            id = fname, language = 'en') }
## Rolling two directories together into a single corpus
author_dirs_test = Sys.glob('C:/Users/Hitesh Prabhu/OneDrive/Documents/MSBA Workspace/Study Material/Summer/James Scott/STA380-master/STA380-master/data/ReutersC50/C50test/*')
#author_dirs = author_dirs[1:2]
file_list_test = NULL
labels_test = NULL
for(author in author_dirs_test) {
  author_name = substring(author, first = 144)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list_test = append(file_list_test, files_to_add)
  labels_test = append(labels_test, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
all_docs_test = lapply(file_list_test, readerPlain) 
names(all_docs_test) = file_list_test
names(all_docs_test) = sub('.txt', '', names(all_docs_test))

my_corpus_test = Corpus(VectorSource(all_docs_test))
names(my_corpus_test) = labels_test

# Preprocessing
my_corpus_test = tm_map(my_corpus_test, content_transformer(tolower)) # make everything lowercase
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeNumbers)) # remove numbers
my_corpus_test = tm_map(my_corpus_test, content_transformer(removePunctuation)) # remove punctuation
my_corpus_test = tm_map(my_corpus_test, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeWords), stopwords("SMART"))

DTM_test = DocumentTermMatrix(my_corpus_test)
DTM_test # some basic summary statistics

# Keeping only those words which we used in the training set.
common_words = colnames(DTM_test)[colnames(DTM_test) %in% colnames(DTM)]
DTM_test <- DTM_test[, common_words]
DTM_test

# Taking test documents and Comparing log probabilities 
X_test = as.matrix(DTM_test)
# Creating empty matrix to calculate log-probabilities
Y_test = matrix(, nrow = 2500, ncol = 50)
K = list()
j = 1
for (i in 1:2500)
{
  for (j in 1:50)
  {
    Y_test[i,j] = sum(X_test[i,]*log(w[[j]]))
  }
}

# Finding the document which corresponds to maximum log-probability (Hence the author)
# This can be done in a more readable way using for loop
library(dplyr)
author_predictions <- as.vector(t(as.data.frame(t(Y_test)) %>%
                      summarise_each(funs(which.max(.) ) ) ) )
# Since authors are arranged so well with one author for every fifty files 
author_actual <- as.vector(rep(1:50,each=50))

library(caret)
library(e1071) # Weird. ConfusionMatrix asked for this library
confMatrix <- confusionMatrix(author_predictions,author_actual)

confMatrix$overall["Accuracy"]

# ------------------------------------------------------------------------

library(randomForest)
rffit = randomForest(y = as.factor(rep(1:50,each=50)), x = as.matrix(DTM_test), 
                     mtry = 50, ntree = 500)

predicted_price_rftree <- predict(rffit, newdata = as.matrix(DTM))

library(caret)
library(e1071) # Weird. ConfusionMatrix asked for this library
confMatrix <- confusionMatrix(as.vector(predicted_price_rftree), author_actual)
confMatrix$overall["Accuracy"]



rffit = randomForest(y = as.factor(rep(1:50,each=50)), x = as.matrix(DTM), 
                     mtry = 50, ntree = 500)

predicted_price_rftree <- predict(rffit, newdata = as.matrix(DTM_test))
confMatrix <- confusionMatrix(as.vector(predicted_price_rftree), author_actual)
confMatrix$overall["Accuracy"]

