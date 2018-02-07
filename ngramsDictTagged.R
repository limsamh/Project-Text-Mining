# install.packages("quanteda")
# install.packages("readr")
# install.packages("RWeka")

library("quanteda")
library("readr")
library("RWeka")



# Train ngram Mot--------------------------------------------------


#Reviews non tagged negative
cheminN <-"Datasets/reviews-tagged/train-tagged/neg/"

val1 <- "N"
datasetNegative <- data.frame()
test<-data.frame()
#Extraction

dataset1 <- data.frame()


for(fich in dir(path=cheminN, pattern="*.txt$", recursive=TRUE)){
  
  print(fich)
  res =  read.delim(paste0(cheminN,fich), header = FALSE, dec = ".")
  res$V1<-as.character(res$V1)
  tmp<-paste(res$V1,collapse = " ")
  
  test<-rbind(test,tmp)
  test<- cbind(test,val1)
  names(test) <- c("text","class")
  dataset1 <- rbind.data.frame(dataset1,test)
  test<-data.frame()
}
datasetNegative <- rbind(datasetNegative,dataset1)
dataset1 <- data.frame()

datasetNegative$text<-as.character(datasetNegative$text)
rm(res,dataset1,test,cheminN,fich,tmp,val1)


#Reviews non tagged positive
cheminP <-"Datasets/reviews-tagged/train-tagged/pos/"

val1 <- "P"
datasetPositive <- data.frame()
test<-data.frame()

#Extraction

dataset1 <- data.frame()

for(fich in dir(path=cheminP, pattern="*.txt$", recursive=TRUE)){
  
  print(fich)
  res =  read.delim(paste0(cheminP,fich), header = FALSE, dec = ".")
  res$V1<-as.character(res$V1)
  tmp<-paste(res$V1,collapse = " ")
  
  test<-rbind(test,tmp)
  test<- cbind(test,val1)
  names(test) <- c("text","class")
  dataset1 <- rbind.data.frame(dataset1,test)
  test<-data.frame()
}
datasetPositive <- rbind(datasetPositive,dataset1)
dataset1 <- data.frame()

datasetPositive$text<-as.character(datasetPositive$text)
rm(res,dataset1,test,cheminP,fich,tmp,val1)
#End processing

datasetTrain<-data.frame()
datasetTrain <- rbind(datasetPositive,datasetNegative)




# 2 gram Mot dictionnaire-------------------------
train.tokens <- tokens_skipgrams(tokens(datasetTrain$text, what = "word", 
                                        remove_numbers = TRUE, remove_punct = TRUE,
                                        remove_symbols = TRUE, remove_hyphens = TRUE),n= 2, skip = 0, concatenator = " ")



train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.dfm <- dfm_trim(train.tokens.dfm, min_docfreq = 10)

train.tokens.matrix <- as.matrix(train.tokens.dfm)


dictTmp <- colnames(train.tokens.matrix)


write.table(dictTmp, file = "newDict/2grams_tagged_words.txt", sep = "\n" , col.names = FALSE, row.names = FALSE)
rm(train.tokens,train.tokens.dfm,train.tokens.matrix,dictTmp)



# 3 gram Mot dictionnaire-------------------------
train.tokens <- tokens_skipgrams(tokens(datasetTrain$text, what = "word", 
                                        remove_numbers = TRUE, remove_punct = TRUE,
                                        remove_symbols = TRUE, remove_hyphens = TRUE),n= 3, skip = 0, concatenator = " ")



train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.dfm <- dfm_trim(train.tokens.dfm, min_docfreq = 10)

train.tokens.matrix <- as.matrix(train.tokens.dfm)


dictTmp <- colnames(train.tokens.matrix)


write.table(dictTmp, file = "newDict/3grams_tagged_words.txt", sep = "\n" , col.names = FALSE, row.names = FALSE )
rm(train.tokens,train.tokens.dfm,train.tokens.matrix,dictTmp)


# 2-3 gram Mot dictionnaire-------------------------
train.tokens <- tokens_skipgrams(tokens(datasetTrain$text, what = "word", 
                                        remove_numbers = TRUE, remove_punct = TRUE,
                                        remove_symbols = TRUE, remove_hyphens = TRUE),n= 2:3, skip = 0, concatenator = " ")



train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.dfm <- dfm_trim(train.tokens.dfm, min_docfreq = 15)

train.tokens.matrix <- as.matrix(train.tokens.dfm)


dictTmp <- colnames(train.tokens.matrix)


write.table(dictTmp, file = "newDict/2_3_grams_tagged_words.txt", sep = "\n" , col.names = FALSE, row.names = FALSE   )
rm(train.tokens,train.tokens.dfm,train.tokens.matrix,dictTmp)



# 2 gram Lettre dictionnaire-------------------------
train.tokens <- tokens_ngrams(tokens(datasetTrain$text, what = "word", 
                                     remove_numbers = TRUE, remove_punct = TRUE,
                                     remove_symbols = TRUE, remove_hyphens = TRUE),n= 2, concatenator = " ")



train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.dfm <- dfm_trim(train.tokens.dfm, min_docfreq = 10)

train.tokens.matrix <- as.matrix(train.tokens.dfm)


dictTmp <- colnames(train.tokens.matrix)


write.table(dictTmp, file = "newDict/2grams_tagged_letters.txt", sep = "\n" , col.names = FALSE, row.names = FALSE   )
rm(train.tokens,train.tokens.dfm,train.tokens.matrix,dictTmp)



# 3 gram Lettre dictionnaire-------------------------
train.tokens <- tokens_ngrams(tokens(datasetTrain$text, what = "word", 
                                     remove_numbers = TRUE, remove_punct = TRUE,
                                     remove_symbols = TRUE, remove_hyphens = TRUE),n= 3, concatenator = " ")



train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.dfm <- dfm_trim(train.tokens.dfm, min_docfreq = 10)

train.tokens.matrix <- as.matrix(train.tokens.dfm)


dictTmp <- colnames(train.tokens.matrix)


write.table(dictTmp, file = "newDict/3grams_tagged_letters.txt", sep = "\n" , col.names = FALSE, row.names = FALSE   )
rm(train.tokens,train.tokens.dfm,train.tokens.matrix,dictTmp)


# 2-3 gram Lettre dictionnaire-------------------------
train.tokens <- tokens_ngrams(tokens(datasetTrain$text, what = "word", 
                                     remove_numbers = TRUE, remove_punct = TRUE,
                                     remove_symbols = TRUE, remove_hyphens = TRUE),n= 2:3, concatenator = " ")



train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.dfm <- dfm_trim(train.tokens.dfm, min_docfreq = 15)

train.tokens.matrix <- as.matrix(train.tokens.dfm)


dictTmp <- colnames(train.tokens.matrix)


write.table(dictTmp, file = "newDict/2_3_grams_tagged_letters.txt", sep = "\n" , col.names = FALSE, row.names = FALSE)

rm(list=ls())



