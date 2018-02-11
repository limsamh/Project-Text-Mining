# install.packages("quanteda")
# install.packages("readr")
# install.packages("RWeka")
# install.packages("arules")


#Importation des libraires
library("quanteda")
library("readr")
library("RWeka")
library("arules")
library("arulesSequences")


# Fonction creation de dictionnaire ---------------------------------------

char2dictionary <- function(x) {
  result <- as.list(x)  # coercion du vector en list
  names(result) <- x
  dictionary(result)
}



# Train  ------------------------------------

# Création de la dataset --------------------------------------------------


##Negative
cheminN <-"Datasets/reviews/train/N/"

val1 <- "N"
datasetNegative <- data.frame()
#Extraction

res1 <- data.frame()
res2 <-c(1:800)
res2 <- as.data.frame(res2)
for(fich in dir(path=cheminN, pattern="*.txt$", recursive=TRUE)){
  
  print(fich)
  res =  read.delim(paste0(cheminN,fich), header = FALSE, dec = ".")
  res$V1<-as.character(res$V1)
  res$V2 <- res2[1,1]
  res$V3 <- val1
  res$v4 <- c(1:nrow(res))
  res1 <- rbind(res1,res)
  res2 <-res2[-1,]
  res2 <- as.data.frame(res2)
  print(paste("Restant " , nrow(res2), sep = "" ) )
}
datasetNegative <- rbind(datasetNegative,res1)
datasetNegative$V1<-as.character(datasetNegative$V1)

#Positive
cheminP <-"Datasets/reviews/train/P/"

val1 <- "P"
datasetPositive <- data.frame()
#Extraction

res1 <- data.frame()
res2 <-c(1:800)
res2 <- as.data.frame(res2)
for(fich in dir(path=cheminP, pattern="*.txt$", recursive=TRUE)){
  print(fich)
  res =  read.delim(paste0(cheminP,fich), header = FALSE, dec = ".")
  res$V1<-as.character(res$V1)
  res$V2 <- res2[1,1]
  res$V3 <-val1
  res$v4 <- c(1:nrow(res))
  res1 <- rbind(res1,res)
  res2 <-res2[-1,]
  res2 <- as.data.frame(res2)
  print(paste("Restant " , nrow(res2), sep = "" ) )
}

datasetPositive <- rbind(datasetPositive,res1)
datasetPositive$V1<-as.character(datasetPositive$V1)



# Transactions POS --------------------------------------------------------

train.tokens <- tokens(datasetPositive$V1, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)

#Importation du fichier stopwords.txt
stpword <- read.delim("stopwords.txt", header = FALSE, dec = ".")
stpword<-as.character(stpword$V1)

#On enlève les stopwords
train.tokens <- tokens_select(train.tokens, stpword, 
                              selection = "remove") 
# coercion en liste
ItemSetFreq <- as.list(train.tokens)

taille <- nrow(datasetPositive)
# set transaction names
names(ItemSetFreq) <- paste("Tr",c(1:taille), sep = "")

# coerce en transactions
trans2 <- as(ItemSetFreq, "transactions")
transactionInfo(trans2)$sequenceID <- datasetPositive$V2
transactionInfo(trans2)$eventID <- datasetPositive$v4


s1 <- cspade(trans2, parameter = list(support = 0.1), 
             control   = list(verbose = TRUE, tidLists = TRUE))

res1 <- as(sort(s1)[1:4000], "data.frame")
res1$class <- "P"




# Transactions NEG --------------------------------------------------------

train.tokens <- tokens(datasetNegative$V1, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)

#Importation du fichier stopwords.txt
stpword <- read.delim("stopwords.txt", header = FALSE, dec = ".")
stpword<-as.character(stpword$V1)

#On enlève les stopwords
train.tokens <- tokens_select(train.tokens, stpword, 
                              selection = "remove") 
# coercion en liste
ItemSetFreq <- as.list(train.tokens)

taille <- nrow(datasetNegative)
# set transaction names
names(ItemSetFreq) <- paste("Tr",c(1:taille), sep = "")

# coerce en transactions
trans2 <- as(ItemSetFreq, "transactions")
transactionInfo(trans2)$sequenceID <- datasetNegative$V2
transactionInfo(trans2)$eventID <- datasetNegative$v4


s1 <- cspade(trans2, parameter = list(support = 0.1), 
             control   = list(verbose = TRUE, tidLists = TRUE))



res2 <- as(sort(s1)[1:4000], "data.frame")
res2$class <- "N"

# DatasetFinal --------------------------------------------------------


res <- rbind(res1,res2)


res$class <-as.factor(res$class)

# Transformation en tokens ---------------------------------------------------

test<- as.list(res$sequence)
test<- as.tokens(test, concatenator = " ")

train.tokens1 <- tokens(test, what = "sentence", 
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, remove_hyphens = TRUE)



# Importation du dictionnaire
dico <- read.table("newDict/sequenceDict.txt")
dico <-as.character(dico$V1)


# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens1, tolower = FALSE)

train.tokens.dfm <- dfm_lookup(train.tokens.dfm, dictionary = char2dictionary(dico))

train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)


df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,DATASEQCLASS = res$class)


write.arff(df_test,file="output_arff/Task3_Train_FrequentSequences.arff")


rm(list = ls())











# Fonction creation de dictionnaire ---------------------------------------

char2dictionary <- function(x) {
  result <- as.list(x)  # coercion du vector en list
  names(result) <- x
  dictionary(result)
}



# Test  ------------------------------------

# Création de la dataset --------------------------------------------------


##Negative
cheminN <-"Datasets/reviews/test/N/"

val1 <- "N"
datasetNegative <- data.frame()
#Extraction

res1 <- data.frame()
res2 <-c(1:800)
res2 <- as.data.frame(res2)
for(fich in dir(path=cheminN, pattern="*.txt$", recursive=TRUE)){
  
  print(fich)
  res =  read.delim(paste0(cheminN,fich), header = FALSE, dec = ".")
  res$V1<-as.character(res$V1)
  res$V2 <- res2[1,1]
  res$V3 <- val1
  res$v4 <- c(1:nrow(res))
  res1 <- rbind(res1,res)
  res2 <-res2[-1,]
  res2 <- as.data.frame(res2)
  print(paste("Restant " , nrow(res2), sep = "" ) )
}
datasetNegative <- rbind(datasetNegative,res1)
datasetNegative$V1<-as.character(datasetNegative$V1)

#Positive
cheminP <-"Datasets/reviews/test/P/"

val1 <- "P"
datasetPositive <- data.frame()
#Extraction

res1 <- data.frame()
res2 <-c(1:800)
res2 <- as.data.frame(res2)
for(fich in dir(path=cheminP, pattern="*.txt$", recursive=TRUE)){
  print(fich)
  res =  read.delim(paste0(cheminP,fich), header = FALSE, dec = ".")
  res$V1<-as.character(res$V1)
  res$V2 <- res2[1,1]
  res$V3 <-val1
  res$v4 <- c(1:nrow(res))
  res1 <- rbind(res1,res)
  res2 <-res2[-1,]
  res2 <- as.data.frame(res2)
  print(paste("Restant " , nrow(res2), sep = "" ) )
}

datasetPositive <- rbind(datasetPositive,res1)
datasetPositive$V1<-as.character(datasetPositive$V1)



# Transactions POS --------------------------------------------------------

train.tokens <- tokens(datasetPositive$V1, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)

#Importation du fichier stopwords.txt
stpword <- read.delim("stopwords.txt", header = FALSE, dec = ".")
stpword<-as.character(stpword$V1)

#On enlève les stopwords
train.tokens <- tokens_select(train.tokens, stpword, 
                              selection = "remove") 
# coercion en liste
ItemSetFreq <- as.list(train.tokens)

taille <- nrow(datasetPositive)
# set transaction names
names(ItemSetFreq) <- paste("Tr",c(1:taille), sep = "")

# coerce en transactions
trans2 <- as(ItemSetFreq, "transactions")
transactionInfo(trans2)$sequenceID <- datasetPositive$V2
transactionInfo(trans2)$eventID <- datasetPositive$v4


s1 <- cspade(trans2, parameter = list(support = 0.1), 
             control   = list(verbose = TRUE, tidLists = TRUE))

res1 <- as(sort(s1)[1:200], "data.frame")
res1$class <- "P"




# Transactions NEG --------------------------------------------------------

train.tokens <- tokens(datasetNegative$V1, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)

#Importation du fichier stopwords.txt
stpword <- read.delim("stopwords.txt", header = FALSE, dec = ".")
stpword<-as.character(stpword$V1)

#On enlève les stopwords
train.tokens <- tokens_select(train.tokens, stpword, 
                              selection = "remove") 
# coercion en liste
ItemSetFreq <- as.list(train.tokens)

taille <- nrow(datasetNegative)
# set transaction names
names(ItemSetFreq) <- paste("Tr",c(1:taille), sep = "")

# coerce en transactions
trans2 <- as(ItemSetFreq, "transactions")
transactionInfo(trans2)$sequenceID <- datasetNegative$V2
transactionInfo(trans2)$eventID <- datasetNegative$v4


s1 <- cspade(trans2, parameter = list(support = 0.1), 
             control   = list(verbose = TRUE, tidLists = TRUE))



res2 <- as(sort(s1)[1:200], "data.frame")
res2$class <- "N"

# DatasetFinal --------------------------------------------------------


res <- rbind(res1,res2)


res$class <-as.factor(res$class)

# Transformation en tokens ---------------------------------------------------

test<- as.list(res$sequence)
test<- as.tokens(test, concatenator = " ")

train.tokens1 <- tokens(test, what = "sentence", 
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, remove_hyphens = TRUE)



# Importation du dictionnaire
dico <- read.table("newDict/sequenceDict.txt")
dico <-as.character(dico$V1)


# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens1, tolower = FALSE)

train.tokens.dfm <- dfm_lookup(train.tokens.dfm, dictionary = char2dictionary(dico))

train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)


df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,DATASEQCLASS = res$class)


write.arff(df_test,file="output_arff/Task3_Test_FrequentSequences.arff")


rm(list = ls())
