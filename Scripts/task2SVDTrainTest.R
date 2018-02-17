# install.packages("quanteda")
# install.packages("readr")
# install.packages("RWeka")

library("quanteda")
library("readr")
library("RWeka")
library("irlba")


# Fonction creation de dictionnaire ---------------------------------------

char2dictionary <- function(x) {
  result <- as.list(x)  # coercion du vector en list
  names(result) <- x
  dictionary(result)
}


# Task 2 Train

# Création de la dataset --------------------------------------------------

# Reviews non tagged negative 
cheminN <-"Datasets/reviews/train/N/"


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
cheminP <-"Datasets/reviews/train/P/"

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





# Utilisation des N Gram de mot :  N=2 ------------------------------------

# Tokénisation de ma dataset
train.tokens <- tokens(datasetTrain$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)



dico <- read.table("newDict/2grams_words.txt")
dico <-as.character(dico$V1)



train.tokens <- tokens_skipgrams(train.tokens, n = 2, skip = 0, concatenator = " ")


# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.dfm <- dfm_lookup(train.tokens.dfm, dictionary = char2dictionary(dico))


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)

train.irlba <- irlba(t(train.tokens.matrix),nv = 200, maxit=500)

class(train.irlba$v)


df_test <- as.data.frame(train.irlba$v)

df_test<-cbind(df_test,DATAFREQCLASS = datasetTrain$class)

write.arff(df_test,file="output_arff/2gramsMotTrainSVD.arff")


rm(list=ls())




# Fonction creation de dictionnaire ---------------------------------------

char2dictionary <- function(x) {
  result <- as.list(x)  # coercion du vector en list
  names(result) <- x
  dictionary(result)
}


# Task 2 Test



# Création de la dataset --------------------------------------------------

# Reviews non tagged negative 
cheminN <-"Datasets/reviews/test/N/"


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
cheminP <-"Datasets/reviews/test/P/"

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





# Utilisation des N Gram de mot :  N=2 ------------------------------------

# Tokénisation de ma dataset
train.tokens <- tokens(datasetTrain$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)



dico <- read.table("newDict/2grams_words.txt")
dico <-as.character(dico$V1)



train.tokens <- tokens_skipgrams(train.tokens, n = 2, skip = 0, concatenator = " ")


# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.dfm <- dfm_lookup(train.tokens.dfm, dictionary = char2dictionary(dico))


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)

train.irlba <- irlba(t(train.tokens.matrix),nv = 200, maxit=500)

class(train.irlba$v)


df_test <- as.data.frame(train.irlba$v)

df_test<-cbind(df_test,DATAFREQCLASS = datasetTrain$class)

write.arff(df_test,file="output_arff/2gramsMotTestSVD.arff")


rm(list=ls())
gc()
