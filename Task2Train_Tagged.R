# install.packages("quanteda")
# install.packages("readr")
# install.packages("RWeka")

library("quanteda")
library("readr")
library("RWeka")



# Pour les train taggés


# Task 2 Train



# Création de la dataset --------------------------------------------------

# Reviews non tagged negative 
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





# Utilisation des N Gram de mot :  N=2 ------------------------------------

# Tokénisation de ma dataset
train.tokens <- tokens(datasetTrain$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)



#Importation du dictionnaire généré sur Python
dico <- read.table("dico/v-words.txt")
dico <-as.character(dico$V1)
str(dico) 
#On ne garde que les mots du dictionnaire
train.tokens <- tokens_select(train.tokens, dico, 
                              selection = "keep")



train.tokens <- tokens_skipgrams(train.tokens, n = 2, skip = 0, concatenator = " ")


# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)

df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,datasetTrain$class)

write.arff(df_test,file="output_arff/2gramsMotTrainTagged.arff")
#On enlève les objets non utilisés
rm(train.tokens,train.tokens.dfm,train.tokens.matrix,df_test)





# Utilisation des N Gram de mot :  N=3 ------------------------------------

# Tokénisation de ma dataset
train.tokens <- tokens(datasetTrain$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)



#Importation du dictionnaire généré sur Python
dico <- read.table("dico/v-words.txt")
dico <-as.character(dico$V1)
str(dico) 
#On ne garde que les mots du dictionnaire
train.tokens <- tokens_select(train.tokens, dico, 
                              selection = "keep")



train.tokens <- tokens_skipgrams(train.tokens, n = 3, skip = 0, concatenator = " ")


# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)

df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,datasetTrain$class)

write.arff(df_test,file="output_arff/3gramsMotTrainTagged.arff")

#On enlève les objets non utilisés
rm(train.tokens,train.tokens.dfm,train.tokens.matrix,df_test)











# Utilisation des N Gram de lettres :  N=2 --------------------------------

# Tokénisation de ma dataset
train.tokens <- tokens(datasetTrain$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)



# Importation du dictionnaire généré sur Python
dico <- read.table("dico/v-words.txt")
dico <-as.character(dico$V1)
str(dico) 
#On ne garde que les mots du dictionnaire
train.tokens <- tokens_select(train.tokens, dico, 
                              selection = "keep")

train.tokens <- tokens_ngrams(train.tokens, n = 2, concatenator = " ")


# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)

df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,datasetTrain$class)

write.arff(df_test,file="output_arff/2gramsLettreTrainTagged.arff")
#On enlève les objets non utilisés
rm(train.tokens,train.tokens.dfm,train.tokens.matrix,df_test)



# Utilisation des N Gram de lettres :  N=3 --------------------------------

# Tokénisation de ma dataset
train.tokens <- tokens(datasetTrain$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)



# Importation du dictionnaire généré sur Python
dico <- read.table("dico/v-words.txt")
dico <-as.character(dico$V1)
str(dico) 
#On ne garde que les mots du dictionnaire
train.tokens <- tokens_select(train.tokens, dico, 
                              selection = "keep")

train.tokens <- tokens_ngrams(train.tokens, n = 3, concatenator = " ")


# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)

df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,datasetTrain$class)

write.arff(df_test,file="output_arff/3gramsLettreTrainTagged.arff")

#On enlève les objets non utilisés
rm(train.tokens,train.tokens.dfm,train.tokens.matrix,df_test)





# Utilisation des N Gram de lettres :  N=2:3 --------------------------------

# Tokénisation de ma dataset
train.tokens <- tokens(datasetTrain$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)



# Importation du dictionnaire généré sur Python
dico <- read.table("dico/v-words.txt")
dico <-as.character(dico$V1)
str(dico) 
#On ne garde que les mots du dictionnaire
train.tokens <- tokens_select(train.tokens, dico, 
                              selection = "keep")

train.tokens <- tokens_ngrams(train.tokens, n = 2:3, concatenator = " ")


# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)

df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,datasetTrain$class)

write.arff(df_test,file="output_arff/2_3gramsLettreTrainTagged.arff")

rm(list=ls())
gc()
