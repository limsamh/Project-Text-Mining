# install.packages("quanteda")
# install.packages("readr")
# install.packages("RWeka")

library("quanteda")
library(readr)
#options(java.parameters = "-Xmx4g")
library("RWeka")

# Pour les test et train taggé

#Task 1 Train

#Reviews non tagged negative
cheminN <-"Datasets/reviews-tagged/train-tagged/neg/"

val1 <- "N"
datasetNegative <- data.frame()
test<-data.frame()
#Extraction

  dataset1 <- data.frame()
  
  
  for(fich in dir(path=cheminN, pattern="*.txt$", recursive=TRUE)){
    
    print(fich)
    res =  read.delim(paste0(cheminN,fich), header = FALSE, sep ="\t")
    res<-subset(res, grepl("JJ",res$V2)|grepl("RB",res$V2)|
                  grepl("NN",res$V2)|grepl("VB",res$V2))
    
    res$V3<-as.character(res$V3)
    tmp<-paste(res$V3,collapse = " ")
    
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
  res =  read.delim(paste0(cheminN,fich), header = FALSE, sep ="\t")
  res<-subset(res, grepl("JJ",res$V2)|grepl("RB",res$V2)|
                grepl("NN",res$V2)|grepl("VB",res$V2))
  
  res$V3<-as.character(res$V3)
  tmp<-paste(res$V3,collapse = " ")
  
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





# Tokénisation de ma dataset
train.tokens <- tokens(datasetTrain$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)


#Importation du fichier stopwords.txt
stpword <- read.delim("stopwords.txt", header = FALSE, dec = ".")
stpword<-as.character(stpword$V1)

#On enlève les stopwords
train.tokens <- tokens_select(train.tokens, stpword, 
                              selection = "remove") 

#Importation du dictionnaire généré sur Python
dico <- read.table("dico/v-tagged.txt")
dico <-as.character(dico$V1)
str(dico) 
#On ne garde que les mots du dictionnaire
train.tokens <- tokens_select(train.tokens, dico, 
                              selection = "keep") 



# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)

df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,datasetTrain$class)

rm(train.tokens,train.tokens.dfm,train.tokens.matrix,datasetTrain,datasetNegative,datasetPositive)

write.arff(df_test,file="output_arff/datasetFrequenceTrainTagged.arff")






#Task 1 Test

#Reviews non tagged negative
cheminN <-"Datasets/reviews-tagged/test-tagged/neg/"

val1 <- "N"
datasetNegative <- data.frame()
test<-data.frame()
#Extraction

dataset1 <- data.frame()


for(fich in dir(path=cheminN, pattern="*.txt$", recursive=TRUE)){
  
  print(fich)
  res =  read.delim(paste0(cheminN,fich), header = FALSE, sep ="\t")
  res<-subset(res, grepl("JJ",res$V2)|grepl("RB",res$V2)|
                grepl("NN",res$V2)|grepl("VB",res$V2))
  
  res$V3<-as.character(res$V3)
  tmp<-paste(res$V3,collapse = " ")
  
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
cheminP <-"Datasets/reviews-tagged/test-tagged/neg/"


val1 <- "P"
datasetPositive <- data.frame()
test<-data.frame()

#Extraction

dataset1 <- data.frame()

for(fich in dir(path=cheminP, pattern="*.txt$", recursive=TRUE)){
  
  print(fich)
  res =  read.delim(paste0(cheminN,fich), header = FALSE, sep ="\t")
  res<-subset(res, grepl("JJ",res$V2)|grepl("RB",res$V2)|
                grepl("NN",res$V2)|grepl("VB",res$V2))
  
  res$V3<-as.character(res$V3)
  tmp<-paste(res$V3,collapse = " ")
  
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

datasetTest<-data.frame()
datasetTest <- rbind(datasetPositive,datasetNegative)





# Tokénisation de ma dataset
train.tokens <- tokens(datasetTest$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)


#Importation du fichier stopwords.txt
stpword <- read.delim("stopwords.txt", header = FALSE, dec = ".")
stpword<-as.character(stpword$V1)

#On enlève les stopwords
train.tokens <- tokens_select(train.tokens, stpword, 
                              selection = "remove") 

#Importation du dictionnaire généré sur Python
dico <- read.table("dico/v-tagged.txt")
dico <-as.character(dico$V1)
str(dico) 
#On ne garde que les mots du dictionnaire
train.tokens <- tokens_select(train.tokens, dico, 
                              selection = "keep") 



# Premier modèle bag-of-words.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)

df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,datasetTest$class)

write.arff(df_test,file="output_arff/datasetFrequenceTestTagged.arff")

rm(list=ls())

