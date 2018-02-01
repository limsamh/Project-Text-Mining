# install.packages("quanteda")
# install.packages("readr")
# install.packages("RWeka")

library("quanteda")
library(readr)
options(java.parameters = "-Xmx4g")
library("RWeka")



cheminN <-"Datasets/reviews/N/"

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
View(datasetNegative)
rm(res,dataset1,test,cheminN,fich,tmp,val1)
str(datasetNegative)





cheminP <-"Datasets/reviews/P/"

val1 <- "P"
datasetPositive <- data.frame()
test<-data.frame()
# 
# test <- read.delim("Datasets/reviews/N/n_cv100_12406.txt", header = FALSE, dec = ".")
# test$V1<-as.character(test$V1)
# test<-rbind.data.frame(test2,paste(test$V1,collapse = " "))
# 
#  names(test2) <- c("text","class")
# head(test2)

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
View(datasetPositive)
rm(res,dataset1,test,cheminN,fich,tmp,val1)
str(datasetPositive)

#End processidng


datasetTest<-data.frame()
datasetTest <- rbind(datasetPositive,datasetNegative)









# Tokénisation de ma datasetpositive
train.tokens <- tokens(datasetTest$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)


#Importation du fichier stopwords.txt
stpword <- read.delim("stopwords.txt", header = FALSE, dec = ".")
stpword<-as.character(stpword$V1)


train.tokens <- tokens_select(train.tokens, stpword, 
                              selection = "remove") 

dico <- read.table("dico/v-words.txt")
dico <-as.character(dico$V1)
str(dico) 

train.tokens <- tokens_select(train.tokens, dico, 
                              selection = "keep") 



# Premier bag-of-words modèle.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)

df_test <- as.data.frame(train.tokens.matrix)

df_test<-cbind(df_test,datasetTest$class)



rm(train.tokens,train.tokens.dfm,train.tokens.matrix,datasetTest,datasetNegative,datasetPositive)
write.arff(df_test,file="output_arff/datasetFrequence.arff")

