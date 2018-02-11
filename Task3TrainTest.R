# install.packages("quanteda")
# install.packages("readr")
# install.packages("RWeka")
# install.packages("arules")
#options(java.parameters = "-Xmx4g")


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




# Task 1 Train ------------------------------------------------------------

# Création de la dataset --------------------------------------------------


#Reviews non tagged negative
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
  res$V3 <- sample(1:1000, 1)
  res1 <- rbind(res1,res)
  res2 <-res2[-1,]
  res2 <- as.data.frame(res2)
  print(paste("Restant " , nrow(res2), sep = "" ) )
  #Sys.sleep(2)
}

datasetNegative <- rbind(datasetNegative,res1)

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
  res$V3 <- sample(1:1000, 1)
  res1 <- rbind(res1,res)
  res2 <-res2[-1,]
  res2 <- as.data.frame(res2)
  print(paste("Restant " , nrow(res2), sep = "" ) )
}

datasetPositive <- rbind(datasetPositive,res1)

datasetPositive$V1<-as.character(datasetPositive$V1)

train.tokens1 <- tokens(datasetPositive$V1, what = "word", 
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, remove_hyphens = TRUE)

#Importation du fichier stopwords.txt
stpword <- read.delim("stopwords.txt", header = FALSE, dec = ".")
stpword<-as.character(stpword$V1)

#On enlève les stopwords
train.tokens1 <- tokens_select(train.tokens1, stpword, 
                               selection = "remove") 
# coercion en liste
ItemSetFreq1 <- as.list(train.tokens1)
taille <- nrow(datasetPositive)
# set transaction names
names(ItemSetFreq1) <- paste("Tr",c(1:taille), sep = "")

# coerce en transactions
trans2 <- as(ItemSetFreq1, "transactions")
transactionInfo(trans2)$sequenceID <- datasetPositive$V2
transactionInfo(trans2)$eventID <- datasetPositive$V3


s1 <- cspade(trans2, parameter = list(support = 0.4), 
             control   = list(verbose = TRUE, tidLists = TRUE))




fsets1 <- eclat(trans2, parameter = list(supp = 0.001,minlen =2 , maxlen = 3))
fsets1 <- sort(fsets1)[1:800]
inspect(fsets1)



















#datasetNegative$V1<-as.character(datasetNegative$V1)

#train.tokens <- tokens(datasetNegative$V1, what = "word", 
 #                      remove_numbers = TRUE, remove_punct = TRUE,
 #                      remove_symbols = TRUE, remove_hyphens = TRUE)

#Importation du fichier stopwords.txt
#stpword <- read.delim("stopwords.txt", header = FALSE, dec = ".")
#stpword<-as.character(stpword$V1)

#On enlève les stopwords
#train.tokens <- tokens_select(train.tokens, stpword, 
 #                             selection = "remove") 


# # coercion en liste
# ItemSetFreq <- as.list(train.tokens)
# taille <- nrow(datasetNegative)
# # set transaction names
# names(ItemSetFreq) <- paste("Tr",c(1:taille), sep = "")
# 
# # coerce en transactions
# trans1 <- as(ItemSetFreq, "transactions")
# inspect(trans1)
# 
# fsets <- eclat(trans1, parameter = list(supp = 0.001,minlen =2 , maxlen = 3))
# fsets <- sort(fsets)[1:800]
# inspect(fsets)
# 
# test <-  as(items(fsets), "list")
# 
# train.tokens <- as.tokens(test, concatenator = "_")
# #Importation du dictionnaire généré sur Python
# dico <- read.table("dico/v-words.txt")
# dico <-as.character(dico$V1)
# # Premier modèle bag-of-words.
# train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
# 
# train.tokens.dfm <- dfm_lookup(train.tokens.dfm, dictionary = char2dictionary(dico))
# 










