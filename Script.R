library("quanteda")

chemin0 <-"Datasets/reviews/"

vect_class <- c("N","P")
dataset <- data.frame()
test<-data.frame()
# 
# test <- read.delim("Datasets/reviews/N/n_cv100_12406.txt", header = FALSE, dec = ".")
# test$V1<-as.character(test$V1)
# test<-rbind.data.frame(test2,paste(test$V1,collapse = " "))
# 
#  names(test2) <- c("text","class")
# head(test2)

#Extraction
for(val1 in vect_class){
  chemin1 <-paste0(chemin0,val1)
  chemin2<-paste0(chemin1,"/")
  print(val1)
  
  dataset1 <- data.frame()
  
  
  for(fich in dir(path=chemin2, pattern="*.txt$", recursive=TRUE)){
    
    print(fich)
    res =  read.delim(paste0(chemin2,fich), header = FALSE, dec = ".")
    res$V1<-as.character(res$V1)
    tmp<-paste(res$V1,collapse = " ")
    
    test<-rbind(test,tmp)
    test<- cbind(test,val1)
    names(test) <- c("text","class")
    dataset1 <- rbind.data.frame(dataset1,test)
    test<-data.frame()
  }
  dataset <- rbind(dataset,dataset1)
  dataset1 <- data.frame()
  
}
dataset$text<-as.character(dataset$text)
View(dataset)
rm(res,dataset1,test,chemin0,chemin2,chemin1,fich,tmp,val1,vect_class)
str(dataset)

#Preprocessing
# Tokénisation de ma dataset
train.tokens <- tokens(dataset$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)


# Tout mettre en miniscule
train.tokens <- tokens_tolower(train.tokens)



#Utilisation du dictionnaire de quanteda (en anglais). Mais il faudra utiliser notre
#propre dictionnaire.
#On enlève les stopwords
train.tokens <- tokens_select(train.tokens, stopwords(), 
                              selection = "remove") 

# Pour faire du stemming.
#On ferra une comparaison après des résultats avec ou sans stemming
train.tokens <- tokens_wordstem(train.tokens, language = "english")



# Premier bag-of-words modèle.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


# Transformation en matrix pour investigation.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)


# Investigations des effets du stemming
colnames(train.tokens.matrix)[1:50]

