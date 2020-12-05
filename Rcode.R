#import datasets
library(readr)
benign <- read_csv("~/NCI/Academic Internship/benign.csv")
malware <- read_csv("~/NCI/Academic Internship/malware.csv")

memory.limit(size=56000) #www.researchgate.net/post/How_to_solve_Error_cannot_allocate_vector_of_size_12_Gb_in_R



#remove duplicate rows
library(dplyr)
cleanedmalware <- malware%>%distinct()
cleanedbenign <- benign%>%distinct()
rm(malware)
rm(benign)

#combine datasets
library(tidyverse)
names(cleanedmalware)[names(cleanedmalware) == "Total Permission"] <- "Total Permissions"
randombenign<-cleanedbenign[sample(nrow(cleanedbenign),8759),]

data<- rbind(randombenign, cleanedmalware)

#export data 
write.csv(data, "C:/Users/louis/OneDrive/Documents/NCI/Academic Internship/finaldata.csv")

#remove any whitespaces

cleaneddata <- data %>% mutate(across(where(is.character), str_trim))
cleanedmalware <- cleanedmalware %>% mutate(across(where(is.character), str_trim))
cleanedbenign <- cleanedbenign %>% mutate(across(where(is.character), str_trim))
rm(data)


#fix Mal_Ben column
cleaneddata<- lapply(cleaneddata, gsub, pattern = "Benign", replacement = "0", fixed = TRUE)
cleaneddata <- as.data.frame(cleaneddata)
cleaneddata<- lapply(cleaneddata, gsub, pattern = "Malicious", replacement = "1", fixed = TRUE)
cleaneddata <- as.data.frame(cleaneddata)
#cleaneddata<- sapply(cleaneddata, function(x) {x<- gsub("Benign", 0, x)}) CHANGES DF TO MATRIX????
#cleaneddata<- sapply(cleaneddata, function(x) {x<- gsub("Malicious", 1, x)})



#####################################
#create bar chart
library(ggplot2)
#create random samples of data for more accuate comparision
set.seed(767)
randommalware<-cleanedmalware[sample(nrow(cleanedmalware), 5000),]
randombenign<-cleanedbenign[sample(nrow(cleanedbenign), 5000),]

#malware
ggplot(data=randommalware, aes(x=`Total Permissions`, fill =..count..)) +
  geom_bar(width = 0.9) + ggtitle("Count of Malware Permissions")+ expand_limits( y=c(0, 40,000) )
  scale_fill_gradient2(low='red', high='darkgreen') +
  theme(axis.text.x=element_blank()) 
#benign
ggplot(data=randombenign, aes(x=`Total Permissions`, fill =..count..)) +
  geom_bar(width = 0.9) + ggtitle("Count of Benign Permissions") + 
  theme(axis.text.x=element_blank()) 
 +geom_bar(fill="blue")

## using base https://www.r-bloggers.com/2014/06/making-back-to-back-histograms/
h1 = hist(cleanedbenign$`Total Permissions`, plot=FALSE)
h2 = hist(cleanedmalware$`Total Permissions`, plot=FALSE)
h2$counts = - h2$counts
hmax = max(h1$counts)
hmin = min(h2$counts)
X = c(h1$breaks, h2$breaks)
xmax = max(X)
xmin = min(X)
plot(h1, ylim=c(hmin, hmax), col="green", xlim=c(xmin, xmax))
lines(h2, col="blue")
title(main="main title")

#########DISREGARD - EATS MEMORY
#feature selection forward stepwise regression
#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
#library(tidyverse)
#library(caret)
#library(leaps)
#library(MASS)
##set.seed(123) #ensure reproduceability
##train.control <- trainControl(method = "cv", number = 10)
##step.model <- train(cleaneddata$Mal_Ben ~., data=cleaneddata, method = "leapForward", tuneGrid =data.frame(nvmax = 1:1440), trControl =train.control)

#feature selection using genetic algorithm
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)
library(GenAlgo)
set.seed(234)
test <- Boruta(Mal_Ben ~ ., data = finalbalanceddataxlsx, doTrace = 2)
print(test)
getSelectedAttributes(test, withTentative = F)
borutadata <-attStats(test)
#subset just the confirmed important attributes
importantattributes1 <- subset(borutadata, decision == "Confirmed")

##############MODEL
#random forest
library(randomForest)
library(e1071)
#split data into training and testing set 
trainingdata<-
#kfold cross validation
trControl<- trainControl(method = "cv", number = 10, search = "grid")
#convert Mal_Ben to a factor
finalbalanceddataxlsx$Mal_Ben<-as.factor(finalbalanceddataxlsx$Mal_Ben)
#model
randomforest <- train(Mal_Ben~., data=finalbalanceddataxlsx,method = "rf", metric = "Accuracy", trControl=trControl)


