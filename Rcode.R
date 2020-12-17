install.packages(c("readr", "dplyr","ggplot2", "boruta", "caret", "randomforest", "c50"))
library(c("readr", "dplyr","ggplot2", "boruta", "caret", "randomforest", "c50"))

#import datasets
library(readr)
benign <- read_csv("~/NCI/Academic Internship/benign.csv")
malware <- read_csv("~/NCI/Academic Internship/malware.csv")

memory.limit(size=56000) #www.researchgate.net/post/How_to_solve_Error_cannot_allocate_vector_of_size_12_Gb_in_R

#remove duplicate rows
cleanedmalware <- malware%>%distinct()
cleanedbenign <- benign%>%distinct()
rm(malware)
rm(benign)
#combine datasets
names(cleanedmalware)[names(cleanedmalware) == "Total Permission"] <- "Total Permissions"
#sample benign dataset to evenly weight final dataset
randombenign<-cleanedbenign[sample(nrow(cleanedbenign),8759),]

data<- rbind(randombenign, cleanedmalware)

#export data 
write.csv(data, "C:/Users/louis/OneDrive/Documents/NCI/Academic Internship/finaldata.csv")

#####clean final dataset
#remove any whitespaces
cleaneddata <- data %>% mutate(across(where(is.character), str_trim))
cleanedmalware <- cleanedmalware %>% mutate(across(where(is.character), str_trim))
cleanedbenign <- cleanedbenign %>% mutate(across(where(is.character), str_trim))

finaldata <- finaldata %>% mutate(across(where(is.character), str_trim))


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

#########################
#feature selection using genetic algorithm
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)
library(GenAlgo)

#import datasets that have been edited in excel
finalbalanceddataxlsx <- read_csv("~/NCI/Academic Internship/finalbalanceddataxlsx.csv")
finaldata <- read_csv("~/NCI/Academic Internship/finaldata.csv")

#extract relevant features from 'Permissions only' dataset
set.seed(234)
test <- Boruta(Mal_Ben ~., data = finalbalanceddataxlsx, doTrace = 2)
print(test)
getSelectedAttributes(test, withTentative = F)
borutadata <-attStats(test)
#export boruta data
write.csv(data, "C:/Users/louis/OneDrive/Documents/NCI/Academic Internship/borutadata.csv")


#extract relevant features from entire dataset
#convert Mal_Ben to a factor
finaldata$Mal_Ben<-as.factor(finaldata$Mal_Ben)
rm(finaldata)
finaldata<-finaldata[,-1]
testALLdata <- Boruta(Mal_Ben~. , data = finaldata, maxRuns = 40, doTrace = 2)
print(testALLdata)
getSelectedAttributes(testALLdata, withTentative = F)
borutaALLdata <-attStats(testALLdata)
#export boruta data



write.csv(borutadata, "C:/Users/louis/OneDrive/Documents/NCI/Academic Internship/borutadata.csv")
#subset just the confirmed important attributes
importantattributes1 <- subset(borutadata, decision == "Confirmed")

##############MODEL
#random forest
library(randomForest)
library(e1071)

memory.limit(size=56000)
#convert Mal_Ben to a factor
finalbalanceddataxlsx$Mal_Ben<-as.factor(finalbalanceddataxlsx$Mal_Ben)


importantattributes <- subset(borutadata, decision == "Confirmed")

summary(finalbalanceddataxlsx)

fbdpermssionsonly <- finalbalanceddataxlsx[, -(1:3)]

trainingdata <- sample(nrow(fbdpermssionsonly), 0.7*nrow(fbdpermssionsonly), replace = FALSE)
TrainingSet <-fbdpermssionsonly[trainingdata,]
ValidationSet <-fbdpermssionsonly[-trainingdata,]

summary(TrainingSet)

#model1 - all permissions/random forest
model1 <- randomForest(Mal_Ben~., data = TrainingSet, importance=TRUE)
model1
#train models
model1a <- randomForest(Mal_Ben~., data = TrainingSet, ntree=1000, mtry=10, importance=TRUE)
model1a
model1b <- randomForest(Mal_Ben~., data = TrainingSet, ntree=1000, mtry=20, importance=TRUE)
model1b
model1c <- randomForest(Mal_Ben~., data = TrainingSet, ntree=1500, mtry=20, importance=TRUE)
model1c
#test models (using caret library)
#model1test
predictionm1 <- predict(model1, ValidationSet)
confusionMatrix(predictionm1, ValidationSet$Mal_Ben)
predictionm1c <- predict(model1c, ValidationSet)
confusionMatrix(predictionm1c, ValidationSet$Mal_Ben)
table(Truth = ValidationSet$Mal_Ben, Prediction = predict(model1c, ValidationSet))

#model2 - boruta permissions/random forest
balanceddatabortrutaimportant$Mal_Ben<-as.factor(balanceddatabortrutaimportant$Mal_Ben)
trainingdataboruta <- sample(nrow(balanceddatabortrutaimportant), 0.7*nrow(balanceddatabortrutaimportant), replace = FALSE)
TrainingSetboruta <-balanceddatabortrutaimportant[trainingdataboruta,]
ValidationSetboruta <-balanceddatabortrutaimportant[-trainingdataboruta,]
#train models
model2 <- randomForest(Mal_Ben~., data = TrainingSetboruta,  importance=TRUE)
model2
model2a <- randomForest(Mal_Ben~., data = TrainingSetboruta, ntree=1000, mtry=10, importance=TRUE)
model2a
model2b <- randomForest(Mal_Ben~., data = TrainingSetboruta, ntree=1000, mtry=20, importance=TRUE)
model2b
model2c <- randomForest(Mal_Ben~., data = TrainingSetboruta, ntree=1500, mtry=20, importance=TRUE)
model2c

#test models (using caret library)
#model2test
predictionm2 <- predict(model2, ValidationSetboruta)
confusionMatrix(predictionm2, ValidationSetboruta$Mal_Ben)
predictionm2c <- predict(model2c, ValidationSetboruta)
confusionMatrix(predictionm2c, ValidationSetboruta$Mal_Ben)
table(Truth = ValidationSetboruta$Mal_Ben, Prediction = predict(model2c, ValidationSetboruta))

#model3 - all permissions/C5.0
library(RWeka)
library(printr)
library(C50)

c50modelALL <- C5.0(Mal_Ben~., data=TrainingSet)
#test
results <- predict(object = c50modelALL, newdata = ValidationSet, type = "class")
#confusionmatrix
table(results, ValidationSet$Mal_Ben)
plot(c50modelALL)
table(Truth = ValidationSet$Mal_Ben, Prediction = predict(c50modelALL, ValidationSet))

#model 4 - Boruta permissions/C5.0
c50boruta <-C5.0(Mal_Ben~., data=TrainingSetboruta)
#test
resultsboruta<- predict(object = c50boruta, newdata = ValidationSetboruta, type = "class")
#confusionmatrix
table(resultsboruta, ValidationSetboruta$Mal_Ben)
plot(c50boruta)
table(Truth = ValidationSetboruta$Mal_Ben, Prediction = predict(c50boruta, ValidationSetboruta))



