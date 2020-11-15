#import datasets
library(readr)
benign <- read_csv("~/NCI/Academic Internship/benign.csv")
malware <- read_csv("~/NCI/Academic Internship/malware.csv")



#memory.limit(size=56000) #www.researchgate.net/post/How_to_solve_Error_cannot_allocate_vector_of_size_12_Gb_in_R
#data<- rbind(benign, malware) #combine both dataframes


#remove duplicate rows
library(dplyr)
cleanedmalware <- malware%>%distinct()
cleanedbenign<-benign%>%distinct()

#combine datasets
library(tidyverse)
data<- rbind(cleanedbenign, cleanedmalware)


##########reducing 
#use the malware dataset to generate a matrix of permissions requested by malicious apps

#### install needed packages
install.packages("caTools")
library(caTools)
install.packages("RWeka")
library(RWeka)

#split data into training and testing subsets
spl=sample.split(data$Mal_Ben, SplitRatio = 0.7)
TrainingData = subset(data, spl==TRUE)
TestingData = subset(data, spl==FALSE)

#change Mal_Ben variable to string
CleanedTrainingData <- lapply(TrainingData$Mal_Ben, as.factor)

#create a J48 decision tree
DecisionTree <- J48(`Mal_Ben`~., data=TrainingData)
