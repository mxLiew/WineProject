#Classification
library(tidyverse) 
library(cluster) 
library(factoextra)
library(ggplot2)
library(readr)
library(caret)
library(e1071)
library(MASS)
library(lattice)
library(plotly)
library(ggparallel)
library(ggfortify)
library(stats)
library(stargazer)
##import data
redwine<-read_delim("~/Desktop/BT2101 project/winequality-red.csv", ";", escape_double = FALSE, trim_ws = TRUE)
whitewine <- read_delim("~/Desktop/BT2101 project/winequality-white.csv",";", escape_double = FALSE, trim_ws = TRUE)

#first part, differentating and classification between red & white wine
winequality_red<-redwine
winequality_white<-whitewine
winequality_red$type<-factor("red")
winequality_white$type<- factor("white")
wine_combined <- rbind(winequality_red,winequality_white)
wine_combined <- wine_combined[complete.cases(wine_combined),]
attach(wine_combined)
cols <- character(nrow(wine_combined))
cols[] <- "black"
cols[wine_combined$type=="white"] <- "blue"
cols[wine_combined$type=="red"] <- "red"
parallelplot(wine_combined,col = cols)
pairs( ~ `fixed acidity`+`volatile acidity` + `sulphates`+chlorides, data = wine_combined ,col=cols)
plot_ly(wine_combined,x=~`volatile acidity`,y=~`fixed acidity` ,z=~sulphates,color = ~type,marker = list(size = 2),colors = c("red","blue"))

## Partitioning training and test data 7:3
set.seed(123)
rnd <- sample(2, nrow(wine_combined), replace = TRUE, prob = c(0.3, 0.7))
wine_train <- wine_combined[rnd ==2,]
wine_test <- wine_combined[rnd ==1,]

#### linear SVM
traincontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
wine_svm_linear <- train(type~ `fixed acidity`+`volatile acidity` + `sulphates`, data = wine_train, method = "svmLinear", trControl= traincontrol,preProcess = c("center", "scale"), tuneLength = 10)
#all 11 properties
wine_svm_linear2 <- train(type~ `fixed acidity`+`volatile acidity` + `sulphates`+ `citric acid`
                          +`residual sugar`+chlorides+`free sulfur dioxide`+`total sulfur dioxide`+density
                          +pH+alcohol, data = wine_train, method = "svmLinear", trControl= traincontrol,preProcess = c("center", "scale"), tuneLength = 10)

wine_svm_linear
wine_svm_linear2
test_predict_linear <- predict(wine_svm_linear, newdata = wine_test)
confusionMatrix(test_predict_linear,wine_test$type)
test_predict_linear2 <- predict(wine_svm_linear2, newdata = wine_test)
confusionMatrix(test_predict_linear2,wine_test$type)
