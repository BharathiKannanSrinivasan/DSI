library(shiny)
library(shinyjs)
library(vcd)
library(MASS)
library(RColorBrewer)
library(datasets)
library(corrgram)
library(visdat)
library(forecast)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(wordcloud2)
library(reshape2)
library(summarytools)
library(pls)
library(ggplot2)
library(tabplot)  
library(visNetwork)
library(ffbase)
library(leaflet)
library(skimr)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)
library(caret)
library(rpart.plot)
library(rpart)
library(plotly)
library(modeldata)
library(recipes)
library(dplyr)
library(gbm)
library(ggrepel)
library(shinydashboard)
library(shinydashboardPlus)
library(randomForest, quietly=TRUE)

dataset <- read.csv("Ass2Data.csv", header = TRUE, na.strings = c(""))
dataset[dataset==-99]<-NA
dataset[dataset=="--"]<-NA
dataset[dataset=="na"]<-NA
dataset[dataset=="N/A"]<-NA


dataset$POPULATION=as.double(dataset$POPULATION)
dataset$AGE25PROP=as.double(dataset$AGE25PROP)
dataset$AGEMEDIAN=as.double(dataset$AGEMEDIAN)
dataset$AGE55PROP=as.double(dataset$AGE55PROP)
dataset$POPDENSITY=as.double(dataset$POPDENSITY)
dataset$GDP2019=as.double(dataset$GDP2019)
dataset$INFANTMORT=as.double(dataset$INFANTMORT)
dataset$DOC10=as.double(dataset$DOC10)
dataset$VAXRATE=as.double(dataset$VAXRATE)
dataset$HEALTHCARE_COST=as.double(dataset$HEALTHCARE_COST)

dataset$HEALTHCARE_COST_SHADOW <- as.numeric(is.na(dataset$HEALTHCARE_COST)) # create a shadow variable

for(i in 1:length(dataset$HEALTHCARE_BASIS)){
  if(dataset$HEALTHCARE_BASIS[i]=="FREE"){
    dataset$HEALTHCARE_COST[i]<-0
  }
}
dataset[dataset=="NA"]<-NA

dataset$GOVERNMENT=as.factor(dataset$GOVERNMENT)
dataset$HEALTHCARE_BASIS=as.factor(dataset$HEALTHCARE_BASIS)

dataset3<-na.omit(dataset)
histdata<-dataset[c(3:11,13,14)]


# Data Cleaning
pMiss <- function(x){ sum(is.na(x))/length(x)*100 }

# Variable Cleaning
threshold <- 55
d <- dataset
cRatio <- apply(X = d, MARGIN = 2, FUN = pMiss) 
cat("Variables to remove:", paste(colnames(d)[cRatio >= threshold], collapse = ","))

# Observation cleaning
threshold <- 50
d <- dataset
rRatio <- apply(X = d, MARGIN = 1, FUN = pMiss)  
cat("Observations to remove (First 50) :", paste(head(rownames(d)[rRatio >= threshold], n = 50), collapse = ", "))

# Removing variables
cRatio <- apply(d,2,pMiss)
d <- d[, cRatio < 55]
# Removing observations
rRatio <- apply(d,1,pMiss)
d <- d[rRatio < 50, ]
cat("Final data dimensions are: ",dim(d))

dataset<-d
continuous_columns <- dataset[,c(3:10,13,14)]



dataset$MISSINGNESS <- apply(X = is.na(dataset), MARGIN = 1, FUN = sum)

tree <- train(MISSINGNESS ~ . -COUNTRY, data = dataset, method = "rpart", na.action = na.rpart)

rpart.plot(tree$finalModel, main = "TUNED: Predicting the number of missing variables in an observation", roundint = TRUE, clip.facs = TRUE)

dim(dataset)
set.seed(50)
subIndex <- caret::createDataPartition(y = dataset$DEATHRATE, p = 0.7, list = FALSE)

train <- dataset[subIndex,]

test <- dataset[-subIndex,]


rec <- recipes::recipe(DEATHRATE ~., data = train) %>%
  
  update_role("COUNTRY", new_role ="COUNTRY") %>% #id is not a predictor
  
  step_knnimpute(all_predictors(), neighbours = 5) %>%
  
  step_center(all_numeric(), -has_role("outcome")) %>%
   
  step_scale(all_numeric(), -has_role("outcome")) %>%
  
  step_dummy(GOVERNMENT,HEALTHCARE_BASIS)

summary(rec)
train

model <- caret::train(rec, data = train, method = "glmnet")

pred = predict(model,test)

residuals <- test$DEATHRATE - pred

mean(residuals^2)
residuals


