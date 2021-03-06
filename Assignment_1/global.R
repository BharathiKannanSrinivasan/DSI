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

dataset <- read.csv("Ass1Data.csv", header = TRUE)
dataset$Date=as.Date(dataset$Date)
dataset$Priority=factor(dataset$Priority,ordered=TRUE,levels=c("Low","Medium","High"))
dataset$Price=factor(dataset$Price,ordered=TRUE,levels=c("Cheap","Costly","Extravagant"))
dataset$Speed=factor(dataset$Speed,ordered=TRUE,levels=c("Slow","Medium","Fast"))
dataset$Duration=factor(dataset$Duration,ordered=TRUE,levels=c("Short","Long","Very Long"))
dataset$Temp=factor(dataset$Temp,ordered=TRUE,levels=c("Cold","Warm","Hot"))
dataset$Agreed=as.factor(dataset$Agreed)
dataset$State=as.factor(dataset$State)
discrete_columns <- colnames(as.data.frame(dataset))
discrete_columns <- discrete_columns[c(2,3,5,6,7,8,9,10,11,12,13,14)]
continuous_columns <- colnames(as.data.frame(dataset))
continuous_columns <- continuous_columns[c(1,15:44)]
mosaic_columns <- colnames(as.data.frame(dataset))
mosaic_columns <- mosaic_columns[c(3,5,6,7,8,9,10,11,12,13,14)]
cols <- c(1,15:44)
box_plot_columns <- dataset[,cols]

spearman<-as.data.frame(dataset[c(35,39,37,44,40,42,43)])
spearman2<-as.data.frame(dataset[c(24,22,16,19,15,23,20,21)])
spearman3<-as.data.frame(dataset[c(32,34,33,25,29,30,28,26)])

pearson<-as.data.frame(dataset[c(37,35,39,42,43,40,44)])
pearson2<-as.data.frame(dataset[c(36,41,38,19,17,31,27)])
pearson3<-as.data.frame(dataset[c(32,26,28,34,33,25,29,30)])
pearson4<-as.data.frame(dataset[c(24,21,20,22,16,15,23,19)])

kendall<-as.data.frame(dataset[c(37,39,35,44,42,40,43)])
kendall2<-as.data.frame(dataset[c(31,25,28,29,34,26,32,33)])
kendall3<-as.data.frame(dataset[c(24,20,22,16,23,15,17,18)])

