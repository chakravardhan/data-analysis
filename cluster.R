
library(data.table)
library(magrittr)
library(stringr)
library(plyr)
library(klaR)
library(cluster)
library(fpc)



mydata <- read.csv("data14_16.csv")
attach(mydata)
used_attributes <- c("Age","Country")

mydata <- mydata [,names(mydata) %in% used_attributes ]
mydata$Age = as.numeric(as.character(mydata$Age)) 
# Removing the outliers in Age
replacing_rows = which(mydata$Age > 99 | mydata$Age < 1)
set(mydata,replacing_rows,"Age",NA)
sapply(mydata,function(x){ sum(is.na(x)) })
mydata<- mydata[complete.cases(mydata),]
sapply(mydata,function(x){ sum(is.na(x)) })

mydata$Age <- as.integer(mydata$Age)

mydata$treatment <- as.integer(mydata$treatment)
plot(mydata)
mycluster <- kmeans(mydata$Age,3,nstart = 20)
kmodes_cluster <- kmodes(mydata,3,iter.max = 10,weighted = FALSE)
plot(mydata,col=kmodes_cluster$cluster,cex=1,pch=1)
plotcluster(mydata,kmodes_cluster$cluster)

new <- cbind(mydata,cluster_output=kmodes_cluster$cluster)
