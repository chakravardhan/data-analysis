
library(data.table)
library(magrittr)
library(stringr)
library(plyr)
library(klaR)
library(cluster)
library(fpc)
library(cba)



mydata <- read.csv("cleaneddata14_16_v2.csv")
mydata1 <- read.csv("cleaneddata14_16_v2.csv")
attach(mydata)
used_attributes <- c("mental_health_consequence","phys_health_consequence","coworkers","tech_company")

mydata <- mydata [,names(mydata) %in% used_attributes ]
#mydata$Age = as.numeric(as.character(mydata$Age)) 
# Removing the outliers in Age
#replacing_rows = which(mydata$Age > 99 | mydata$Age < 1)
#set(mydata,replacing_rows,"Age",NA)
sapply(mydata,function(x){ sum(is.na(x)) })
mydata<- mydata[complete.cases(mydata),]
#sapply(mydata1,function(x){ sum(is.na(x)) })
mydata1 <- mydata1[complete.cases(mydata),]
mydata$Age <- as.integer(mydata$Age)
count(mydata)
mydata$treatment <- as.integer(mydata$treatment)
plot(mydata)
mycluster <- kmeans(mydata$mental_illness,3,nstart = 20)
kmodes_cluster <- kmodes(mydata,2,iter.max = 10,weighted = FALSE)
krock_cluster <- rockCluster(mydata,2)

#plot(mydata1,col=kmodes_cluster$cluster,cex=1,pch=1)

#plotcluster(mydata1,kmodes_cluster$cluster)

#temp<-mydata[which(mydata$tech_company=="Yes" & mydata$mental_health_consequence=="No" & mydata$phys_health_consequence=="No" & mydata$coworkers =="Yes"),]

new <- cbind(mydata1,cluster_ouput = kmodes_cluster$cluster)
#str(new)
write.csv(new,file="tc_mhc_phc_cw.csv")
