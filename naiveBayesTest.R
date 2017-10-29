library(ggplot2)
library(e1071)

source("./NaiveBayes.R")

test <- iris

model <- naiveBayes(x = test[,1:4],y = test[,5])
predict(model,test[,1:4])  

nbparcord(iris, model)
nbparcordFacet(iris,model)

################

library(ggplot2)
library(e1071)
source("./NaiveBayes.R")

flights <- read.csv("/home/user/2008.csv",header = T, sep = ',', nrows = 500000)
flights1 <- flights
flights1 <- as.data.frame(flights1)

flights1$Delay <- 'No'
flights1[!is.na(flights1$CarrierDelay) & !is.na(flights1$WeatherDelay) & !is.na(flights1$NASDelay) & !is.na(flights1$SecurityDelay) & !is.na(flights1$LateAircraftDelay),][,30]  <- 'Yes'
flights1 <- flights1[,c("DayofMonth",
                        "DayOfWeek",
                        "CRSDepTime",
                        "CRSArrTime",
                        "CRSElapsedTime",
                        "Origin",
                        "Dest",
                        "Distance",
                        "TaxiIn",
                        "TaxiOut",
                        "Delay"
)]



flights1$Delay <- as.factor(flights1$Delay)
flights1$DayOfWeek <- as.factor(flights1$DayOfWeek)
flights1$DayofMonth <- as.factor(flights1$DayofMonth)
flights1$CRSDepTime <- as.factor(flights1$CRSDepTime)
flights1$CRSArrTime <- as.factor(flights1$CRSArrTime)
flights1 <- na.omit(flights1)

min <- min(nrow(flights1[flights1$Delay=="No",]), nrow(flights1[flights1$Delay=="Yes",]))
flights1 <- rbind(flights1[flights1$Delay=="No",][1:min,], flights1[flights1$Delay=="Yes",][1:min,] )
nrow(flights1[flights1$Delay=="No",])
nrow(flights1[flights1$Delay=="Yes",])

model <- naiveBayes(x = flights1[,1:10], y = flights1[,11])

nbparcordFacet(flights1, model, 0.01)


#############################################################################################################
## data cleaning from https://www.kaggle.com/yildirimarda/titanic/decision-tree-visualization-submission/code
## age prediction with random forest

library(ggplot2)
library(e1071)
library(randomForest)
source("./NaiveBayes.R")
source("./Titanic.R")

model <- naiveBayes(x = titanic[,-2], y = titanic[,2])

nbparcordFacet(titanic, model,0.2)



