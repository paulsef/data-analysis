load("./rawData/samsungdata.rda")

#data exploration and training set comparison to ensure proper partitioning
#no missing values
sum(is.na(samsungData))
#column names
names(samsungData)
#some names are not unique
length(unique(names(samsungData)))
#change variable names so they are unique
samsungData <- data.frame(samsungData)
#variable types
str(samsungData)
#change subject from numeric to factor,
#change activity from character to factor
samsungData$subject <- as.factor(samsungData$subject)
samsungData$activity <- as.factor(samsungData$activity)

#create test set, write to file, remove fom workspace
samsung_training <- subset(samsungData, subject == 1 |
                             subject == 3 |
                             subject == 5 |
                             subject == 6 |
                             subject == 7 |
                             subject == 8)
samsung_training <- samsung_training[,c(1:561, 563)]
samsung_validation <- subset(samsungData, subject == 11 |
                               subject == 14 |
                               subject == 15 |
                               subject == 16)
samsung_validation <- samsung_validation[,c(1:561, 563)]
samsung_test <- subset(samsungData, subject == 27 |
                         subject == 28 |
                         subject == 29 | 
                         subject == 30)
write.csv(samsung_test, file = "./samsungtestdata.csv")
rm(samsung_test)

#bagging trees, never run this again
#library(ipred)
#bagTree <- bagging(activity ~., data = samsung_training, coob = TRUE)

#random forests TRY EXCLUDING SUBJECT THAT MIGHT BE TOTALLY NECCESSARY 
library(randomForest)
samsung_forest <- randomForest(activity ~ ., data = samsung_training, prox = TRUE)
#validation set has an 85 % success rate
predictions <- predict(samsung_forest, samsung_test)
(sum(predictions != samsung_test$activity)/(length(predictions)))*100


important <- samsung_forest$importance
important <- important[order(important[,1], decreasing = TRUE),]
important <- as.matrix(important)
plot(samsung_training[,rownames(important)[1]],
     samsung_training[,rownames(important)[3]],
     col = samsung_training$activity,
     xlab = rownames(important)[1],
     ylab = rownames(important)[3])

library(caret)
prediction_frame <- data.frame("actual" = samsung_test$activity, 
                               "prediction" = predictions)
confusion_matrix <- confusionMatrix(prediction_frame$prediction,
                                    prediction_frame$actual)

                       
pdf(file="../finalfigures.pdf", height=4, width=3*4)

mypar <- function(a=1,b=1,brewer.n=8,brewer.name="Dark2",...){
  par(mar=c(2.5,2.5,1.6,1.1),mgp=c(1.5,.5,0))
  par(mfrow=c(a,b),...)
  palette(brewer.pal(brewer.n,brewer.name))
}
mypar(mfrow = c(1,2))
plot(samsung_training[,rownames(important)[3]],
     samsung_training[,rownames(important)[1]],
     col = samsung_training$activity,
     xlab = rownames(important)[1],
     ylab = rownames(important)[3],
     main = "First and Second Most Important Variables")

percents <- data.frame("actual" = c("laying", "sitting","standing","walk","waldown","walkup"),
                       "total mistakes" = c(0,48,34,13,12,14),
                       "laying" = c(0,0,0,0,0,0),
                       "sitting" = c(0,0,48,0,0,0),
                       "standing" = c(0,34,0,0,0,0),
                       "walking" = c(0,0,0,0,4,9),
                       "downstairs" = c(0,0,0,2,0,11),
                       "upstairs" = c(0,0,3,0,11,0))
barplot(as.matrix(percents[,3:8]), 
        col = percents$actual, 
        legend = percents$actual,
        xlab = "Actual Activity", 
        ylab = "Number of Errors",
        main = "Error Identification")
