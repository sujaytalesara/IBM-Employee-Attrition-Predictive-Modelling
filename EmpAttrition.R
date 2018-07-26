#library(colorspace)
#library(grid)
#library(data.table)
#library(VIM)
#library(RColorBrewer)
#library(rattle)
#library(lattice)
#install.packages("shiny_plotRO")

#Load Library
library(readxl) # to read data from excel file
library(rpart)
library(caret)
library(caTools)
library(ggplot2)
library(Matrix)
library(ROCR)
library(pROC)
library(plotROC)
library(randomForest)
#library(RColorBrewer)
#library(lattice)
#library(rattle)
#library(mlbench)
#library(caretEnsemble)



#Data Examination -------------------------------------------------------------------------
Employeeattrition = read_excel('Employeeattrition.xlsx')
data.frame <- data.frame(Employeeattrition)


dim(data.frame) #Calculating no of features and row
sapply(data.frame,class) #See the type of variables
summary(data.frame)

agg <- aggr(data.frame) #Missing data : No missing data found
summary(agg)
#---another method : colSums(sapply(data.frame, is.na))



#Encoding Categorical Variables -----------------------------------------------------------
data.frame <- data.frame[-22]
data.frame <- as.data.frame(unclass(data.frame))

sparse_matrix <- sparse.model.matrix(Attrition ~ .-1, data = data.frame)



# Training and Splitting data.frame -----------------------------------------------------------
set.seed(75)
split = sample.split(data.frame$Attrition, SplitRatio = 0.80)
train = subset(data.frame, split == TRUE)
test = subset(data.frame, split == FALSE)



# Decision Tree ------------------------------------------------------------------------------
fit.rpart <- rpart(formula = Attrition ~ ., data = train, cp=0.021053)
fit.rpart$variable.importance
#plot(fit) rpart.prp

fancyRpartPlot(fit.rpart,palettes=c("PuRd"),type=1)
#main = "Classification Tree of Employee Attrition"
printcp(fit.rpart)
text(fit.rpart)

#Predict Test result
pred.rpart <- predict(fit.rpart,test,type = 'class')
predProbrpart <- predict(fit.rpart,test,type = 'prob')
#Confusion Matrix
confusionMatrix(test$Attrition,pred.rpart)
cm.rpart = table(test$Attrition,pred.rpart)
print(cm.rpart)

#Roc 
plot(roc(test$Attrition,predProbrpart[,2], direction="<"),col="blue")

predics<-prediction(predProbrpart[,2],test$Attrition)
perf<-performance(predics, "tpr","fpr")
plot(perf,col="blue")
auc<-performance(predics,"auc")@y.values
print(auc)




# Ensemble technique 1 Random Forest -----------------------------------------------------
#RandomForest<-train(Attrition ~ ., data = train, method='rf',trControl=control, tuneLength=3)

RandomForest <- randomForest(Attrition ~ ., data=train,trControl=control, 
                        importance=TRUE, ntree=100, mtry= 24)

# Prediction ------------------------
predRandom <- predict(RandomForest,test,type = 'class')
predProbsRandom <- predict(RandomForest,test,type = 'prob')

# Accuracy ---------------------------
confusionMatrix(predRandom,test$Attrition)
plot(RandomForest)

# ROC Curve -------------------------
plot(roc(test$Attrition,predProbsRandom[,2], direction="<"),col="red")

predics<-prediction(predProbsRandom[,2],test$Attrition)
perf<-performance(predics, "tpr","fpr")
plot(perf,col="blue")
auc<-performance(predics,"auc")@y.values
print(auc)




# Ensemble technique 2 Gradient Boosting -----------------------------------------------------------------
control <- trainControl(method="repeatedcv", number=10, repeats=3)
GradientBoost <- train(Attrition ~ ., data=train, method="gbm",
                 metric="Accuracy", trControl=control, verbose=FALSE)
# Prediction --------------
predProbGradient <- predict(GradientBoost ,test,type ='prob')
predGradient <- predict(GradientBoost,test)
# Confusion Matrix ---------
confusionMatrix(predGradient,test$Attrition)
# Plot ---------------------
plot(GradientBoost)


# ROC -----------------------------------------------------------------
plot(roc(test$Attrition,predProbsRandom[,2], direction="<"),col="red")

predics<-prediction(predProbGradient[,2],test$Attrition)
perf<-performance(predics, "tpr","fpr")
plot(perf,col="blue")
auc<-performance(predics,"auc")@y.values
print(auc)




ggplot(data=test,aes(x=test$Attrition,y=predRandom[,1]))+geom_boxplot()




# Plots ----------------------------------------------------------------------------------
Attrition <- data.frame$Attrition
Total_Working_years <- data.frame$TotalWorkingYears
Job_Involvement <- data.frame$JobInvolvement
Income <- data.frame$MonthlyIncome

ggplot(data.frame,aes(data.frame$TotalWorkingYears,..count..)) + 
  geom_bar(aes(fill=Attrition))+ xlab("Total Working Years") + ylab("Frequency") +
  ggtitle("Distribution of Total Working Years v/s Target Value : Attrition") +
  scale_size_area()

ggplot(data.frame, aes(x = factor(JobRole),y = 100 * (..count..)/sum(..count..))) +
  geom_bar(aes(fill = Attrition),position='dodge') +
  xlab("Job Role") + ylab("Frequency") + theme(axis.text.x=element_text(angle=55,hjust=1)) +
  #stat_bin(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 5) +
  #scale_y_continuous(labels = percent) +
  ggtitle("Distribution of Job Role v/s Target Value : Attrition")

library(scales)
ggplot(data.frame,aes(x = factor(JobRole), fill = factor(Attrition), 
                   y = (..count..)/sum(..count..))) +
  geom_bar() +
  stat_bin(geom = "text",
           aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 5) +
  scale_y_continuous(labels = percent)


ggplot(data.frame, aes(x = data.frame$Age)) +
  geom_bar(aes(fill=Attrition),position="dodge") + xlab("Employee Age") + ylab("Frequency") +
  ggtitle("Distribution of Age v/s Target Value : Attrition")


#ggplot(data.frame, aes(x = data.frame$Attrition, y = data.frame$TotalWorkingYears, color=data.frame$Attrition))
#+ geom_point(alpha = 0.4) + geom_smooth(se=FALSE, method='lm')

#ggplot(data.frame, aes(x = data.frame$TotalWorkingYears, y = '',xlab = 'dfd',
#                    fill=data.frame$Attrition)) + geom_bar(stat ='identity')


fit.treebag <- train(Attrition~., data=train, method="treebag", 
                     metric=metric, trControl=control)
str(train)
fit.rf <- train(train$Attrition~., data=train, method="rf", 
                metric=metric, trControl=control)

#C5.0
fit.c50 <- train(Attrition ~ ., data=train, method="C5.0", metric="Accuracy", 
                 trControl=control)
