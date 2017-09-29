##Zhila Esna Ashari Esfahani
##Milliman Exercise
################################
library("e1071")
install.packages("dummies")
library("dummies")
library(ade4)

##read input files
trainig_data<- read.csv(file="Data Set - Modeling Exercise - Train.csv", header = TRUE, sep = ",")
testing_data <- read.csv(file="Data Set - Modeling Exercise - Test.csv", header = TRUE, sep = ",")

##selecting training data and output lables: ATTORNEY_ULT and NMVCCS_NARCOTIC_ULT
trainig_data<-trainig_data[complete.cases(trainig_data), ] ##eliminating rows with incomplete data
attach(trainig_data)
x_train <- subset(trainig_data, select=-ATTORNEY_ULT)
x_train <- subset(x_train, select=-NMVCCS_NARCOTIC_ULT)
x_train <- x_train[14:217]
y1 <- ATTORNEY_ULT
y2 <- NMVCCS_NARCOTIC_ULT

#############################################
#x1<- WC_KNEE_AN
#x2<- EVENT_CLASSIF
model.matrix(x2,trainig_data(x2) )##converting categorical variables to dummy variables
x2<- dummy.data.frame(x2)
  

svm_model <- svm(y1 ~ ., data=x_train,probability=FALSE)
svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_model <- svm(y1 ~ ., data=x_train, kernel="radial", cost=0.1)
##############################################

  
svm_model1 <- svm(x_train, factor(y1))##classification model
svm_model2 <- svm(x_train, factor(y2))##classification model
svm_model1_r <- svm(x_train, y1)##regression model
svm_model2_r <- svm(x_train, y2)##regression model
#################################Do the prediction on test data for two outputs: ATTORNEY_ULT and NMVCCS_NARCOTIC_ULT
testing_data<-testing_data[complete.cases(testing_data), ] ##eliminating rows with incomplete data
attach(testing_data)

#x <- subset(testing_data, select=-ATTORNEY_ULT)
#x <- subset(x, select=-NMVCCS_NARCOTIC_ULT)
x_test<- testing_data[14:217]

pred1 <- predict(svm_model1,x_test)###prediction for ATTORNEY_ULT
pred2 <- predict(svm_model2,x_test)###prediction for NMVCCS_NARCOTIC_ULT

pred1_r <- predict(svm_model1_r,x_test)###prediction for ATTORNEY_ULT
pred2_r <- predict(svm_model2_r,x_test)###prediction for NMVCCS_NARCOTIC_ULT

write.csv(pred1, file ="pred1.csv", row.names=FALSE)
write.csv(pred2, file ="pred2.csv", row.names=FALSE)
write.csv(pred1_r, file ="pred1_r.csv", row.names=FALSE)
write.csv(pred2_r, file ="pred2_r.csv", row.names=FALSE)
###########################test the accuracy over the training data
pred1_train<-predict(svm_model1,x_train)
pred2_train<-predict(svm_model2,x_train)
table(pred1_train,y1)
table(pred2_train,y2)




