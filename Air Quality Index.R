d=read.csv("Prajna project.csv",header=TRUE);d
attach(d)

head(d,1)
D=AQI_Bucket
table(D)
D[D=="Good"]=0
D[D=="Satisfactory"]=1
D[D=="Moderate"]=2
D[D=="Poor"]=3
D[D=="Very Poor"]=4
D[D=="Severe"]=5

write.csv(D,"praj4.csv")
read.csv("praj4.csv")

summary(d)
library(moments)
skewness(d$PM2.5)
skewness(d$PM10)
skewness(d$NO)
skewness(d$NO2)
skewness(d$NH3)
skewness(d$CO)
skewness(d$SO2)
skewness(d$O3)
skewness(d$Benzene)
skewness(d$Temp)
skewness(d$AQI)

kurtosis(d$PM2.5)
kurtosis(d$PM10)
kurtosis(d$NO)
kurtosis(d$NO2)
kurtosis(d$NH3)
kurtosis(d$CO)
kurtosis(d$SO2)
kurtosis(d$O3)
kurtosis(d$Benzene)
kurtosis(d$Temp,na.rm=TRUE)
kurtosis(d$AQI,na.rm=TRUE)

sd(d$PM2.5)
sd(d$PM10)
sd(d$NO)
sd(d$NO2)
sd(d$NH3)
sd(d$CO)
sd(d$SO2)
sd(d$O3)
sd(d$Benzene)
sd(d$Temp,na.rm=TRUE)
sd(d$AQI,na.rm=TRUE)

m1=(d$PM2.5)
md1=function(m1){
um1=unique(m1)
um1[which.max(tabulate(match(m1,um1)))]
}
md1(m1)

m2=(d$PM10)
md2=function(m2){
um2=unique(m2)
um2[which.max(tabulate(match(m2,um2)))]
}
md2(m2)

m3=(d$NO)
md3=function(m3){
um3=unique(m3)
um3[which.max(tabulate(match(m3,um3)))]
}
md3(m3)

m4=(d$NO2)
md4=function(m4){
um4=unique(m4)
um4[which.max(tabulate(match(m4,um4)))]
}
md4(m4)

m5=(d$NH3)
md5=function(m5){
um5=unique(m5)
um5[which.max(tabulate(match(m5,um5)))]
}
md5(m5)

m6=(d$CO)
md6=function(m6){
um6=unique(m6)
um6[which.max(tabulate(match(m6,um6)))]
}
md6(m6)

m7=(d$SO2)
md7=function(m7){
um7=unique(m7)
um7[which.max(tabulate(match(m7,um7)))]
}
md7(m7)


m8=(d$O3)
md8=function(m8){
um8=unique(m8)
um8[which.max(tabulate(match(m8,um8)))]
}
md8(m8)

m9=(d$Benzene)
md9=function(m9){
um9=unique(m9)
um9[which.max(tabulate(match(m9,um9)))]
}
md9(m9)`

m10=(d$Temp)
md10=function(m10){
um10=unique(m10)
um10[which.max(tabulate(match(m10,um10)))]
}
md10(m10)
                                                                                                                                                                                                                              
m11=(d$AQI)
md11=function(m11){
um11=unique(m11)
um11[which.max(tabulate(match(m11,um11)))]
}
md11(m11)



set.seed(12345)
Y=Target
X=cbind(PM2.5,PM10,CO,NO,NO2,NH3,Benzene,O3,SO2,Temp,Target)
n=length(Y)
dt=sample(n,size=n*.8)
train=data.frame(X[dt,]);head(train)
ytrain=train[,11];head(ytrain)
xtrain=train[,-11];head(xtrain)
test=data.frame(X[-dt,]);head(test)
ytest=test[,11];head(ytest)
xtest=test[,-11]

n1=nrow(ytest)



table(d$Target)
#
library(caret) 

   

head(train)
model=nnet::multinom(train$Target~.,data=train,ref="5");model
summary(model)
z=abs(summary(model)$coefficients/summary(model)$standards.errors);head(z)
tb=qnorm(.975,0,1)
tb
s=step(model,method="backword")
summary(s)
exp(coef(s))
pvalue=(1-pnorm(abs(z),0,1))*2;head(pvalue)
p=predict(model,newdata=test,'class');head(p)
length(p)
nrow(train)
head(train$Target)
tab=table(p,test$Target);tab
confusionMatrix(p,as.factor(test$Target))
table(p)

#K-NN classification
library(caret)
library(class)
library(gmodels)
trainl=train[,"Target"]
testl=test[,"Target"]
xtrain
knnpred=knn(train=xtrain,test=xtest,cl=trainl,k=sqrt(n));knnpred
confusionMatrix1=confusionMatrix(knnpred,as.factor(ytest));confusionMatrix1
 

dt=sample(n,size=n*.8);head(dt)
train=data.frame(X[dt,]);head(train)
ytrain=train[,9];head(ytrain)
xtrain=train[,-9];head(xtrain)
test=data.frame(X[-dt,]);head(test)
ytest=test[,9];head(ytest)
xtest=test[,-9]

#Decision tree
library(rpart)
library(rpart.plot)
library(party)
set.seed(123)
DT=rpart(ytrain~.,data=xtrain,method="class");DT

rpart.plot(DT,type=1,extra=3,leaf.round=2,varlen=4,tweak=1.1,main="Decision tree plot")

pred1=predict(DT,xtest,type="class")
confusionmatrix2=confusionMatrix(pred1,as.factor(ytest));confusionmatrix2
table(pred1)

#Decision tree
library(e1071)
dtree1=rpart(train$Target~.,data=train,method="class")
rpart.plot(dtree1,type=1,extra=1)
predict=predict(dtree1,xtest,type="class");predict
confusionmatrix3=confusionMatrix(predict,as.factor(ytest));confusionmatrix3

tsy=xtest[,10]
head(xtest)	
length(tsy)

#Random forest
library(randomForest)
set.seed(123)
library(e1071)
d$Target=as.factor(d$Target)
train$Target=as.factor(train$Target)
test$Target=as.factor(test$Target)
rf=randomForest(Target~.,data=train,ntree=300,mtry=8,importance=TRUE,proximity=TRUE);rf
attributes(rf)
pred=predict(rf,xtest,type="class");pred
confusionmatrix4=confusionMatrix(pred,as.factor(test$Target));confusionmatrix4
plot(rf)


#svm
library(e1071)
as.matrix(train)
svm=svm(formula=train$Target~.,data=train);svm
p5=predict(svm,xtest,type="class");p5
table(p5)
confusionmatrix5=confusionMatrix(p5,as.factor(test$Target));confusionmatrix5


#Navie bayes

d$Target=as.factor(d$Target)
train$Target=as.factor(train$Target)
test$Target=as.factor(test$Target)
mod=naiveBayes(Target~.,data=train)
p6=predict(mod,xtest,type="class")
tab6=table(Actual=test$Target,Predicted=p6)
A6=sum(diag(tab6))/sum(tab6)
confusionmatrix6=confusionMatrix(p6,as.factor(test$Target));confusionmatrix6


#Bagging
library(ipred)
bag=bagging(Target~.,train)
p7=predict(bag,xtest);p7

tab7=with(test,table(Target,p7));tab7
A7=sum(diag(tab7))/sum(tab7)
confusionmatrix7=confusionMatrix(p7,as.factor(test$Target));confusionmatrix7

#Artificial neural network
library("nnet")
require(nnet)
library(ipred)
library(forecast)
nn=class.ind(ytrain)
seedsANN=nnet(xtrain,nn,size=12,softmax=TRUE);seedsANN
p8=predict(seedsANN,xtest,type="class")
confusionmatrix8=confusionMatrix(as.factor(p8),as.factor(test$Target));confusionmatrix8

#xgboost
library(xgboost)
xgb=xgboost(data=data.matrix(xtrain),
label=ytrain,
eta=0.05,
max_depth=2,
nround=25,
subsample=0.5,
colsample_bytree=0.5,
eval_metric="merror",
num_class=12,
nthread=2
)
p9=predict(xgb,data.matrix(xtest))
confusionmatrix9=confusionMatrix(as.factor(p9),as.factor(ytest));confusionmatrix9

r#ssc Classification
library(ssc)
library(caret)
m.selft1=selfTraining(x=xtrain,y=ytrain,learner=knn3,learner.pars=list(k=1),pred="predict")

dtrain=as.matrix(proxy::dist(x=xtrain,method="euclidian",by_rows=TRUE))
m.self2=selfTraining(x=dtrain,y=ytrain,x.inst=FALSE,learner=oneNN,pred="predict",pred.pars=list(type="prob"))

library(kernlab)
ktrain=as.matrix(exp(-0.048*dtrain^2))
m.selft3=selfTraining(x=ktrain,y=ytrain,x.inst=FALSE,learner=ksvm,learner.pars=list(kernel="matrix",prob.model=TRUE),pred=function(m,k)
predict(m,as.kernalMatrix(k[,SVindex(m)]),
type="probabilities"))

#Self Training
x=d[,-14]
y=d[,14]
x=scale(d[,14])

tra.idx=sample(x=length(y),size=ceiling(length(y)*0.5))
xtrain=x[tra.idx,]
ytrain=y[tra.idx]

tra.na.idx=sample(x=length(tra.idx),size=ceiling(length(tra.idx)*0.7))
ytrain[tra.na.idx]=NA

tst.idx=set.diff(1:length(y),tra.idx)
xitest=x[tst.idx,]
yitest=y[tst.idx]

xttest=x[tra.idx[tra.na.idx],]
yttest=y[tra.idx[tra.na.idx]]
#

####semi supervised learning


# Installing Packages
#install.packages("mlpack")
#install.packages("caTools")
#install.packages("caret")

# Loading package
library(e1071)
library(RSSL)
library(caTools)
library(caret)
library(mlpack)
d1=dd[Target,]
d1
attach(d1)
d2=data.frame(Benzene,CO,O3,PM10,PM2.5,SO2,NO,Target)

# Model Evaluation
g2=as.data.frame(xtest)
g_nm <- NearestMeanClassifier(g1~.,d2,prior=matrix((1/6),6))
g_nm
ts=data.frame(xtest)
Pr=predict(g_nm,d2)
Pr
tpr=table(Target,Pr)
tpr
g_self <- SelfLearning(g1~.,d2,
                       method=NearestMeanClassifier,
                       prior=matrix((1/6),6))
Pr1=predict(g_self,g2)

tpr1=table(Target,Pr1)
tpr1
confusionMatrix(tpr)


        
(accuracy <- sum(diag(tpr1)) / sum(tpr1))
                              (recall <- diag(tpr1) / rowSums(tpr1))
                                  (precision<- (diag(tpr1) / colSums(tpr1)))
                                   (F1 <- (2 * precision * recall) / (precision + recall))
                                   (f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall) / (precision + recall))


method=c("K-NN Classification","Decision tree","Random forest","svm","Navie bayes","Bagging","Artificial neural network","xgboost")


#semi supervised learning
library(e1071)
library(RSSL)
library(caTools)
library(caret)
library(mlpack)
library(ssc)
d1=d[train,]
d1
table(Target[366:731])
X=cbind(Benzene,CO,O3,PM10,PM2.5,SO2,NO)
g=as.factor(Target)
table(g)

data_variables <- as.matrix(X[,1:7])
data_label <- g
class(data_label)
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label = datSite)

train_data   <- data_variables[train,]
train_label  <- datSite[train]
tr_label=train_label-1
tr_label
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
write.csv(train_data,"taindata.csv")
write.csv(train_label,"tainlabel.csv")
write.csv(test_data,"testdata.csv")
write.csv(test_label,"testlabel.csv")
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train,]
test_label <- data_label[-train]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


numberOfClasses <- length(unique(datSite))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)



nround    <- 20 # number of XGBoost rounds
gg=as.numeric(test_label)
gg
g1=g[train]
g1





attach(d1)
data_variables <- as.matrix(X[,1:7])
data_label <- g
class(data_label)
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label = datSite)


d2=data.frame(Benzene,CO,O3,PM10,PM2.5,SO2,NO,Remark)
R1=Remark[1:1747]
d22=data.frame(train_data)
d22
g2=as.data.frame(xtest)
g2
df <- d1 %>% add_missinglabels_mar(Remark~.,prob=0.4) 
df
R1=df$Remark;R1
length(R1)
trl=Remark[train_index]
library(ssc)
m <- selfTraining(x = train_data, y = R1, learner = caret::knn3, 
                  learner.pars = list(k =1),pred="predict")
class(g2)
pred <- predict(m, test_data)
pred
(test_label1 <- data_label[-train])
tpr=table(test_label1,pred);tpr
class(tpr)
table(test_label)
confusionMatrix(tpr)

(accuracy <- sum(diag(tpr)) / sum(tpr))
(recall <- diag(tpr) / rowSums(tpr))
(precision<- (diag(tpr) / colSums(tpr)))
(F1 <- (2 * precision * recall) / (precision + recall))
####################################################################
install.packages("archdata")
library(xgboost)  # the main algorithm
library("archdata") # for the sample dataset
library(caret)    # for the confusionmatrix() function (also needs e1071 package)
library(dplyr)    # for some data preperation



table(Target[366:731])
X=cbind(Benzene,CO,O3,PM10,PM2.5,SO2,NO)
g=as.factor(Target)
table(g)

(datSite <- as.numeric(g))
summary(X)
# Make split index
train_index <- sample(1:nrow(X), nrow(X)*0.70)
# Full data set
data_variables <- as.matrix(X[,1:7])
data_label <- g
class(data_label)
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label = datSite)


# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- datSite[train_index]
tr_label=train_label-1
tr_label
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

write.csv(train_data,"taindata.csv")
write.csv(train_label,"tainlabel.csv")
write.csv(test_data,"testdata.csv")
write.csv(test_label,"testlabel.csv")
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


numberOfClasses <- length(unique(datSite))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
gg=as.numeric(test_label)
gg
g1=g[train_index]
g1
library(e1071)
library(RSSL)
library(caTools)
library(caret)
library(mlpack)
library(ssc)
d1=d[train_index,]
d1
attach(d1)
d2=data.frame(Benzene,CO,O3,PM10,PM2.5,SO2,NO,Target)
R1=Target[1:1134]
d22=data.frame(train_data)
d22
g2=as.data.frame(test_data)
g2
df <- d1 %>% add_missinglabels_mar(Target~.,prob=0.4) 
df
R1=df$Target;R1
length(R1)
trl=Target[train_index]
library(ssc)
m <- selfTraining(x = train_data, y = R1, learner = caret::knn3, 
                  learner.pars = list(k =1),pred="predict")
class(g2)

pred <- predict(m, test_data)
pred
(test_label1 <- data_label[-train_index])
tpr=table(pred,test_label1);tpr
class(tpr)
table(test_label)
confusionMatrix(tpr)

(accuracy <- sum(diag(tpr)) / sum(tpr))
(recall <- diag(tpr) / rowSums(tpr))
(precision<- (diag(tpr) / colSums(tpr)))
(F1 <- (2 * precision * recall) / (precision + recall))











