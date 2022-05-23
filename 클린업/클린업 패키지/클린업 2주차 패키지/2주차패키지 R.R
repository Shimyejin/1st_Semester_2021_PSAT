install.packages("tidyverse")
install.packages("data.table")
install.packages("VIM")
library(tidyverse)
library(data.table)
library(VIM)
setwd('C:\\Users\\User\\Desktop\\P-SAT\\2주차패키지')
data<-fread("data.csv")
data<-as.data.frame(data)
head(data)

#ch1 문제1
sdata<-data %>% select(-ends_with("2"))
head(sdata)

#ch1 문제2
aggr(sdata,col=c('lightyellow','pink'),numbers=TRUE,prop=FALSE,cex.axis=0.5)

#ch1 문제3-1
sdata[]<-lapply(sdata,function(x){
  x[is.na(x)]<-mean(x,na.rm=TRUE)
  x})
head(sdata)

#ch1 문제3-2
mode<-!sapply(sdata,is.numeric)
Md<-function(x){
  md1<-sort(unique(x))
  md1[which.max(tabulate(match(x,md1)))]
}
sdata[mode]<-lapply(sdata[mode],function(x) replace(x,is.na(x),Md(x[!is.na(x)])))

#ch1 문제4
sdata$OC<-ifelse(sdata$OC%in%c('open'),1,0)
head(sdata)

#ch1 문제5
sapply(sdata,class)
sdata[]<-lapply(sdata,function(x){
  if(bit64::is.integer64(x)){
    as.numeric(x)}
  else x})
sapply(sdata,class)

#[모델1 로지스틱 회귀]
#ch2 문제1
library(caret)
library(MLmetrics)
install.packages('randomForest')
library(randomForest)
set.seed(1234)
inTrain<-createDataPartition(y=sdata$OC,p=0.3,list=FALSE)
training<-sdata[-inTrain,]
validationset<-sdata[inTrain,]
nrow(training)
nrow(validationset)

#ch2 문제2
logit_model1<-glm(OC~.,family='binomial',data=training)
pred0<-predict(logit_model1,newdata=validationset,type='response')
pred1<-ifelse(pred0<0.5,0,1)
Accuracy(y_pred=pred1,y_true=validationset$OC)

#ch2 문제3
step(glm(OC~.,family=binomial,data=training),scope=list(lower=~1,upper=~.),direction='both')
logit_model2<-glm(OC~revenue1+salescost1+noi1
                  +interest1+quickAsset1+receivableS1
                  +nonCAsset1+tanAsset1+receivableL1,
                  family=binomial,data=training)
pred2<-predict(logit_model2,newdata=validationset,type='response')
pred3<-ifelse(pred2<0.5,0,1)
Accuracy(y_pred=pred3,y_true=validationset$OC)

#[모델2 랜덤포레스트]
#ch2 문제4
mtry_grid<-3:5
acc_rf<-expand.grid("mtry"=mtry_grid)
acc_rf$acc<-rep(NA,nrow(acc_rf))
acc_rf

#ch2 문제5
set.seed(1234)
cv_index<-createFolds(sdata$OC,k=5,list=TRUE)
mtry_grid<-3:5
x=1
for(i in 1:length(mtry_grid)){
  for(k in 1:5){
    cv_train=sdata[-cv_index[[k]],]
    cv_test=sdata[cv_index[[k]],]
    set.seed(1234)
    cv_acc<-c()
    rf<-randomForest(OC~ revenue1+salescost1+sga1+noi1+noe1+interest1+
                       profit1+liquidAsset1+quickAsset1+receivableS1+
                       inventoryAsset1+nonCAsset1+tanAsset1+surplus1+
                       ownerChange, data = cv_train, mtry = mtry_grid[i], 
                     ntree = 10, importance = F)
    data.pred = predict(rf, newdata = cv_test, type = 'response')
    rf.pred<-ifelse(data.pred < 0.5, 0, 1)
    rf.acc<- sum(rf.pred) / length(rf.pred)
    cv_acc = rf.acc
  }
  acc_rf[x, "acc"] = max(cv_acc)
  x = x + 1
}
acc_rf

#ch2 문제6
acc_rf[which.max(acc_rf$acc),]

#ch2 문제7
set.seed(1234)
Rf=randomForest(as.factor(OC)~revenue1+salescost1+sga1+noi1+noe1+interest1+
                  profit1+liquidAsset1+quickAsset1+receivableS1+
                  inventoryAsset1+nonCAsset1+tanAsset1+surplus1+
                  ownerChange, cv_train, mtry=4, ntree=10,importance=T)
im<-importance(Rf)
im.Rf<-im[,c(3,4)]
im.Rf<-as.matrix(im.Rf)
im.Rf<-data.frame(variable=rownames(im.Rf),im.Rf)
rownames(im.Rf)<-c(1:15)
imRfsort<-transform(im.Rf, variable=reorder(variable,MeanDecreaseGini))
ggplot(data=imRfsort, aes(x=variable,y=MeanDecreaseGini))+
  theme_classic()+geom_bar(stat='identity',fill='pink', width=0.09)+
  geom_point(color='pink',size=1.5)+coord_flip()+
  ylab("Mean Decrease Gini")+xlab("Variable Name")

#ch3
library(MASS)
library(caret)
library(MLmetrics)
library(randomForest)
Boston<-as.data.frame(Boston)
Boston

#ch3 문제1
set.seed(1234)
index_Train<-createDataPartition(Boston$medv,p=0.2,list=F)
train <- Boston[-index_Train, ]
test <- Boston[index_Train, ]
nrow(train)
nrow(test)

#ch3 문제2
mtry_grid<-3:5
ntree_grid <- rep(c(10,100,200))
RMSE_rf<-expand.grid("mtry"=mtry_grid,"ntree"=ntree_grid)
RMSE_rf$RMSE=rep(NA,nrow(RMSE_rf))
RMSE_rf

#ch3 문제3
set.seed(1234)
cv_index2<-createFolds(train$medv,k=5,list=T)
for(i in 1: nrow(RMSE_rf)){
  cv_rmse=NULL
  for(k in 1:5){
    cv_train = train[-cv_index2[[k]],]
    cv_test = train[cv_index2[[k]],]
    rf = randomForest(medv~.,
                      data = cv_train,
                      mtry=RMSE_rf[i,1],
                      num.trees=RMSE_rf[i,2],
                      importance = F)
    temp_pred = predict(rf, newdata=cv_test[-14],type='response')
    temp_RMSE = RMSE(temp_pred, cv_test$medv)
    cv_rmse = c(cv_rmse, temp_RMSE)
  }
  RMSE_rf[i, "RMSE"] = mean(cv_rmse)
}
RMSE_rf

#ch3 문제4
RMSE_rf = RMSE_rf %>% 
  arrange(RMSE)

opt_param = RMSE_rf[1,]
opt_param

#ch3 문제5
set.seed(1234)
rf_model = randomForest(medv ~ ., data = train,
                        mtry = opt_param$mtry, ntree = opt_param$ntree)

rf_yhat = predict(rf_model,test)

RMSE(rf_yhat,test$medv)
