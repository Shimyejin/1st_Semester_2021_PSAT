install.packages("tidyverse")
install.packages("data.table")
install.packages("gridExtra")
library(tidyverse)
library(data.table)
library(gridExtra)
setwd('C:\\Users\\User\\Desktop\\P-SAT\\3주차패키지')
data<-fread("data.csv")
test<-fread("test.csv")

data$bmi<-data$bmi %>% as.numeric()
data[]<-lapply(data,function(x){
  x[is.na(x)]<-mean(x,na.rm=TRUE)
  x})
data$hypertension<-as.integer(data$hypertension)
data$heart_disease<-as.integer(data$heart_disease)
data$stroke<-as.integer(data$stroke)
str(data)

sapply(data,class)
data[]<-lapply(data,function(x){
  if(is.character(x)){
    as.factor(x)}
  else x})
str(data)

data<-subset(data,select=-id) %>% as.data.frame()
str(data)

is.integer = function(x){
  class(x)=="integer"
}
data<-data %>% mutate_if(is.integer,as.factor)
strokeone<-data %>% select(where((is.factor))) %>% gather(key='variable', value='value',-stroke) %>% filter(stroke==1) %>% ggplot(aes(x=variable,fill=value)) + geom_bar(position = 'fill',alpha=0.5) + coord_flip() + labs(title="stroke : 1") + theme_bw() + theme(legend.position = "bottom",plot.title = element_text(size=11)) 
strokezero<-data %>% select(where((is.factor))) %>% gather(key='variable', value='value',-stroke) %>% filter(stroke==0) %>% ggplot(aes(x=variable,fill=value)) + geom_bar(position = 'fill',alpha=0.5) + coord_flip() + labs(title="stroke : 0") + theme_bw() + theme(legend.position = "bottom",plot.title = element_text(size=11)) 
grid.arrange(strokeone,strokezero,ncol=2)

strokeone2<-data %>% select(where((is.numeric)),stroke) %>% gather(key='variable', value='value',-stroke) %>% filter(stroke==1) %>% ggplot(aes(x=value,color=variable))+labs(title='stroke:1')+geom_density()+theme_bw() + theme(plot.title = element_text(size=11,hjust=0.5))
strokezero2<-data %>% select(where((is.numeric)),stroke) %>% gather(key='variable', value='value',-stroke) %>% filter(stroke==0) %>% ggplot(aes(x=value,color=variable))+labs(title='stroke:0')+geom_density()+theme_bw() + theme(plot.title = element_text(size=11,hjust=0.5))
grid.arrange(strokeone2,strokezero2,nrow=2)

cate_var<-select(data,where(is.factor),-stroke) %>% colnames()
re_cate<-data.frame(cate_var,chi=rep(NA,7))
for(i in 1:7){
  dt<-table(as_vector(data[,cate_var[i]]),data$stroke)
  chi<-chisq.test(dt)
  if(chi$p.value > 0.05){
    re_cate$chi[i]='accept'
  }else{
    re_cate$chi[i]='denied'
  }
}
re_cate

data<-select(data,-c(gender,Residence_type))

str(test)
test$bmi<-test$bmi %>% as.numeric()
test[]<-lapply(test,function(x){
  x[is.na(x)]<-mean(x,na.rm=T)
  x
})
test$id<-as.integer(test$id)
test$hypertension<-as.integer(test$hypertension)
test$heart_disease<-as.integer(test$heart_disease)
test$stroke<-as.integer(test$stroke)
test[]<-lapply(test,function(x){
  if(is.character(x)){
    as.factor(x)}
  else x})
test[]<-lapply(test,function(x){
  if(is.integer(x)){
    as.factor(x)}
  else x})
str(test)
test<-select(test,-c(id,gender,Residence_type))
str(test)
###################################
install.packages("devtools")
devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.21/catboost-R-Windows-0.21.tgz', INSTALL_opts = c("--no-multiarch"))
install.packages("catboost")
install.packages("caret")
install.packages("MLmetrics")
library(catboost)
library(caret)
library(MLmetrics)

logloss_cb<-expand.grid(depth=c(4,6,8),iterations=c(100,200),logloss=NA)
logloss_cb

set.seed(1234)
cf<-createFolds(data$stroke,k=5,list=FALSE)
cf
for(i in 1:6){
  temp<-NULL
  catrunningtime<-NULL
  for(k in 1:5){
    cv_data<-data[which(cf==k),]
    cv_test<-data[which(cf==k),]
    data_pool<-catboost.load_pool(data=cv_data[names(cv_data)!="stroke"],label=as.double(cv_data$stroke))
    test_pool<-catboost.load_pool(data=cv_test[names(cv_test)!="stroke"],label=as.double(cv_data$stroke))
    start=Sys.time()
    parameter<-list(loss_function='Logloss',random_seed=1234,iterations=logloss_cb[i,2],depth=logloss_cb[i,1],custom_loss='Logloss')
    cat<-catboost.train(learn_pool=data_pool,test_pool=test_pool,params=parameter)
    catrunningtime<-c(catrunningtime,Sys.time()-start)
    test_error<-read.table("catboost_info/test_error.tsv",sep="\t",header=T)
    temp<-c(temp,test_error[nrow(test_error),'Logloss'])
  }
  logloss_cb[i,'logloss']=min(temp)
  print(min(catrunningtime))
}
logloss_cb

logloss_cb[which.min(logloss_cb$logloss),]

data_pool_again<-catboost.load_pool(data=data[names(data)!="stroke"],label=as.double(data$stroke))
test_pool_again<-catboost.load_pool(data=data[names(data)!="stroke"],label=as.double(data$stroke))
parameter<-list(loss_function='Logloss',random_seed=1234,iterations=200,depth=8,custom_loss='Logloss')
catT<-catboost.train(learn_pool=data_pool_again,test_pool=test_pool_again,params=parameter)
test_error<-read.table("catboost_info/test_error.tsv",sep="\t",header=T)
test_error[nrow(test_error),'Logloss']
################################################
install.packages("factoextra")
install.packages("cluster")
library(factoextra)
library(cluster)

data_scale<-select(data,where(is.numeric))
data_scale<-scale(data_scale)

fviz1<-fviz_nbclust(data_scale,kmeans,method='wss')
fviz2<-fviz_nbclust(data_scale,kmeans,method='silhouette')
grid.arrange(fviz1,fviz2,ncol=2)

set.seed(1234)
k_means<-kmeans(data_scale,centers=3,nstart=1,iter.max=30)
clustering<-fviz_cluster(k_means,data_scale)+theme_bw()
clustering

Bp<-select(data,where(is.numeric))
Bp<-mutate(Bp,cluster=k_means$cluster)
Bp1<-Bp %>% ggplot(aes(x=factor(cluster),y=age))+geom_boxplot(outlier.shape=NA,alpha=0.5,fill=c('#845ec2', '#ffc75f', '#ff5e78'),color=c('#845ec2', '#ffc75f', '#ff5e78'))+stat_boxplot(geom='errorbar',color=c('#845ec2', '#ffc75f', '#ff5e78'))+labs(x='cluster')+theme_bw()
Bp2<-Bp %>% ggplot(aes(x=factor(cluster),y=avg_glucose_level))+geom_boxplot(outlier.shape=NA,alpha=0.5,fill=c('#845ec2', '#ffc75f', '#ff5e78'),color=c('#845ec2', '#ffc75f', '#ff5e78'))+stat_boxplot(geom='errorbar',color=c('#845ec2', '#ffc75f', '#ff5e78'))+labs(x='cluster')+theme_bw()
Bp3<-Bp %>% ggplot(aes(x=factor(cluster),y=bmi))+geom_boxplot(outlier.shape=NA,alpha=0.5,fill=c('#845ec2', '#ffc75f', '#ff5e78'),color=c('#845ec2', '#ffc75f', '#ff5e78'))+stat_boxplot(geom='errorbar',color=c('#845ec2', '#ffc75f', '#ff5e78'))+labs(x='cluster')+theme_bw()
grid.arrange(Bp1,Bp2,Bp3,ncol=3)
