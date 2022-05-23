library(plyr)
library(tidyverse)
library(data.table)

setwd("C:\\Users\\User\\Desktop\\P-SAT\\1주차패키지")
data<-fread("data.csv")
data %>% str()
data %>% is.na() %>% colSums
data %>% unique() %>% head()
dfdata<-data %>% data.frame()
for(i in 1:8){
  len<-dfdata[,i] %>% unique() %>% length()
  nacol<-colnames(dfdata)
  cat(i,'열',nacol[i],'의 유니크한 값의 개수는',len,'개\n')
}

nona<-data %>% filter(!is.na(confirmed_date))
nona %>% head()
nona %>% is.na() %>% colSums

nona[nona==""]<-NA
nona %>% is.na() %>% colSums
nona<-nona %>% filter(!is.na(sex))
nona<-nona %>% filter(!is.na(age))
nona<-nona %>% filter(!is.na(city))
nona %>% head()

nona %>% is.na() %>% colSums
nona<-nona %>% data.frame()
nona %>% unique() %>% head()
for(i in 1:8){
  newlen<-nona[,i] %>% unique() %>% length()
  nacol2<-colnames(nona)
  cat(i,'열',nacol2[i],'의 유니크한 값의 개수는',newlen,'개\n')
}

nona<-nona %>% filter(nona$country=='Korea') %>% select(-country)
nona %>% head()

nona$province[nona$province=='서울']<-'서울특별시'
nona$province[nona$province=='부산']<-'부산광역시'
nona$province[nona$province=='대구']<-'대구광역시'
nona$province[nona$province=='인천']<-'인천광역시'
nona$province[nona$province=='대전']<-'대전광역시'
nona$province[nona$province=='세종']<-'세종특별자치시'
nona$province[nona$province=='울산']<-'울산광역시'
nona$province[nona$province=='제주도']<-'제주도특별자치도'
nona %>% head()

nona$confirmed_date<-as.Date(nona$confirmed_date)
nona %>% head()

nona<-nona %>% group_by(confirmed_date) %>% mutate(confirmed_number=n())
nona %>% head()

nona<-nona %>% mutate(wday=ifelse(wday(confirmed_date)%in%c(1,7),'주말','주중'))
nona %>% head()

summary<-nona %>% group_by(age,confirmed_date) %>% summarise(confirmed_number=n()) %>% as.data.frame()
tapply(summary$confirmed_number,summary$age,summary)

#2
library(ggplot2)
nona<-data.frame(nona)
first<-ggplot(nona,aes(x=confirmed_date, y=confirmed_number))+
  geom_line(color="lightblue")+
  theme_classic()+
  geom_point(aes(x=confirmed_date[which.max(confirmed_number)],y=max(confirmed_number)),color="navy")+
  labs(title='코로나 확진자수 추이\n-국내인기준')+theme(plot.title=element_text(hjust=0.5,face="bold"))+
  geom_text(aes(x=confirmed_date[which.max(confirmed_number)],y=max(confirmed_number)), label="2020-03-05(143명)", color='navy',hjust=1.05)
first

second<-ggplot(nona, aes(x=confirmed_date,y=confirmed_number))+geom_line(aes(color=province, group=province))+facet_wrap(vars(province),nrow=4)
second

nona %>% group_by(province,state) %>% summarise(n=n()) %>% ggplot()+geom_bar(aes(x=n,y=reorder(province,n),fill=state,color=state),stat="identity",alpha=0.5)+labs(x='확진자 수',y='지역')

Bx<-summary %>% ggplot(aes(x=age,y=confirmed_number))+geom_boxplot(aes(color=age, fill=age), alpha=0.5,outlier.size=-1)+labs(y='일단위 확진자수')+theme_classic()+stat_boxplot(geom='errorbar',aes(color=age))
Bx

summary(anotest<-aov(confirmed_number~age, data=summary))
anotest

#3
install.packages("corrplot")
install.packages("caret")
install.packages("MLmetrics")
library(MASS)
library(tidyverse)
library(corrplot)
library(caret)
library(MLmetrics)
Bdata<-Boston %>% select_if(is.numeric) %>% cor()
Bdata
corrplot(Bdata,type="upper",method="number")

Boston<-as.data.frame(Boston)
Boston %>% gather(-medv,key="var",value="val") %>% ggplot(aes(x=val,y=medv))+geom_point()+facet_wrap(~var,nrow=4,scale='free')+theme_light()+labs(title="Scatter plot of dependent variables vs Median Value (medv)")+stat_smooth(color="lightblue", method="lm")

set.seed(1234)
sample.size <- floor(0.7 * nrow(Boston))
train.index <- sample(seq_len(nrow(Boston)), size = sample.size)
train <- Boston[train.index, ]
test <- Boston[- train.index, ]
nrow(train)
nrow(test)

lm.fit=lm(medv~.,data=train)
summary(lm.fit)
predictions<-lm.fit %>% predict(test[,-14])
RMSE(predictions,test$medv)

lm.fit
dffit<-lm.fit$coefficients %>% as.data.frame()
dffit$cols=rownames(dffit)
colnames(dffit)=c("value","val")
dffit

dffit %>% ggplot(aes(x=value, y=reorder(val,value),fill=value,color=value))+
  geom_bar(stat = 'identity',alpha=0.2)+
  theme_classic()+labs(x='value',y='intercept and variable')+
  scale_color_gradient2(low="blue", mid="yellow",high="red")+
  scale_fill_gradient2(low="blue", mid="yellow",high="red")+
  theme(legend.position = 'none') +
  geom_text(aes(label = round(value,2)),color="black",position=position_stack(0.5))
