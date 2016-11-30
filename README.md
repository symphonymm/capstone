# capstone
library(plyr)
library(stringr)
news<-read.csv('C:/Users/Miaomiao/Desktop/capstone/kaggle stock and news data/news.csv',header = T)
pos <- scan('C:/Users/Miaomiao/Desktop/capstone/kaggle stock and news data/positive.txt', what='character')
head(pos)
neg <- scan('C:/Users/Miaomiao/Desktop/capstone/kaggle stock and news data/negative.txt', what='character')

a<-news
d<-a[,3:27]
for (x in 1:25)
{
  d[,x]<-gsub('^b',"",d[,x])
}


for (x in 1:25)
{
  d[,x]<-gsub('[[:punct:]]'," ",d[,x])
}

po<-matrix(0,nrow=1989,ncol=25)
for (y in 1:25)
{
  for (x in 1:1989)
  {
    lis<- str_split(d[x,y], "\\s+",simplify = F)
    token<-unlist(lis)
    po[x,y]<-sum(!is.na(match(token,pos)))
    
  }
}

ne<-matrix(0,nrow=1989,ncol=25)
for (y in 1:25)
{
  for (x in 1:1989)
  {
    lis<- str_split(d[x,y], "\\s+",simplify = F)
    token<-unlist(lis)
    ne[x,y]<-sum(!is.na(match(token,neg)))
    
  }
}
fi<-matrix(0,nrow=1989,ncol=25)
fi<-(po*2.4-ne)
tt<-rowSums(fi)
fi<-cbind(fi,round(tt,1))
fi<-cbind(a$Label,fi)

DJIA_table<-read.csv("C:/Users/Miaomiao/Desktop/capstone/kaggle stock and news data/DJIA_table.csv",header=T)
stock<-DJIA_table
stock[,2:6]<-NULL
stock<-stock[rev(rownames(stock)),]
stock$return<-0
for (x in 1:1988)
{
  stock[(x+1),3]<-round((stock[(x+1),2]-stock[x,2])/stock[x,2],digits=6)
}

stock<-cbind(stock,fi)
colnames(stock)<-c("Date","Price","Returns","label","Top.1","Top.2","Top.3","Top.4",
                   "Top.5","Top.6","Top.7","Top.8","Top.9","Top.10",
                   "Top.11","Top.12","Top.13","Top.14","Top.15","Top.16",
                   "Top.17","Top.18","Top.19","Top.20","Top.21","Top.22",
                   "Top.23","Top.24","Top.25","Total")

stock$prediction<-0
stock<-stock[-1,] ######!!!!!delete first row of returns which is zero
for ( x in 1:1789)
{ 
  z<-subset(stock[x:(199+x),])
  v<-lm(Returns~Total,data=z)
  stock[(200+x),31]<-round(stock[(200+x),30]*v$coefficients[2]+v$coefficients[1],digits = 6)
  
}

stock$label_model_uni<-NA
for (x in 201:1988)
{
  if(stock[x,31]>0)
  {
    stock$label_model_uni[x]<-1
  }
  else 
  {
    stock$label_model_uni[x]<-0
  }
}

correctly_label_model_uni<-(sum(stock$label==stock$label_model_uni,na.rm = T))/1788



stock$prediction_25<-0
for (x in 1:1788)
{
  z<-subset(stock[x:(199+x),])
  v<-lm(z$Returns~z$Top.1+z$Top.2+z$Top.3+z$Top.4+z$Top.5+z$Top.6+z$Top.7+z$Top.8+z$Top.9
        +z$Top.10+z$Top.11+z$Top.12+z$Top.13+z$Top.14+z$Top.15+z$Top.16+z$Top.17+z$Top.18+z$Top.19
        +z$Top.20+z$Top.21+z$Top.22+z$Top.23+z$Top.24+z$Top.25)
  stock[(200+x),33]<-round(stock[(200+x),4]*v$coefficients[2]+stock[(200+x),5]*v$coefficients[3]+
                             stock[(200+x),6]*v$coefficients[4]+stock[(200+x),7]*v$coefficients[5]+
                             stock[(200+x),8]*v$coefficients[6]+stock[(200+x),9]*v$coefficients[7]+
                             stock[(200+x),10]*v$coefficients[8]+stock[(200+x),11]*v$coefficients[9]+
                             stock[(200+x),12]*v$coefficients[10]+stock[(200+x),13]*v$coefficients[11]+
                             stock[(200+x),14]*v$coefficients[12]+stock[(200+x),15]*v$coefficients[13]+
                             stock[(200+x),16]*v$coefficients[14]+stock[(200+x),17]*v$coefficients[15]+
                             stock[(200+x),18]*v$coefficients[16]+stock[(200+x),19]*v$coefficients[17]+
                             stock[(200+x),20]*v$coefficients[18]+stock[(200+x),21]*v$coefficients[19]+
                             stock[(200+x),22]*v$coefficients[20]+stock[(200+x),23]*v$coefficients[21]+
                             stock[(200+x),24]*v$coefficients[22]+stock[(200+x),25]*v$coefficients[23]+
                             stock[(200+x),26]*v$coefficients[24]+stock[(200+x),27]*v$coefficients[25]+
                             stock[(200+x),28]*v$coefficients[26]+v$coefficients[1],digits = 6)
}
stock$label_model_multi<-NA
for (x in 201:1988)
{
  if(stock[x,33]>0)
  {
    stock$label_model_multi[x]<-1
  }
  else 
  {
    stock$label_model_multi[x]<-0
  }
}

correctly_label_model_multi<-(sum(stock$label==stock$label_model_multi,na.rm = T))/1788
library(gmodels)
table(stock$label[201:1988],stock$label_model_multi[201:1988])
table(stock$label[201:1988],stock$label_model_uni[201:1988])

##write.csv(stock[,4:29],"C:/Users/Miaomiao/Desktop/capstone/kaggle stock and news data/number.csv")

pp<-stock[1400:1450,c(1,4,32,34)]
df_melt = melt(pp, id.vars = 'Date')
qplot(Date, value,data=df_melt,facets =.~variable,xlab = "Date (03/04/2014~05/14/2014)")

qq<-stock[1100:1150,c(1,4,32,34)]
df_melt = melt(qq, id.vars = 'Date')
qplot(Date, value,data=df_melt,facets =.~variable,xlab = "Date (12/20/2012~03/6/2013)")

ll<-stock[800:850,c(1,4,32,34)]
df_melt = melt(ll, id.vars = 'Date')
qplot(Date, value,data=df_melt,facets =.~variable, xlab = "Date (10/11/2011~12/21/2011)")
