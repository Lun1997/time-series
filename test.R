options(scipen=999)
install.packages("tseries")
library(tseries)
install.packages("forecast")
library(forecast)
install.packages("astsa")
library(astsa)

total=ts(total,frequency=21)
train=total[106:546];test=total[547:630]
train=ts(train,frequency=21);test=ts(test,frequency=21)
a=auto.arima(train,trace=TRUE,ic="aic")
Box.test(a$residuals)

v=c(1:441)
l=c(6:26)
m=cbind(v,train,a$fitted);m=data.frame(m)
ggplot(data=m,aes(x=v))+geom_line(aes(y=train,col="實際值"))+
  geom_line(aes(y=a.fitted,col="擬合值"))+
  scale_x_continuous(breaks=seq(1,441,21),labels=l)+
  labs(x="day",y="總運量",title="市政府 ARIMA")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))


p=Arima(total,order=c(0,0,3),seasonal=c(0,1,0))$fitted[547:630]
t=c(1:84);l=c(27:30)
c=data.frame(cbind(t,test,p))
ggplot(data=c,aes(x=t))+geom_line(aes(y=test,col="實際值"))+
  geom_line(aes(y=p,col="預測值"))+
  scale_x_continuous(breaks=seq(1,84,21),labels=l)+
  labs(x="day",y="總運量",title="4/27~4/30估計")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))
mean(abs(test-p))/mean(test)

taipei=p[64:84]
city=p[64:84]
Nanjing=p[64:84]
Tamsui=p[64:84]
Banqiao=p[64:84]
l=c(c(0,1),c(5:23))
M=data.frame(cbind(l,taipei,city,Nanjing,Tamsui,Banqiao))
ggplot(data=M,aes(x=t))+geom_line(aes(y=taipei,col="台北車站"))+
  geom_line(aes(y=city,col="市政府"))+geom_line(aes(y=Nanjing,col="南京復興"))+
  geom_line(aes(y=Tamsui,col="淡水"))+geom_line(aes(y=Banqiao,col="板橋"))+
  scale_x_continuous(breaks=c(1:21),labels=l)+
  labs(x="hr",y="總運量",title="一日人流估計")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))