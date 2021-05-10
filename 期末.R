
Taipei_Main=read_excel("分時統計.xlsx",sheet=1)
Taipei_City_Hall=read_excel("分時統計.xlsx",sheet=2)
Nanjing_Fuxing=read_excel("分時統計.xlsx",sheet=3)
Tamsui=read_excel("分時統計.xlsx",sheet=4)
Banqiao=read_excel("分時統計.xlsx",sheet=5)

#台北車站
input=as.vector(t(as.matrix(Taipei_Main[1:30,2:22])))
t=c(1:630)
m=cbind(input,t);m=as.data.frame(m)
l=as.character(c(1:30))
a=ggplot(data=m,aes(x=t,y=input))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="進站",title="台北車站")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))
 

output=as.vector(t(as.matrix(Taipei_Main[32:61,2:22])))
t=c(1:630)
m=cbind(output,t);m=as.data.frame(m)
b=ggplot(data=m,aes(x=t,y=output))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="出站",title="台北車站")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))



total=as.vector(t(as.matrix(Taipei_Main[63:92,2:22])))
t=c(1:630)
m=cbind(total,t);m=as.data.frame(m)
c=ggplot(data=m,aes(x=t,y=total))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="總運量",title="台北車站")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

M=cbind(t,input,output);M=as.data.frame(M)
ggplot(data=M,aes(x=t))+geom_line(aes(y=input,col="進站"))+
  geom_line(aes(y=output,col="出站"))+geom_line(aes(y=total,col="總運量"))+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="運量",title="台北車站")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

#市政府
input=as.vector(t(as.matrix(Taipei_City_Hall[1:30,2:22])))
t=c(1:630)
m=cbind(input,t);m=as.data.frame(m)
l=as.character(c(1:30))
a=ggplot(data=m,aes(x=t,y=input))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="進站",title="市政府")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

output=as.vector(t(as.matrix(Taipei_City_Hall[32:61,2:22])))
t=c(1:630)
m=cbind(output,t);m=as.data.frame(m)
b=ggplot(data=m,aes(x=t,y=output))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="出站",title="市政府")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

total=as.vector(t(as.matrix(Taipei_City_Hall[63:92,2:22])))
t=c(1:630)
m=cbind(total,t);m=as.data.frame(m)
c=ggplot(data=m,aes(x=t,y=total))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="總運量",title="市政府")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

M=cbind(t,input,output);M=as.data.frame(M)
ggplot(data=M,aes(x=t))+geom_line(aes(y=input,col="進站"))+
  geom_line(aes(y=output,col="出站"))+geom_line(aes(y=total,col="總運量"))+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="運量",title="市政府")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

#南京復興
input=as.vector(t(as.matrix(Nanjing_Fuxing[1:30,2:22])))
t=c(1:630)
m=cbind(input,t);m=as.data.frame(m)
l=as.character(c(1:30))
a=ggplot(data=m,aes(x=t,y=input))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="進站",title="南京復興")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

output=as.vector(t(as.matrix(Nanjing_Fuxing[32:61,2:22])))
t=c(1:630)
m=cbind(output,t);m=as.data.frame(m)
b=ggplot(data=m,aes(x=t,y=output))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="出站",title="南京復興")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

total=as.vector(t(as.matrix(Nanjing_Fuxing[63:92,2:22])))
t=c(1:630)
m=cbind(total,t);m=as.data.frame(m)
c=ggplot(data=m,aes(x=t,y=total))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="總運量",title="南京復興")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

M=cbind(t,input,output);M=as.data.frame(M)
ggplot(data=M,aes(x=t))+geom_line(aes(y=input,col="進站"))+
  geom_line(aes(y=output,col="出站"))+geom_line(aes(y=total,col="總運量"))+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="運量",title="南京復興")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))
#淡水
input=as.vector(t(as.matrix(Tamsui[1:30,2:22])))
t=c(1:630)
m=cbind(input,t);m=as.data.frame(m)
l=as.character(c(1:30))
a=ggplot(data=m,aes(x=t,y=input))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="進站",title="淡水")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

output=as.vector(t(as.matrix(Tamsui[32:61,2:22])))
t=c(1:630)
m=cbind(output,t);m=as.data.frame(m)
b=ggplot(data=m,aes(x=t,y=output))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="出站",title="淡水")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

total=as.vector(t(as.matrix(Tamsui[63:92,2:22])))
t=c(1:630)
m=cbind(total,t);m=as.data.frame(m)
c=ggplot(data=m,aes(x=t,y=total))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="總運量",title="淡水")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

M=cbind(t,input,output);M=as.data.frame(M)
ggplot(data=M,aes(x=t))+geom_line(aes(y=input,col="進站"))+
  geom_line(aes(y=output,col="出站"))+geom_line(aes(y=total,col="總運量"))+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="運量",title="淡水")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))
#板橋
input=as.vector(t(as.matrix(Banqiao[1:30,2:22])))
t=c(1:630)
m=cbind(input,t);m=as.data.frame(m)
l=as.character(c(1:30))
a=ggplot(data=m,aes(x=t,y=input))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="進站",title="板橋")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

output=as.vector(t(as.matrix(Banqiao[32:61,2:22])))
t=c(1:630)
m=cbind(output,t);m=as.data.frame(m)
b=ggplot(data=m,aes(x=t,y=output))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="出站",title="板橋")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

total=as.vector(t(as.matrix(Banqiao[63:92,2:22])))
t=c(1:630)
m=cbind(total,t);m=as.data.frame(m)
c=ggplot(data=m,aes(x=t,y=total))+geom_line()+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="總運量",title="板橋")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))

M=cbind(t,input,output);M=as.data.frame(M)
ggplot(data=M,aes(x=t))+geom_line(aes(y=input,col="進站"))+
  geom_line(aes(y=output,col="出站"))+geom_line(aes(y=total,col="總運量"))+
  scale_x_continuous(breaks=seq(1,630,21),labels =l)+
  labs(x="day",y="運量",title="板橋")+
  theme(plot.title=element_text(hjust = 0.5,face="bold"))


