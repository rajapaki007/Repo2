


##### Hi Dr Peter
library(mgcv);library(dlnm)
Av7daytemp<-(datause2$meantlag0+datause2$meantlag1+datause2$meantlag2+datause2$meantlag3+datause2$meantlag4 +datause2$meantlag5+datause2$meantlag6+datause2$meantlag7)/8
n
LessBMI <- subset(datause2,datause2$BMI<=18.5)
MediumBMI <- subset(datause2,datause2$BMI>=18.6 &datause2$BMI<=23.9)
AboeBMI <- subset(datause2,datause2$BMI>23.9)
unique(sort(datause2$BMI))
min(datause2$BMI)
max(datause2$BMI)
bs.tmean<-onebasis(Av7daytemp,fun="ns",df=5)
#bs.month <- onebasis(datause2$month,fun = "ns",df=3)
#bs.age <- onebasis(datause2$age,fun = "ns",df=5)
fit<-gam(sbp~bs.tmean+s(age)+Sex+s(BMI)
         +factor(edu)+factor(phy)+factor(smoke)
         +factor(drink)+factor(salt)+factor(income)+factor(HB_H )+factor(HB_T)+factor(t_db)
         +factor(h_db)+year+s(ID,bs="re"),data=datause2,method="REML")

pred.7avday<-crosspred(bs.tmean,fit, cen=22)
plot(pred.7avday,xlab="Temperature (°C)",ylab="Changes in HR (beats/min)",col="red")



#plot(pred.7avday,xlab="Temperature (°C)",ylab="Change in TC (mmol/L)",col="red",xlim=c(-10,30),ylim=c(0,0.3))
quantile(Av7daytemp, c(.01, .25, .75,.99))
-9.300  0.325 22.575 27.875 
temp30.pred<-crosspred(bs.tmean,fit,at=27.875,cen=22)
print(temp30.pred)