#Workshop 1: Imputing missing values
#Uncomment the following if you need to install the following packages 
#install.packages("mice")
#install.packages("VIM")
#install.packages("NHANES")
library(mice)
library(VIM)
library(NHANES)

#Exercise 2 
#(a) 
data(tao) #in VIM package
?tao
table(tao$Year)
#there are only two years and we do not need the numeric value of Year in the regression model, 
# so convert it to a factor
tao$Year<-as.factor(tao$Year) 
plot(tao$Air.Temp,tao$Sea.Surface.Temp,col=tao$Year)

startMar<-par()$mar #store the current margin settings
par(mar=c(0,0,0,0)+0.1)
md.pattern(tao, rotate.names=TRUE) 
summary(tao)
aggr(tao)

#(b) Questions regarding the output to the above commands. 

#(c)
any.missing<-!complete.cases(tao)
par(mar=startMar)
marginplot(tao[,c("Air.Temp", "Humidity")])
marginplot(tao[,c("Air.Temp","Sea.Surface.Temp")])
#Notice that the missing values appear not to be random

#(d)
#You will use the same longish model specification several times in Exercise 3, so let's give the "formula" a short name.
tao.model<-formula(Sea.Surface.Temp~Year+Longitude+Latitude+Air.Temp+ Humidity+UWind+VWind)
#fit a linear regression model to the "fully known" observations
lm.knowns<-lm(tao.model,data=tao)
summary(lm.knowns)

##=====================================================
#Exercise 4 Multivariate imputation using Gibbs sampling
#(a)
GibbsData <- mice(tao,m=5,maxit=50,meth='pmm',seed=600)

#(b)
Gibbsdata1<-complete(GibbsData,1)
#plot with missing values in red
with(Gibbsdata1,plot(Air.Temp,Sea.Surface.Temp,col=1+any.missing))

lm.Gibbs1<-lm(tao.model,data=Gibbsdata1)
summary(lm.Gibbs1)
cbind(lm.knowns$coefficients,lm.mrep$coefficients,lm.msdrep$coefficients,lm.drs$coefficients,lm.Gibbs1$coefficients)

#(c)
#run lm on all 5 complete data sets
lm.Gibbs.all<-with(GibbsData,lm(Sea.Surface.Temp~Year+Longitude+Latitude+Air.Temp+ Humidity+UWind+VWind))
#the resulst of each on 
lm.Gibbs.all$analyses 

#summary(lm.obj) for each of the 5 lms.
lapply(lm.Gibbs.all$analyses,summary)
sapply(lm.Gibbs.all$analyses,coefficients)

#(d)
summary(pool(lm.Gibbs.all))
# final model
lm.Gibbs.all.final<-with(GibbsData,lm(Sea.Surface.Temp~Year+Longitude+Latitude+Air.Temp+Humidity+VWind))
summary(pool(lm.Gibbs.all.final))

#Exercise 5: Imputing missing data for the Diabetes data set
#(a)
library(NHANES)
Diabetes2<-data.frame(NHANES[,c("Diabetes","Gender","Race1","BMI","Age","Pulse","BPSysAve","BPDiaAve","HealthGen","DaysPhysHlthBad","DaysMentHlthBad","LittleInterest","Depressed")])
table(Diabetes2$Diabetes)

#convert Diabetes vaiable to be a binary outcome for logistic regression
Diabetes2$Diabetes<-as.numeric(Diabetes2$Diabetes)-1

par(mar=c(0,0,0,0)+0.1)
md.pattern(Diabetes2,rotate.names = TRUE) 
#reset the margins
startMar<-par()$mar #store the current margin settings
par(mar=startMar)

summary(Diabetes2)
aggr(Diabetes2)

any.missing<-!complete.cases(Diabetes2)

#(b)Multivariate imputation
#this takes a while!
GibbsDiabetes <- mice(Diabetes2, m=5,maxit=10,meth='pmm',seed=700)

##obtain the first complete data set
Gibbs1Diabetes<-???;  
Gibbs1Diabetes<-complete(GibbsDiabetes,1)

#output a cat!
md.pattern(Gibbs1Diabetes) 



#(c) glm() with family="binomial" specifies a logistic regression
#first the logistic regression on the original known data, throwing away the missings
logreg.known<-glm(Diabetes~.,data=???,family="binomial")
logreg.known<-glm(Diabetes~.,data=Diabetes2,family="binomial")
summary(logreg.known)
logreg.Gibbs1<-glm(Diabetes~.,???)
summary(???)
logreg.full<-with(GibbsDiabetes,glm(Diabetes~Gender+Race1+BMI+Age+Pulse+BPSysAve+BPDiaAve+HealthGen+
                                      DaysPhysHlthBad+DaysMentHlthBad+LittleInterest+Depressed,family="binomial"))
summary(pool(???))


#remove the variable Depressed as it seems not to be significant at 5% level  
logreg.final<-with(GibbsDiabetes,glm(Diabetes~Gender+Race1+BMI+Age+Pulse+BPSysAve+BPDiaAve+HealthGen+
                                       DaysPhysHlthBad+DaysMentHlthBad,family="binomial"))
summary(pool(????))


#predictions from the logistic regression model
pr.logreg<-predict(logreg.Gibbs1,type="response")>0.5
table(pr.logreg,Diabetes2$Diabetes)


#e)
library(rpart)
library(rpart.plot)
tree.known<-rpart(Diabetes~.,data=Diabetes2)
rpart.plot(tree.known)

tree.Gibbs1<-rpart(Diabetes~.,data=Gibbs1Diabetes)
rpart.plot(tree.Gibbs1)



Gibbs.full<-with(GibbsDiabetes,rpart(Diabetes~Gender+Race1+BMI+Age))

pool(Gibbs.full) ###Damn!  (produces an error)

for(i in 1:5){
  tree.Gibbsfull<-rpart(Diabetes~Gender+Race1+BMI+Age+Pulse+BPSysAve+BPDiaAve+HealthGen+DaysPhysHlthBad+DaysMentHlthBad+LittleInterest+Depressed,
                        data=complete(GibbsDiabetes,i))
  rpart.plot(tree.Gibbsfull,main=i,roundint=FALSE)
}