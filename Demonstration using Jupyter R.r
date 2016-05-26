library(sjPlot)
library(sjmisc)
library(stargazer)
library(ggplot2)
library(plotrix)
setwd("C:/Users/ning/Dropbox/NYU/semester/Fall 2015/multivariate + regression/final project")

datatoread = read_spss("projectdata.sav",
                      option='foregin',
                      enc='UTF-8')

data = data.frame(datatoread)

data[data == 999] = NA # tell the program what 999 means, it means missing data
data = subset(data,select = -c(ID))#drop subject No.
datanames=colnames(data)

datanames
# set basic theme options
sjp.setTheme(theme = "forestgrey",
             axis.title.size = .85, 
             axis.textsize = .85, 
             legend.size = .8, 
             geom.label.size = 3.5)

stargazer(data)

remove_outliers = function(x,na.rm=T){
  qnt = quantile(x,probs=c(.25,.75),na.rm = na.rm)
  H = 1.5* IQR(x,na.rm = na.rm)
  y = x
  y[x<(qnt[1] - H)]= NA
  y[x>(qnt[2] + H)]= NA
  return(y)
}

# identify extreme data that locate 3 standard deviation away from the mean
outfun <- function(x){
  outliers = abs(x-mean(x,na.rm=T))>3*sd(x,na.rm=T)
  countoutliers = which(is.na(outliers))
  numofoutliers = length(countoutliers)
  return(list(countoutliers=countoutliers,numofoutliers=numofoutliers))
}

# empty matrix, preallocation
Table_Table = matrix(NA,nrow = 17, ncol=2)
cnt = 1

# just a better for me to read the results
for (i in datanames){
    if (i == "Conf1" | i == "Conf2" | i == 'year' | i == 'gender') {
        next
    }else{
  k=outfun(data[,i])
  #print(c(i,k$numofoutliers,"outliers"))
  Table_Table[cnt,1] = i
  Table_Table[cnt,2] = k$numofoutliers
  cnt = cnt + 1
  #qqnorm(data[,i],main=i)# show normality 
  data[,i]=remove_outliers(data[,i])
  }}
Table_Table = data.frame(Table_Table)
colnames(Table_Table)=c('name','# of outliers')
Table_Table

#for (i in datanames){
    
    #data[is.na(data[,i]),i]=mean(data[,i],na.rm=T)
    #}

# empty matrix, preallocation
Table_Table = matrix(NA,nrow = 17, ncol=2)
cnt = 1

# just a better for me to read the results
for (i in datanames){
    if (i == "Conf1" | i == "Conf2" | i == 'year' | i == 'gender') {
        next
    }else{
  k=outfun(data[,i])
  #print(c(i,k$numofoutliers,"outliers"))
  Table_Table[cnt,1] = i
  Table_Table[cnt,2] = k$numofoutliers
  cnt = cnt + 1
  #qqnorm(data[,i],main=i)# show normality 
  data[,i]=remove_outliers(data[,i])
  }}
Table_Table = data.frame(Table_Table)
colnames(Table_Table)=c('name','# of outliers')
Table_Table

library(psych)

# compute the Sense of beloning indicator
data$SOB_pre = (data$SOBitem1pre+data$SOBitem2pre+data$SOBitem3pre
                +data$SOBitem4pre+data$SOBitem5pre)/5
data$SOB_post=(data$SOBitem1post+data$SOBitem2psot+data$SOBitem3post
               +data$SOBitem4post+data$SOBitem5post)/5
data$SOB_indicator = (data$SOB_post - data$SOB_pre)/(data$SOB_post + data$SOB_pre)
# positive means effort based and negative means ability based sense of belonging

ggplot(data=data,aes(x=1:length(data$SOB_indicator),y=data$SOB_indicator))+geom_point()+
    geom_hline(yintercept = 0)+xlab('subject #')+ylab('SOB')

norm_vec <- function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T) # function for normalization
V1 = data$Post1Taskfeel1+data$Post1Taskfeel2+data$Post1Taskfeel3 # day1 self-feeling
V2 = data$Post2Taskfeel1+data$Post2Taskfeel2 # day2 self-feeling
data$self_estimate = norm_vec(V1)- norm_vec(data$ACC1)
data$self_estimate2 = norm_vec(V2)- norm_vec(data$ACC2)

data$self_estimate=norm_vec(data$self_estimate)
data$self_estimate2=norm_vec(data$self_estimate2)
ggplot(data=data, aes(x=self_estimate,y=self_estimate2))+geom_point()

data$ACC1 = norm_vec(data$ACC1)
data$ACC2 = norm_vec(data$ACC2)
#data$Conf1 = data$Conf1 - mean(data$Conf1,na.rm = T)
#data$Conf2 = data$Conf2 - mean(data$Conf2,na.rm = T)
data$SOB_indicator = norm_vec(data$SOB_indicator) # rescale the indicator

ggplot(data=data,aes(x=1:length(data$SOB_indicator),y=data$SOB_indicator))+geom_point()+
    geom_hline(yintercept = 0)+xlab('subject #')+ylab('SOB')+
    annotate('text',x=100,y=2,label='no much change...',color='red')
#text(100,0.3,'No much change..',col='red')

describe(data)
cor.ci(data)

ylims=c(-0.02,0.01)
lmDay1.one = lm(self_estimate~ACC1,data=data)
lmDay1.two = lm(self_estimate ~ Conf1+ACC1,data=data)
# interaction
data$ACCconf1 = data$ACC1 * data$Conf1
lmDay1.three = lm(self_estimate ~ Conf1+ACC1+ACCconf1,data=data)
# direct prediction with moderator SOB
lmDay1.four = lm(self_estimate~Conf1+ACC1+ACCconf1+SOB_indicator,data=data)
# interactions
data$ACCSOB1=data$ACC1*data$SOB_indicator
data$confSOB1 = data$Conf1*data$SOB_indicator
data$ACCconfSOB1 = data$ACC1*data$SOB_indicator*data$SOB_indicator
lmDay1.five = lm(self_estimate~Conf1+ACC1+SOB_indicator+ACCconf1+confSOB1+ACCSOB1,data = data)

summary(lmDay1.five)
sjp.lm(lmDay1.five)

sjp.lm(lmDay1.five,type='slope')

anova(lmDay1.one,lmDay1.two,lmDay1.three,test="F")
anova(lmDay1.four,lmDay1.five,test="F")


diagPlot<-function(model){
    p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
    p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
    p1<-p1+xlab("Fitted values")+ylab("Residuals")
    p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
    
    p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point()
    p2<-p2+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
    p2<-p2+ggtitle("Normal Q-Q")+theme_bw()
    
    p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point()
    p3<-p3+stat_smooth(method="loess")+xlab("Fitted Value")
    p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
    p3<-p3+ggtitle("Scale-Location")+theme_bw()
    
    p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
    p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
    p4<-p4+ggtitle("Cook's distance")+theme_bw()
    
    p5<-ggplot(model, aes(.hat, .stdresid))+geom_point()
    p5<-p5+stat_smooth(method="loess")
    p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
    p5<-p5+ggtitle("Residual vs Leverage Plot")
    p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
    p5<-p5+theme_bw()+theme(legend.position="bottom")
    
    p6<-ggplot(model, aes(.hat, .cooksd))+geom_point()+stat_smooth(method="loess")
    p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
    p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
    p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
    p6<-p6+theme_bw()
    
    return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}

diagPlot(lmDay1.three)


diagPlot(lmDay1.five)

which(rstudent(lmDay1.three)>2)
which(rstudent(lmDay1.five)>2)

# day 2
lmDay2.one = lm(data$self_estimate2~data$ACC2)
lmDay2.two = lm(data$self_estimate2 ~ data$Conf2+data$ACC2)
# interaction
data$ACCconf2 = data$ACC2 * data$Conf2
lmDay2.three = lm(data$self_estimate2 ~ data$Conf2+data$ACC2+data$ACCconf2)
lmDay2.four = lm(self_estimate2~Conf2+ACC2+ACCconf2+SOB_indicator,data=data)
# interactions
data$ACCSOB2=data$ACC2*data$SOB_indicator
data$confSOB2 = data$Conf2*data$SOB_indicator
lmDay2.five = lm(self_estimate2~Conf2+ACC2+SOB_indicator+ACCconf2+confSOB2+ACCSOB2,data=data)

summary(lmDay2.five)
sjp.lm(lmDay2.five)
sjp.lm(lmDay2.five,type="slope")
sjp.lm(lmDay2.five,type="eff")
sjp.lm(lmDay2.five,type="fe")
sjp.lm(lmDay2.five,type="pred",vars=c('ACC2','Conf2'),facet.grid = F)
sjp.lm(lmDay2.five,type="fe.cor")
sjp.lm(lmDay2.five,type="ri.slope")
sjp.lm(lmDay2.five,type="re.qq")



anova(lmDay2.one,lmDay2.two,lmDay2.three,test="F")
anova(lmDay2.four,lmDay2.five,test="F")




pMiss = function(x){sum(is.na(x))/length(x)*100}
sort(apply(data,2,pMiss))

library(VIM)
library(mice)
aggr_plot=aggr(data,col=c('navyblue','red'),numbers=TRUE,
               sortVars=TRUE,labels=names(data),cex.axis=.7,
               gap=3,ylab=c("Histogram of missing data","Pattern"))

tempData=mice(data,m=5,method='norm',seed=500)

cnt = 1
p.values=c()
for (c in 1:5){
  completeData=complete(tempData,c)
  for (i in datanames){
    print(c(i,cnt))
    TT = t.test(data[,i],completeData[,i])
    p.values[cnt]=TT$p.value
    cnt = cnt +1
    
  }
}

which(p.values<0.05)

fit3 = lm(self_estimate~Conf1+ACC1+SOB_indicator+ACCconf1+confSOB1+ACCSOB1,data = data)

# pooling 
tempData = mice(data,m=5,seed=245435)
modelFit1=with(tempData,lm(self_estimate~Conf1+ACC1+SOB_indicator+ACCconf1+confSOB1+ACCSOB1))
tempData2=mice(data,m=50,seed=245435)
modelFit2=with(tempData2,lm(self_estimate~Conf1+ACC1+SOB_indicator+ACCconf1+confSOB1+ACCSOB1))
print(summary(pool(modelFit1)),digits=2)
print(summary(pool(modelFit2)),digits=3)
print(summary(fit3),digits=2)

# Day 2
fit3 = lm(self_estimate2~Conf2+ACC2+SOB_indicator+ACCconf2+confSOB2+ACCSOB2,data=data)
# pooling 
tempData = mice(data,m=5,seed=245435)
modelFit1=with(tempData,lm(self_estimate2~Conf2+ACC2+SOB_indicator+ACCconf2+confSOB2+ACCSOB2))
tempData2=mice(data,m=50,seed=245435)
modelFit2=with(tempData2,lm(self_estimate2~Conf2+ACC2+SOB_indicator+ACCconf2+confSOB2+ACCSOB2))
print(summary(pool(modelFit1)),digits=2)
print(summary(pool(modelFit2)),digits=3)
print(summary(fit3),digits=3)


library(car)

summary(lmDay2.five)
avPlots(lmDay2.five)

library(VIM)

marginplot(data[,c("SOB_indicator","self_estimate2")])

library(coefplot)

coefplot(lmDay2.five, parm = -1)

library(visreg)

summary(lmDay1.five)
visreg(lmDay1.five)

summary(lmDay2.five)
visreg(lmDay2.five)

library(Rmisc)

ggplot(data, aes(data$SOB_indicator))+geom_histogram()

data$SOB_category[data$SOB_indicator < mean(data$SOB_indicator,na.rm=TRUE)+.7*sd(data$SOB_indicator,na.rm=TRUE) & 
data$SOB_indicator > mean(data$SOB_indicator,na.rm=TRUE)-.7*sd(data$SOB_indicator,na.rm=TRUE)] = 0
data$SOB_category[data$SOB_indicator > mean(data$SOB_indicator,na.rm=TRUE)+.7*sd(data$SOB_indicator,na.rm=TRUE)] = 1
data$SOB_category[data$SOB_indicator < mean(data$SOB_indicator,na.rm=TRUE)-.7*sd(data$SOB_indicator,na.rm=TRUE)] = -1

data$ACC2_category[data$ACC2 < mean(data$ACC2,na.rm=TRUE)+.7*sd(data$ACC2,na.rm=TRUE)&
                  data$ACC2 > mean(data$ACC2,na.rm=TRUE)-.7*sd(data$ACC2,na.rm=TRUE)] = 0
data$ACC2_category[data$ACC2 > mean(data$ACC2,na.rm=TRUE)+.7*sd(data$ACC2,na.rm=TRUE)] = 1
data$ACC2_category[data$ACC2 < mean(data$ACC2,na.rm=TRUE)-.7*sd(data$ACC2,na.rm=TRUE)] = -1

datac=summarySE(data,measurevar = 'self_estimate2', groupvars = c('ACC2_category','SOB_category'),na.rm=TRUE)

index_remove = vector()
for (i in 1:nrow(datac)){
    #print(any(is.na(datac[i,])))
    if (any(is.na(datac[i,]))){
        index_remove=c(index_remove,i)
    }
}
datac=datac[-c(index_remove),]

pd = position_dodge(0.1)
ggplot(datac,aes(x=SOB_category,y=self_estimate2,colour=ACC2_category,group=ACC2_category))+
geom_errorbar(aes(ymin=self_estimate2-se,ymax=self_estimate2+se),width=.1,colour="black")+
geom_line()+geom_point(shape=21,size=3,fill='white')+xlab('SOB level')+ylab('self estimate')+
scale_color_continuous(name='performance')+
theme_bw()+theme(legend.justification=c(1,0),legend.position=c(1,0))+ggtitle("2nd day data")


