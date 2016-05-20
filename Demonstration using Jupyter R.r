
library(foreign)

setwd("/home/adowaconan/Dropbox/NYU/semester/Fall 2015/multivariate + regression/final project")

datatoread = read.spss("projectdata.sav",to.data.frame=TRUE)

data = data.frame(datatoread)

data[data == 999] = NA # tell the program what 999 means, it means missing data
data = subset(data,select = -c(ID))#drop subject No.
datanames=colnames(data)

datanames

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
  print(c(i,k$numofoutliers,"outliers"))
  Table_Table[cnt,1] = cnt
  Table_Table[cnt,2] = k$numofoutliers
  cnt = cnt + 1
  qqnorm(data[,i],main=i)# show normality 
  data[,i]=remove_outliers(data[,i])
  }}
Table_Table = data.frame(Table_Table)
print(Table_Table)

library(psych)

# compute the Sense of beloning indicator
data$SOB_pre = (data$SOBitem1pre+data$SOBitem2pre+data$SOBitem3pre
                +data$SOBitem4pre+data$SOBitem5pre)/5
data$SOB_post=(data$SOBitem1post+data$SOBitem2psot+data$SOBitem3post
               +data$SOBitem4post+data$SOBitem5post)/5
data$SOB_indicator = (data$SOB_post - data$SOB_pre)/(data$SOB_post + data$SOB_pre)
# positive means effort based and negative means ability based sense of belonging

plot(data$SOB_indicator,xlab='subject #',ylab='Normalized SOB')
abline(0,0)

norm_vec <- function(x) sqrt(sum(x^2,na.rm =T)) # function for normalization
V1 = data$Post1Taskfeel1+data$Post1Taskfeel2+data$Post1Taskfeel3 # day1 self-feeling
V2 = data$Post2Taskfeel1+data$Post2Taskfeel2 # day2 self-feeling
data$self_estimate = ((V1 - mean(V1,na.rm=T)) / norm_vec(V1))- ((data$ACC1 -mean(data$ACC1,na.rm=T))/norm_vec(data$ACC1))
data$self_estimate2 = ((V2 - mean(V2,na.rm=T)) / norm_vec(V2))- ((data$ACC2 -mean(data$ACC2,na.rm=T))/norm_vec(data$ACC2))

data$ACC1 = data$ACC1 - mean(data$ACC1,na.rm = T)
data$ACC2 = data$ACC2 - mean(data$ACC2,na.rm = T)
#data$Conf1 = data$Conf1 - mean(data$Conf1,na.rm = T)
#data$Conf2 = data$Conf2 - mean(data$Conf2,na.rm = T)
data$SOB_indicator = data$SOB_indicator / norm_vec(data$SOB_indicator) # rescale the indicator

plot(data$SOB_indicator,xlab='subject #',ylab='Normalized SOB')
abline(0,0)
text(100,0.3,'No much change..',col='red')

describe(data)
cor.ci(data)

ylims=c(-0.02,0.01)
lmDay1.one = lm(self_estimate~ACC1,data=data)
lmDay1.two = lm(self_estimate ~ Conf1+ACC1,data=data)
# interaction
data$ACCconf1 = data$ACC1 * data$Conf1
lmDay1.three = lm(self_estimate ~ Conf1+ACC1+ACCconf1,data=data)
# direct prediction with moderator SOB
lmDay1.four = lm(data$self_estimate~data$Conf1+data$ACC1+data$ACCconf1+data$SOB_indicator)
# interactions
data$ACCSOB1=data$ACC1*data$SOB_indicator
data$confSOB1 = data$Conf1*data$SOB_indicator
data$ACCconfSOB1 = data$ACC1*data$SOB_indicator*data$SOB_indicator
lmDay1.five = lm(self_estimate~Conf1+ACC1+SOB_indicator+ACCconf1+confSOB1+ACCSOB1,data = data)

summary(lmDay1.five)

anova(lmDay1.one,lmDay1.two,lmDay1.three,test="F")
anova(lmDay1.four,lmDay1.five,test="F")

plot(lmDay1.three,which = 1:4)

plot(lmDay1.five,which = 1:4)

which(rstudent(lmDay1.three)>2)
which(rstudent(lmDay1.five)>2)

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

xbar.acc = mean(data$ACC1,na.rm = T)
xbar.conf = mean(data$Conf1,na.rm = T)
s.acc = SD(data$ACC1)
s.conf = SD(data$Conf1)
lowNlow = c(1,-s.conf,-s.acc,(-s.conf*-s.acc)) %*% coef(lmDay1.three)
lowNAve = c(1,-s.conf,0,0) %*% coef(lmDay1.three)
lowNhigh = c(1,-s.conf,s.acc,(-s.conf*s.acc)) %*% coef(lmDay1.three)
AveNlow = c(1,0,-s.acc,(0*-s.acc)) %*% coef(lmDay1.three)
AveNAve = c(1,0,0,0) %*% coef(lmDay1.three)
AveNhigh = c(1,0,s.acc,(0*s.acc)) %*% coef(lmDay1.three)
HighNlow = c(1,s.conf,-s.acc,(s.conf*-s.acc)) %*% coef(lmDay1.three)
HighNAve = c(1,s.conf,0,(s.conf*0))%*% coef(lmDay1.three)
HighNhigh = c(1,s.conf,s.acc,(s.conf*s.acc)) %*% coef(lmDay1.three)
# set performance as the x axis
D = matrix(data = c(lowNlow,lowNAve,lowNhigh,AveNlow,
                    AveNAve,AveNhigh,HighNlow,HighNAve,
                    HighNhigh),nrow=3,ncol=3,byrow = TRUE)

ylims = c(-0.04,0.02)

par(xpd=T, mar=par()$mar+c(0,0,0,6))
par(ps = 12, cex = 1, cex.main = 1)
plot(c(1,2,3),D[1,],type="l",ylim=ylims,col=2,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[2,],type="l",ylim=ylims,col=3,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[3,],type="l",ylim=ylims,col=4,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
title(main="replicate Dunning-Kruger effect in day 1 data")
legend("topright",legend=c("- SD confidence level","average confidence level","+ SD confidence level"),lty = 1,col=c(2,3,4),lwd=2)
abline(v=2)



xbar.acc = mean(data$ACC1,na.rm = T)
xbar.conf = mean(data$Conf1,na.rm = T)
xbar.SOB = mean(data$SOB_indicator,na.rm=T)
s.acc = SD(data$ACC1)
s.conf = SD(data$Conf1)
s.SOB = SD(data$SOB_indicator)
lowNlowNhigh = c(1,-s.conf,-s.acc,s.SOB,(-s.conf*-s.acc),(-s.conf*s.SOB),(-s.acc*s.SOB)) %*% coef(lmDay1.five)
lowNAveNhigh = c(1,-s.conf,0,s.SOB,(-s.conf*0),(-s.conf*s.SOB),(0*s.SOB)) %*% coef(lmDay1.five)
lowNhighNhigh = c(1,-s.conf,s.acc,s.SOB,(-s.conf*s.acc),(-s.conf*s.SOB),(s.acc*s.SOB)) %*% coef(lmDay1.five)
AveNlowNhigh = c(1,0,-s.acc,s.SOB,(0*-s.acc),(-0*s.SOB),(-s.acc*s.SOB)) %*% coef(lmDay1.five)
AveNAveNhigh = c(1,-0,0,s.SOB,(0*0),(0*s.SOB),(0*s.SOB)) %*% coef(lmDay1.five)
AveNhighNhigh = c(1,-0,s.acc,s.SOB,(0*s.acc),(0*s.SOB),(s.acc*s.SOB)) %*% coef(lmDay1.five)
HighNlowNhigh = c(1,s.conf,-s.acc,s.SOB,(s.conf*-s.acc),(s.conf*s.SOB),(-s.acc*s.SOB)) %*% coef(lmDay1.five)
HighNAveNhigh = c(1,s.conf,0,s.SOB,(s.conf*0),(s.conf*s.SOB),(0*s.SOB)) %*% coef(lmDay1.five)
HighNhighNhigh = c(1,s.conf,s.acc,s.SOB,(s.conf*s.acc),(s.conf*s.SOB),(s.acc*s.SOB)) %*% coef(lmDay1.five)
#set performance as x axis
D = matrix(data = c(lowNlowNhigh,lowNAveNhigh,lowNhighNhigh,AveNlowNhigh,
                    AveNAveNhigh,AveNhighNhigh,HighNlowNhigh,HighNAveNhigh,
                    HighNhighNhigh),nrow=3,ncol=3,byrow = TRUE)
ylims = c(-0.03,0.03)
par(xpd=T, mar=par()$mar+c(0,0,0,6))
par(ps = 12, cex = 1, cex.main = 1)
plot(c(1,2,3),D[1,],type="l",ylim=ylims,col=2,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[2,],type="l",ylim=ylims,col=3,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[3,],type="l",ylim=ylims,col=4,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(seq(1,3,length=3),y = array(0,c(1,3)),type="l",ylim=ylims,col=1,xaxt = "n",ylab="self estimate",xlab="performance",lwd=0.1)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
title(main="high SOB - working hard")
legend("topright",legend=c("- SD confidence level","average confidence level","+ SD confidence level"),lty = 1,col=c(2,3,4),lwd=2)
text(2.5,-0.002,'here')
abline(v=2)


lowNlowNlow = c(1,-s.conf,-s.acc,-s.SOB,(-s.conf*-s.acc),(-s.conf*-s.SOB),(-s.acc*-s.SOB)) %*% coef(lmDay1.five)
lowNAveNlow = c(1,-s.conf,0,-s.SOB,(-s.conf*0),(-s.conf*-s.SOB),(0*-s.SOB)) %*% coef(lmDay1.five)
lowNhighNlow = c(1,-s.conf,s.acc,-s.SOB,(-s.conf*s.acc),(-s.conf*-s.SOB),(s.acc*-s.SOB)) %*% coef(lmDay1.five)
AveNlowNlow = c(1,0,-s.acc,-s.SOB,(0*-s.acc),(-0*-s.SOB),(-s.acc*-s.SOB)) %*% coef(lmDay1.five)
AveNAveNlow = c(1,-0,0,-s.SOB,(0*0),(0*-s.SOB),(0*-s.SOB)) %*% coef(lmDay1.five)
AveNhighNlow = c(1,-0,s.acc,-s.SOB,(0*s.acc),(0*-s.SOB),(s.acc*-s.SOB)) %*% coef(lmDay1.five)
HighNlowNlow = c(1,s.conf,-s.acc,-s.SOB,(s.conf*-s.acc),(s.conf*-s.SOB),(-s.acc*-s.SOB)) %*% coef(lmDay1.five)
HighNAveNlow = c(1,s.conf,0,-s.SOB,(s.conf*0),(s.conf*-s.SOB),(0*-s.SOB)) %*% coef(lmDay1.five)
HighNhighNlow = c(1,s.conf,s.acc,-s.SOB,(s.conf*s.acc),(s.conf*-s.SOB),(s.acc*-s.SOB)) %*% coef(lmDay1.five)
# set performance as axis
D = matrix(data = c(lowNlowNlow,lowNAveNlow,lowNhighNlow,AveNlowNlow,
                    AveNAveNlow,AveNhighNlow,HighNlowNlow,HighNAveNlow,
                    HighNhighNlow),nrow=3,ncol=3,byrow = TRUE)

ylims = c(-0.03,0.03)
par(xpd=T, mar=par()$mar+c(0,0,0,6))
par(ps = 12, cex = 1, cex.main = 1)
plot(c(1,2,3),D[1,],type="l",ylim=ylims,col=2,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[2,],type="l",ylim=ylims,col=3,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[3,],type="l",ylim=ylims,col=4,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(seq(1,3,length=3),y = array(0,c(1,3)),type="l",ylim=ylims,col=1,xaxt = "n",ylab="self estimate",xlab="performance",lwd=0.1)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
title(main="low SOB - easy A")
legend("topright",legend=c("- SD confidence level","average confidence level","+ SD confidence level"),lty = 1,col=c(2,3,4),lwd=2)
text(1.75,0.005,'here')
abline(v = 2)

xbar.acc = mean(data$ACC2,na.rm = T)
xbar.conf = mean(data$Conf2,na.rm = T)
s.acc = SD(data$ACC2)
s.conf = SD(data$Conf2)

lowNlow = c(1,-s.conf,-s.acc,(-s.conf*-s.acc)) %*% coef(lmDay2.three)
lowNAve = c(1,-s.conf,0,0) %*% coef(lmDay2.three)
lowNhigh = c(1,-s.conf,s.acc,(-s.conf*s.acc)) %*% coef(lmDay2.three)
AveNlow = c(1,0,-s.acc,(0*-s.acc)) %*% coef(lmDay2.three)
AveNAve = c(1,0,0,0) %*% coef(lmDay2.three)
AveNhigh = c(1,0,s.acc,(0*s.acc)) %*% coef(lmDay2.three)
HighNlow = c(1,s.conf,-s.acc,(s.conf*-s.acc)) %*% coef(lmDay2.three)
HighNAve = c(1,s.conf,0,(s.conf*0))%*% coef(lmDay2.three)
HighNhigh = c(1,s.conf,s.acc,(s.conf*s.acc)) %*% coef(lmDay2.three)
# set performance as x axis
D = matrix(data = c(lowNlow,lowNAve,lowNhigh,AveNlow,
                    AveNAve,AveNhigh,HighNlow,HighNAve,
                    HighNhigh),nrow=3,ncol=3,byrow = TRUE)

ylims = c(-0.11,0.03)
par(xpd=T, mar=par()$mar+c(0,0,0,6))
par(ps = 12, cex = 1, cex.main = 1)
plot(c(1,2,3),D[1,],type="l",ylim=ylims,col=2,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[2,],type="l",ylim=ylims,col=3,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[3,],type="l",ylim=ylims,col=4,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
title(main="replicate Dunning-Kruger effect in day 2 data")
legend("topright",legend=c("- SD confidence level","average confidence level","+ SD confidence level"),lty = 1,col=c(2,3,4),lwd=2)


xbar.acc = mean(data$ACC2,na.rm = T)
xbar.conf = mean(data$Conf2,na.rm = T)
xbar.SOB = mean(data$SOB_indicator,na.rm=T)
s.acc = SD(data$ACC2)
s.conf = SD(data$Conf2)
s.SOB = SD(data$SOB_indicator)

# easy A
lowNlowNlow = c(1,-s.conf,-s.acc,-s.SOB,(-s.conf*-s.acc),(-s.conf*-s.SOB),(-s.acc*-s.SOB)) %*% coef(lmDay2.five)
lowNAveNlow = c(1,-s.conf,0,-s.SOB,(-s.conf*0),(-s.conf*-s.SOB),(0*-s.SOB)) %*% coef(lmDay2.five)
lowNhighNlow = c(1,-s.conf,s.acc,-s.SOB,(-s.conf*s.acc),(-s.conf*-s.SOB),(s.acc*-s.SOB)) %*% coef(lmDay2.five)
AveNlowNlow = c(1,0,-s.acc,-s.SOB,(0*-s.acc),(-0*-s.SOB),(-s.acc*-s.SOB)) %*% coef(lmDay2.five)
AveNAveNlow = c(1,-0,0,-s.SOB,(0*0),(0*-s.SOB),(0*-s.SOB)) %*% coef(lmDay2.five)
AveNhighNlow = c(1,-0,s.acc,-s.SOB,(0*s.acc),(0*-s.SOB),(s.acc*-s.SOB)) %*% coef(lmDay2.five)
HighNlowNlow = c(1,s.conf,-s.acc,-s.SOB,(s.conf*-s.acc),(s.conf*-s.SOB),(-s.acc*-s.SOB)) %*% coef(lmDay2.five)
HighNAveNlow = c(1,s.conf,0,-s.SOB,(s.conf*0),(s.conf*-s.SOB),(0*-s.SOB)) %*% coef(lmDay2.five)
HighNhighNlow = c(1,s.conf,s.acc,-s.SOB,(s.conf*s.acc),(s.conf*-s.SOB),(s.acc*-s.SOB)) %*% coef(lmDay2.five)
# set performance as axis
D = matrix(data = c(lowNlowNlow,lowNAveNlow,lowNhighNlow,AveNlowNlow,
                    AveNAveNlow,AveNhighNlow,HighNlowNlow,HighNAveNlow,
                    HighNhighNlow),nrow=3,ncol=3,byrow = TRUE)

ylims = c(-0.11,0.08)
par(xpd=T, mar=par()$mar+c(0,0,0,6))
par(ps = 12, cex = 1, cex.main = 1)
plot(c(1,2,3),D[1,],type="l",ylim=ylims,col=2,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[2,],type="l",ylim=ylims,col=3,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[3,],type="l",ylim=ylims,col=4,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(seq(1,3,length=3),y = array(0,c(1,3)),type="l",ylim=ylims,col=1,xaxt = "n",ylab="self estimate",xlab="performance",lwd=0.1)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
title(main="low SOB - easy A")
legend("topright",legend=c("- SD confidence level","average confidence level","+ SD confidence level"),lty = 1,col=c(2,3,4),lwd=2)
abline(v=2)
text(1.5,0,'here')

lowNlowNhigh = c(1,-s.conf,-s.acc,s.SOB,(-s.conf*-s.acc),(-s.conf*s.SOB),(-s.acc*s.SOB)) %*% coef(lmDay2.five)
lowNAveNhigh = c(1,-s.conf,0,s.SOB,(-s.conf*0),(-s.conf*s.SOB),(0*s.SOB)) %*% coef(lmDay2.five)
lowNhighNhigh = c(1,-s.conf,s.acc,s.SOB,(-s.conf*s.acc),(-s.conf*s.SOB),(s.acc*s.SOB)) %*% coef(lmDay2.five)
AveNlowNhigh = c(1,0,-s.acc,s.SOB,(0*-s.acc),(-0*s.SOB),(-s.acc*s.SOB)) %*% coef(lmDay2.five)
AveNAveNhigh = c(1,-0,0,s.SOB,(0*0),(0*s.SOB),(0*s.SOB)) %*% coef(lmDay2.five)
AveNhighNhigh = c(1,-0,s.acc,s.SOB,(0*s.acc),(0*s.SOB),(s.acc*s.SOB)) %*% coef(lmDay2.five)
HighNlowNhigh = c(1,s.conf,-s.acc,s.SOB,(s.conf*-s.acc),(s.conf*s.SOB),(-s.acc*s.SOB)) %*% coef(lmDay2.five)
HighNAveNhigh = c(1,s.conf,0,s.SOB,(s.conf*0),(s.conf*s.SOB),(0*s.SOB)) %*% coef(lmDay2.five)
HighNhighNhigh = c(1,s.conf,s.acc,s.SOB,(s.conf*s.acc),(s.conf*s.SOB),(s.acc*s.SOB)) %*% coef(lmDay2.five)

#set performance as x axis
D = matrix(data = c(lowNlowNhigh,lowNAveNhigh,lowNhighNhigh,AveNlowNhigh,
                    AveNAveNhigh,AveNhighNhigh,HighNlowNhigh,HighNAveNhigh,
                    HighNhighNhigh),nrow=3,ncol=3,byrow = TRUE)

ylims = c(-0.11,0.08)
par(xpd=T, mar=par()$mar+c(0,0,0,6))
par(ps = 12, cex = 1, cex.main = 1)
plot(c(1,2,3),D[1,],type="l",ylim=ylims,col=2,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[2,],type="l",ylim=ylims,col=3,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(c(1,2,3),D[3,],type="l",ylim=ylims,col=4,xaxt = "n",ylab="self estimate",xlab="performance",lwd=2)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
par(new=T)
plot(seq(1,3,length=3),y = array(0,c(1,3)),type="l",ylim=ylims,col=1,xaxt = "n",ylab="self estimate",xlab="performance",lwd=0.1)
axis(1,at=seq(1,3,0.5),labels = c("-1 SD performance","","average performance","","+ 1 SD performance"))
title(main="high SOB - working hard")
legend("topright",legend=c("- SD confidence level","average confidence level","+ SD confidence level"),lty = 1,col=c(2,3,4),lwd=2)
abline(v=2)
text(1.5,0,'here')

library(car)

summary(lmDay2.five)
avPlots(lmDay2.five)




