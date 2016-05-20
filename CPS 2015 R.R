setwd("/Users/nadiakudos/Desktop/data sets/")
library('foreign')
cps2015 = read.csv("cps2015.csv")
# selecting data
cps2015.include = subset(cps2015, cps2015$age>16)

# subset subset data
cps2015.include.minWage = subset(cps2015.include,cps2015.include$hourwage <= 7.25)
summary(cps2015.include.minWage)


table(cps2015.include.minWage$poverty_indicator)

# refactorize variables
varNames = c("black","asian","hispanic","other","lessHS","HS","somecoll")
cps2015.include.minWage$black=factor(cps2015.include.minWage$black)
cps2015.include.minWage$asian=factor(cps2015.include.minWage$asian)
cps2015.include.minWage$hispanic=factor(cps2015.include.minWage$hispanic)
cps2015.include.minWage$other = factor(cps2015.include.minWage$other)
cps2015.include.minWage$lessHS=factor(cps2015.include.minWage$lessHS)
cps2015.include.minWage$HS=factor(cps2015.include.minWage$HS)
cps2015.include.minWage$somecoll=factor(cps2015.include.minWage$somecoll)
cps2015.include.minWage$sex=factor(cps2015.include.minWage$sex)

# rescale GDP
cps2015.include.minWage$GDP2014 = cps2015.include.minWage$GDP2014/10e6
# regression model
lm.model = lm(poverty_indicator~ln_wage+StateUnemploymentRate+sex+GDP2014+employed+
                black+asian+hispanic+other+lessHS+HS+somecoll,data=cps2015.include.minWage
              )

# print the table
summary(lm.model)
avPlots(lm.model)

