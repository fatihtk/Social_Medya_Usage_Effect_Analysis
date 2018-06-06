#Anova Test Example
library(Publish) 

#Data Ýmport and Outlier Checking
MyData <- read.csv(file="C:/Users/Moonster/Desktop/18-30 YAÞ ARASINDAKÝ BÝREYLERÝN SOSYAL MEDYA KULLANIMI VE  ETKÝLENME SEVÝYELERÝ ANKET SONUÇLARI.csv", header=TRUE, sep=",")

boxplot(MyData$Soru4~MyData$Eðitim, main="Fig.-1: Boxplot of social media spending time for days with education level", col= rainbow(4))

library(ggplot2)

ggplot(MyData$soru5, aes(MyData$Eðitim,MyData$Soru4))+geom_boxplot(aes(col=MyData$Eðitim))+labs(title="Boxplot of social media spending time for days with number of created content as education level")

boxplot.stats(MyData$Soru4[MyData$Eðitim=="Üniversite"])

#Estimation of model

model1<- aov(MyData$Soru4~MyData$Eðitim)

summary(model1)

TukeyHSD(model1, conf.level = 0.99)

plot(TukeyHSD(model1, conf.level = 0.99),las=1, col = "red")

library(gplots)
plotmeans(MyData$Soru4~MyData$Eðitim, main="Fig.-3: Mean Plot with 95% Confidence Interval", ylab = "Social Media Spending Time(Hour)", xlab = "Created Content's Education Level")

#Diagnostic Checking

par(mfrow=c(2,2))
plot(model1)

uhat <-resid(model1)

shapiro.test(uhat)

bartlett.test(MyData$Soru4~MyData$Eðitim)

library(car)
leveneTest(MyData$Soru4~MyData$Eðitim)

#One Sample T-Test Example

mean(MyData$Soru5)

t.test(MyData$Soru5, mu=mean(MyData$Soru5), alternative="less", conf.level=0.99)


#Two sample T-Test Example

t.test(MyData$Soru4,MyData$Soru5,alternative="less", var.equal=TRUE)

t.test(MyData$Soru4,MyData$Soru5,alternative="less")

#Paired Saple T-Test 

t.test(MyData$Soru14,MyData$Soru15,alternative="greater", paired=TRUE)

t.test(MyData$Soru6,MyData$Soru7,alternative="greater", paired=TRUE)

#Confidence Intervals for Means

library(Rmisc)
library(Hmisc)
library(DescTools)
#
CI(MyData$Soru9, ci = 0.95)
#
t.test(MyData$Soru9,
       conf.level=0.95)

#
MeanCI(MyData$Soru9, 
       conf.level=0.95)
#
MeanCI(MyData$Soru9, method="boot", type="norm", R=10000)
#
library(boot)                   

Fun = function(x, index) {  
  return(c(mean(x[index]), 
           var(x[index]) / length(index)))
}

Boot = boot(data=MyData$Soru9,
            statistic=Fun,
            R=10000)

mean(Boot$t[,1])

boot.ci(Boot,
        conf=0.95)
##############################################