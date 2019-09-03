
### regression models

##red wine
red_wine<-redwine
red_wine<-na.omit(red_wine)
library(stargazer)
red_wine1<-data.frame(scale(red_wine[,1:11]), quality=red_wine[,12])

#compute multicollinearity 
library(usdm)
q<-vif(red_wine1[,-12])
is.num<-sapply(q, is.numeric)
q[is.num] <- lapply(q[is.num], round, 2)
library(tableHTML)
tableHTML(q)

#plot correlation table of all variables with the quality 
cor_quality<-cor(x=red_wine1[,1:11], y=red_wine1$quality)

#linear regression
reg_red<-lm(quality~., data=red_wine1)
summary(reg_red)
reg_red4<-lm(quality~.-fixed.acidity-citric.acid-residual.sugar-density
             #+fixed.acidity:chlorides
             #+fixed.acidity:free.sulfur.dioxide
             +fixed.acidity:pH
             #+fixed.acidity:sulphates
             #+fixed.acidity:alcohol
             +volatile.acidity:total.sulfur.dioxide
             #+citric.acid:density
             +citric.acid:pH
             #+citric.acid:alcohol
             #+residual.sugar:total.sulfur.dioxide
             #+chlorides:density
             #+free.sulfur.dioxide:total.sulfur.dioxide
             #+free.sulfur.dioxide:density
             +free.sulfur.dioxide:sulphates
             #+free.sulfur.dioxide:alcohol
             #+total.sulfur.dioxide:density
             #+total.sulfur.dioxide:pH
             #+total.sulfur.dioxide:alcohol
             #+density:sulphates
             #+pH:sulphates
             
             ,data=red_wine1)
summary(reg_red4)
stargazer(reg_red, reg_red4, type="html", out="new_reg_red4.html")


###White wine
white_wine<-whitewine
white_wine1<-data.frame(scale(white_wine[,1:11]), quality=white_wine[,12])
white_wine2<-white_wine1[-4746,]#remove outlier 
#check multi-colinearity
p<-vif(white_wine1)
is.num<-sapply(p, is.numeric)
p[is.num] <- lapply(p[is.num], round, 2)
tableHTML(p)
#remove density
p<-vif(white_wine1[,-8])
is.num<-sapply(p, is.numeric)
p[is.num] <- lapply(p[is.num], round, 2)
tableHTML(p)

######regression models for white
reg_white<-lm(quality~.-density, data=white_wine2)
reg_white3<-lm(quality~.-citric.acid-density-fixed.acidity
               +fixed.acidity:citric.acid
               +fixed.acidity:residual.sugar
               +volatile.acidity:alcohol
               +free.sulfur.dioxide:total.sulfur.dioxide
               +residual.sugar:alcohol, data=white_wine2)
summary(reg_white3)
AIC(reg_white, reg_white3)
stargazer(reg_white, reg_white3, type="html", out="reg_white3.html")
