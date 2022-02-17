#############################################################
#
#           STAT6090:GENERALISED LINEAR MODELS
#                   COMPUTER WORKSHOP 2
#                          2021/22
#
#############################################################

########################################################
########################################################
# 1. BINARY LOGISTIC REGRESSION 
########################################################
########################################################
haven::read_stata("GLM/logistic.dat")
data=read.table("GLM/logistic.dat",header=T,
                colClasses = c(rep("factor",3),"numeric"))
###colClasses indicates the type of the variable

levels(data$hgsex)<- c("Male", "Female")
levels(data$hlstat)<- c("Excellent", "Good", "Average", "Poor", "Very Poor")
levels(data$vs2gp)<- c("<3", "3+")
head(data)

########################################################
#Fitting of a binary logistic regression in R
########################################################

mod0 <- glm(vs2gp~1, family="binomial", data=data)
summary(mod0)


#fitted probabilities for all individuals
fitted(mod0)
#or given that it only contains an intercept
predict(mod0, newdata=data.frame(1), type="response")

mod1 <- glm(vs2gp~hgsex, family="binomial", data=data)
summary(mod1)

exp(-1.03279+0.83474)/(1+exp(-1.03279+0.83474))
exp(-1.03279+0*0.83474)/(1+exp(-1.03279+0*0.83474))
exp(0.83474)
#deviance for this model = 7911.0
#It is better than null model, but don't know scale as it is not grouped data

mod1prob <- data
mod1$predprob <- fitted(mod1)
########################################################
#Adding explanatory variables
########################################################

mod2 <- glm(vs2gp~hgsex+hlstat, family="binomial", data=data)
summ_mod2 <- summary(mod2)
summ_mod2
#H0 is that including hlstat does not improve the model and the deviance is not significantly different
#H1 is that including hlstat does improve the odel and the deviance is statistically significantly lower
anova(mod1,mod2, test = "LRT")

sprintf(1-pchisq(7911.0-6707.3,4), fmt = '%#.20f')

#intercept
exp(summ_mod2$coefficients[1,1])
#Female
exp(summ_mod2$coefficients[2,1])
#Good
exp(summ_mod2$coefficients[3,1])
#average
exp(summ_mod2$coefficients[4,1])
#poor
exp(summ_mod2$coefficients[5,1])
#v. poor
exp(summ_mod2$coefficients[6,1])

mod2prob <- data
mod2prob$predprob <- fitted(mod2)

exp(summ_mod2$coefficients[1,1] + summ_mod2$coefficients[2,1] + summ_mod2$coefficients[3,1]) / (1 + exp(summ_mod2$coefficients[1,1] + summ_mod2$coefficients[2,1] + summ_mod2$coefficients[3,1]))

mod2a <- glm(vs2gp~hgsex+hlstat+linc, family="binomial", data=data)
summ_mod2a <- summary(mod2a)
summ_mod2a

anova(mod2,mod2a, test = "LRT")
#keep linc at 5% significance


#to obtain all fitted probabilities
#new dataset with all possible covariate patterns
newdata <- expand.grid(levels(data$hlstat),levels(data$hgsex)) 
colnames(newdata) <- c("hlstat","hgsex")
newdata$logit <- predict(mod2,newdata, type = "link")
newdata$prob <- predict(mod2,newdata, type = "response")
newdata
test <- data
test$logit <- predict(mod2,test, type = "link")
test$prob <- predict(mod2,test, type = "response")
unique(test[,c(2:3,5:ncol(test))]) |> arrange(hgsex, hlstat)

mod3 <- glm(vs2gp~hgsex+hlstat+linc, family="binomial", data=data)
summary(mod3)
anova(mod2, mod3, test="LRT")
sprintf(1-pchisq(6707.3-6698.2,1), fmt = '%#.6f')

########################################################
#Diagnostics and summary of predictive power
########################################################

######fitted values 

fitted(mod3) ####fitted probabilities
head(fitted(mod3)) #only first 6 observations

head(predict(mod3, type="response")) ####fitted probabilities first 6 individuals
head(predict(mod3, type="link")) ####fitted logit scale first 6 individuals

######residuals

r.stand <- rstandard(mod3) ####standardized residuals
r.pearson <- residuals(mod3,type=c("pearson"))
r.deviance <- residuals(mod3,type=c("deviance"))

plot(predict(mod3),rstandard(mod3)) ###standardised residuals vs fitted values
abline(h=-1.96, lty=3, col=2)
abline(h=1.96, lty=3, col=2)

plot(data$linc,rstandard(mod3)) ###standardised residuals vs covariates
abline(h=-1.96, lty=3, col=2)
abline(h=1.96, lty=3, col=2)

######predictive power

library(pscl)

#R2 measures
pR2(mod3)
# McFadden
1-logLik(mod3)/logLik(mod0) 
###cox-snell (r2ML)
n<- nrow(data)
r2ML <- 1-exp(logLik(mod0)*(2/n)-logLik(mod3)*(2/n)) 
r2ML
###nagelkerke(r2CU)
r2CU <- r2ML*(1/(1-exp(logLik(mod0)*(2/n)))) 
r2CU

#Classification tables
YhatP <- fitted(mod3)
boxplot(YhatP~data$vs2gp, ylab="Fitted probability", xlab="Y",
        ylim=c(0,1))
abline(h=0.5, lty=2, col=2)

########### Classification table using 0.5 as cut-off
c=0.5
Yfac<- cut(YhatP,breaks=c(0,c,1),labels=c(0,1))
aa<-as.matrix(table(Yfac,data$vs2gp))
(aa[1,1]+aa[2,2])/sum(aa) #proportion of correct classification
aa[2,2]/sum(aa[,2]) #sensitivity for c=0.5
aa[1,1]/sum(aa[,1]) #specificity for c=0.5

########### ROC curve
#install.packages("ROCR") #if not installed, run this first
library(ROCR)
pred <- data.frame(data$vs2gp,YhatP)
pred <- pred[order(data$vs2gp),]
colnames(pred) <- c("vs2gp", "YhatP")
pred <- prediction(pred$YhatP,as.numeric(pred$vs2gp))
perf<- performance(pred,"tpr", "fpr")
plot(perf); abline(0,1)
performance(pred,"auc")@y.values #area under the curve

########################################################
########################################################
# 2. MULTINOMIAL REGRESSION 
########################################################
########################################################

library(VGAM)

########################################################
#Fitting of a multinomial regression in R
########################################################

#empty model
mult0<-vglm(hlstat~1,family=multinomial(refLevel = 1), data=data)
mult0_summ <- summary(mult0)

#log(p2/p1) = 0.66054
#log(p3/p1) = -0.19235 
#log(p4/p1) = -1.50667
#log(p5/p1) = -2.98478
exp(coef(mult0))
#notice the presence of Hauck-Donner effect
#predicted odds under this model
exp(predict(mult0,data.frame(1)))
#observed proportions in each category
prop.table(table(data$hlstat))

#model including sex
mult<-vglm(hlstat~hgsex,family=multinomial(refLevel = 1), data=data)
summary(mult)

#predicted odds ratios
exp(coefficients(mult))

#predicted probabilities
newdata <- expand.grid(levels(data$hgsex)) #new dataset with all possible covariate patterns
colnames(newdata) <- c("hgsex")
cbind(newdata, predict(mult,newdata, type = "response"))

#LRT
mult.linc<-vglm(hlstat~hgsex+linc,family=multinomial(refLevel = 1),
                data=data)
summary(mult.linc)

anova(mult, mult.linc, type=1)

########################################################
########################################################
# 3. ORDINAL RESPONSES: CUMULATIVE LOGIT MODEL 
########################################################
########################################################

ord<-vglm(as.ordered(hlstat)~hgsex, family=cumulative(parallel = T), 
            data=data)
summary(ord)

#predicted cumulative odds ratios
exp(coefficients(ord))

#predicted probabilities
newdata <- expand.grid(levels(data$hgsex)) #new dataset with all possible covariate patterns
colnames(newdata) <- c("hgsex")
cbind(newdata, predict(ord,newdata, type = "response"))

#model without proportional odds assumption
ord.np<-vglm(as.ordered(hlstat)~hgsex,family=cumulative(parallel = F), 
             data=data)
summary(ord.np)

anova(ord, ord.np, type=1)

#check deviances
summary(ord.np)
summary(mult)

#predicted probabilities:
#cumulative logit without proportional odds assumption
cbind(newdata, predict(ord.np,newdata,type="r"))
#multinomial model
cbind(newdata,predict(mult,newdata,type="response"))

########################################################
########################################################
# 4. ADDITIONAL EXERCISES 
########################################################
########################################################

###############################
#4.1.1 Grouped data
###############################

#model ungrouped data
mod2 <- glm(vs2gp~hgsex+hlstat, family="binomial", data=data)
summary(mod2)

#model grouped data
data.group <- aggregate(vs2gp~hgsex+hlstat, data=data, 
                        FUN = function(x) c(succ = length(x[x=="3+"]), n= length(x)))
data.group <- data.frame(data.group[,1:2], n=data.group$vs2gp[,2], 
                         p = data.group$vs2gp[,1]/data.group$vs2gp[,2] )
mod2g <- glm(p~hgsex+hlstat, data=data.group, family="binomial", weights=n)

#compare the outputs
summary(mod2)
summary(mod2g)

#predicted values
newdatag <- expand.grid(levels(data$hlstat),levels(data$hgsex)) 
colnames(newdatag) <- c("hlstat","hgsex")
newdatag$prob.ug <- predict(mod2,newdatag, type = "response")
newdatag$prob.g <- predict(mod2g,newdatag, type = "response")
newdatag

#standardised residuals vs fitted values
plot(predict(mod2g),rstandard(mod2g))
abline(h=-1.96, lty=3, col=2)
abline(h=1.96, lty=3, col=2)


########################################
#4.1.2 hlstat as a continuous predictor
########################################

#verifying the linearity of the relationship
tab <- prop.table(table(data$hlstat,data$vs2gp),1)
obs.logit <- log(tab[,2]/tab[,1])
plot(obs.logit, xlab="hlstat", ylab="logit", col=4, pch=20,
        xlim=c(0,6), ylim=c(-3,3) )
abline(lm(obs.logit~seq(1:5)))

#fitting the model with hlstat as continuous
mod2.c <- glm(vs2gp ~ hgsex + as.numeric(hlstat), family="binomial", 
              data=data)
summary(mod2.c)
summary(mod2)
plot(fitted(mod2.c), fitted(mod2))
abline(0,1)

#fitted probabilities
newdata <- expand.grid(levels(data$hlstat),levels(data$hgsex)) 
colnames(newdata) <- c("hlstat","hgsex")
newdata$prob.cat <- predict(mod2,newdata, type = "response")
newdata$prob.cont <- predict(mod2.c,newdata, type = "response")
newdata

########################################
#4.1.3 Probit regression with R 
########################################

####Binary Logistic regression model	
mod_logistic <- glm(vs2gp~ hgsex, family=binomial, data=data)
mod_logistic

####Probit regression model	
mod_probit <- glm(vs2gp~ hgsex, family=binomial(link="probit"), 
                  data=data)
mod_probit

####fitted probabilities
newdata <- expand.grid(levels(data$hgsex)) 
colnames(newdata) <- c("hgsex")
pr_logit <- predict(mod_logistic,newdata,type="response")
pr_probit <- predict(mod_probit,newdata,type="response")
cbind(hgsex=c(0,1),pr_probit,pr_logit)

###############################################
#4.2.1 More on the proportional odds assumption 
###############################################

#with paralell assumption for linc
ord2<-vglm(as.ordered(hlstat) ~ hgsex+linc, 
           family=cumulative(parallel = T), data=data)
summary(ord2)


#without paralell assumption for linc
ord3 <- vglm(as.ordered(hlstat) ~ hgsex+linc, 
             family=cumulative(parallel = F~hgsex), data=data)
summary(ord3)

anova(ord2, ord3, type=1)


