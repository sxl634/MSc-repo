#############################################################
#
#           STAT6090:GENERALISED LINEAR MODELS
#                   COMPUTER WORKSHOP 1
#                          2021/22
#
#############################################################


#############################
# Reading the dataset
#############################

data.reg <- read.table("GLM/attainment.dat", header=T)
head(data.reg)

#check the class of the columns of data.reg
sapply(data.reg, class)

#Convert School, Sex and Ethnic group to factor and name their levels
data.reg$school <- factor(data.reg$school, levels=c(1,2,3))
data.reg$sex <- factor(data.reg$sex, levels=c(0,1), labels=c("Boy","Girl"))
data.reg$eg <- factor(data.reg$eg, levels=c(0,1),labels=c("White","Afr-Car"))

#verify all classes are correct now
sapply(data.reg, class)
head(data.reg)

#make data.reg the default dataset 
attach(data.reg)


#########################################
# Some basic operations with R objects
#########################################

#Find the dimension of the dataset:
dim(data.reg)

#Select the first 10 cases for all variables:
data.reg[1:10,] 

#Select all cases for the fifth variable:
data.reg[,5] 

#Select the first 10 cases for the second variable:
data.reg[1:10,2]

#Select all girls
data.reg[sex=="Girl",]
#If you receive an error this is because you have detached the
#data so R doesn't know where to look for variable sex. 
#If that is the case, attach it again with attach(data.reg)

#Create a new dataset called \texttt{data\_new} containing only the first and third variables:
data.new <- data.reg[,c(1,3)] 
head(data.new)

#Calculate a new variable with the average attainment in maths for each subject:
mathatt.mean <- (mathatt2 + mathatt1)/2
mathatt.mean

data.reg[mathatt1 >= 10,]
#########################################
# A simple exploratory analysis
#########################################

#Descriptive statistics
summary(data.reg)

# Frequency tables
table(school) 
table(sex)

#some histograms 
par(mfrow=c(2,2))#create a 2x2 matrix of histograms filled by row
hist(curric) # Histogram for curriculum in (1,1)
hist(mathatt1)# Histogram for math attainment at year 1 in (1,2)
hist(mathatt2)## Histogram for math attainment at year 2 in (2,1)

#Scatterplots
dev.off() #cleans the plots window. All plots will be deleted!
plot(curric, mathatt1)

#other bi-variate plots
par(mfrow=c(2,2)) 
plot(curric, mathatt2)
plot(mathatt1, mathatt2)
boxplot(mathatt2 ~ school) # Produces box-plots of mathatt2 by school
boxplot(mathatt2 ~ sex)
pairs(data.reg) # Scatterplot matrix between all variables in the dataset


#########################################
# Linear Regression with R
#########################################

###### Model 1
model.1 <- lm(mathatt2 ~ 1) 
summary(model.1)
predict.lm(model.1,interval = "confidence", level = 0.95)[1,]
confint(model.1, level = 0.95)
###### Model 2
model.2 <- lm(mathatt2 ~ curric)
summary(model.2)
anova(model.2)
AIC(model.1)
AIC(model.2)
#E(mathatt2 | curric) = 13.09 + 0.08curric 
# std.err = 3.2490 for intercept and 0.01566 for parameter
#regresion line is significant to 99.9%
#43.17% of variation can be explained using this model
# for every 1 unit curric increases, mathatt2 goes up by 0.08

dev.off()
plot(curric, mathatt2)
abline(model.2, col='red')

########################
#Analysis of residuals

#raw residuals
resid(model.2)

#standardised residuals
rstandard(model.2)

#QQ plot of standardised residuals
qqnorm(rstandard(model.2))
qqline(rstandard(model.2)) 
# This adds the line on which the empirical residuals must lie 
#for normality to hold.

#scatterplot of fitted vs raw residuals
plot(model.2$fitted, resid(model.2))#raw residuals
abline(h=0) #adds a constant line at 0.

#scatterplot of fitted vs standardised residuals
plot(model.2$fitted, rstandard(model.2))#raw residuals
abline(h=0) #adds a constant line at 0.

#automatic diagnostic plots
plot(model.2)

########################
#Adding further variables in the model

model.3 <- lm(mathatt2 ~curric + mathatt1) 
anova(model.3)
summary(model.3)
anova(model.3,model.2)
qqnorm(rstandard(model.3))
qqline(rstandard(model.3)) 
# This adds the line on which the empirical residuals must lie 
#for normality to hold.

#scatterplot of fitted vs raw residuals
plot(model.3$fitted, resid(model.3))#raw residuals
abline(h=0) #adds a constant line at 0.

#scatterplot of fitted vs standardised residuals
plot(model.3$fitted, rstandard(model.3))#raw residuals
abline(h=0) #adds a constant line at 0.

plot(model.3)

########################
#Adding a quaratic term

math.quadr = mathatt1^2 
model.4 <- lm(mathatt2~ curric+mathatt1+math.quadr)
summary(model.4)

anova(model.4, model.3)
########################
#Considering Categorical Explanatory Variables

model.5 <- lm(mathatt2~ sex)
summary(model.5) 

model.6 <- lm(mathatt2 ~curric + mathatt1+sex)
summary(model.6)

anova(model.6, model.5)

########################
#Interaction Variables

model.7 <- lm(mathatt2 ~ curric + mathatt1 + sex*mathatt1)
summary(model.7)

anova(model.7, model.6)

model.7a <- lm(mathatt2 ~curric + mathatt1 + curric*sex)
summary(model.7a)
anova(model.7a, model.6)

#choose model 6

#########################################
# The Anscombe quartet example
#########################################

attach(anscombe)  # Attach the anscombe data frame
anscombe  # View the anscombe data frame

#descriptive statistics
apply(anscombe,2,mean) #means
apply(anscombe,2,var) #variances
cbind(cor(x1,y1),cor(x2,y2),cor(x3,y3),cor(x4,y4)) #correlations

#some plots
pairs(anscombe)  # Scatterplot matrix
par(mfrow=c(2,2)) # Define 2 by 2 multifigure display - filled by rows
plot(x1, y1)
plot(x2, y2)
plot(x3, y3)
plot(x4, y4)

#fitting a linear regression model to each pair of variables
f1 <- lm(y1 ~ x1) # Fit linear model: y1 regressed on x1
f2 <- lm(y2 ~ x2) # Fit linear model: y2 regressed on x2
f3 <- lm(y3 ~ x3) # Fit linear model: y3 regressed on x3
f4 <- lm(y4 ~ x4) # Fit linear model: y4 regressed on x4

#regression coefficients
rbind(f1$coefficients, f2$coefficients, f3$coefficients, f4$coefficients) 

#rsquared
rbind(summary(f1)$r.squared, summary(f2)$r.squared, 
      summary(f3)$r.squared,summary(f1)$r.squared) 

####diagnostic plots. Run each block

#PAIR 1
par(mfrow=c(2, 2)) # Define 1 by 2 multifigure display - filled by rows
plot(x1, y1)
abline(f1, col=2) # Add the linear regression line. col=2 corresponds to red colour
plot(x1, resid(f1))
qqnorm(resid(f1)) # A normal q-q plot to check for skewness, etc.
qqline(resid(f1)) # Adds a line to normal q-q plot
plot.new() #fill the remaining position so the new plot starts in
#a new page

plot(f1)



#PAIR 2
par(mfrow=c(2,2)) # Define 2 by 2 multifigure display - filled by rows
plot(x2, y2)
abline(f2, col=2) # Add the linear regression line
plot(x2, resid(f2))
qqnorm(resid(f2)) # A normal q-q plot to check for skewness, etc.
qqline(resid(f2)) # Add a line to normal q-q plot
plot.new() 

plot(f2)


#PAIR 3
par(mfrow=c(2,2)) # Define 2 by 2 multifigure display - filled by rows
plot(x3, y3)
abline(f3, col=2) # Add the linear regression line
plot(x3, resid(f3))
qqnorm(resid(f3)) # A normal q-q plot to check for skewness, etc.
qqline(resid(f3)) # Add a line to normal q-q plot
plot.new() 

plot(f3)


#PAIR 4
par(mfrow=c(2,2)) # Define 2 by 2 multifigure display - filled by rows
plot (x4, y4)
abline(f4, col=2) # Add the linear regression line
plot(x4, resid(f4))
qqnorm(resid(f4)) # A normal q-q plot to check for skewness, etc.
qqline(resid(f4)) # Add a line to normal q-q plot
plot.new() 

plot(f4)





