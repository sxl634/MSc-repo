#############################################################
#
#           STAT6090:GENERALISED LINEAR MODELS
#                   COMPUTER WORKSHOP 3
#                          2021/22
#
#############################################################

########################################################
########################################################
#
# 2. POISSON REGRESSION MODEL 
#         
########################################################
########################################################

########################################################
# 2.1 Words spoken by an 18 month old
########################################################


#####entering the data
words <- c(92,147,105,80,95,87,119,98,109,96)
weight <- c(30,75,25,50,50,50,80,25,50,10)
siblings <- c(0,1,1,0,0,0,1,1,1,0)

# Model with only an intercept
mod.null <- glm(words~1, family="poisson")
summary(mod.null)

# What is the expected value of the number of words under the null model?
exp(mod.null$coefficients)
# alternatively use the fitted values under the model produced by \textit{R}
mod.null$fitted.values

# Model that includes the siblings variable
mod.siblings <- glm(words~siblings, family="poisson")
summary(mod.siblings)

# Is the variable siblings significant in predicting the number of words?
# check the Z test for the variable
# What is expected increase in the number of words due to having siblings? Use the following command to answer the question
exp(mod.siblings$coefficients[2])

# Model including both the sibling and weight variables
mod.sw <- glm(words~siblings+weight, family="poisson")
sum_mod.sw <- summary(mod.sw)
anova(mod.siblings, mod.sw, test="LRT")

#profile likelihood
confint(mod.sw)

plot(mod.sw)

exp(mod.sw$coefficients[1])
exp(mod.sw$coefficients[2])
exp(mod.sw$coefficients[3])
exp(predict(mod.sw, data.frame(siblings = c(0,0), weight = c(50,75))))
exp(mod.sw$coefficients[1] + mod.sw$coefficients[3] * 50)
exp(mod.sw$coefficients[1] + mod.sw$coefficients[3] * 75)

exp(mod.sw$coefficients[2]) - qnorm(0.05/2, lower.tail = FALSE) * sum_mod.sw$coefficients[2,2]
exp(mod.sw$coefficients[2]) + qnorm(0.05/2, lower.tail = FALSE) * sum_mod.sw$coefficients[2,2]

########################################################
# 2.2 Heart valve example
########################################################

#####entering the data
expand.grid(c(1,2), c(1,2))

# Now do the same with character vectors
Table <- expand.grid(c("aortic", "mitral"), c("<55", "55+"))
Table # Note that table with a lower case t is an R function, so it is better 
# to avoid using it as a variable name

# Construct a data frame with the factors, deaths and exposures
Table <- data.frame(Table, deaths=c(4, 1, 7, 9), exposure=c(1259, 2082, 1417, 1647))
Table
names(Table)[1:2] <- c("valve", "age") # Name the factors
Table$rate = Table$deaths/Table$exposure # Add rates to Table

attach(Table)

#model age.valve
mod.age.valve <- glm(deaths ~ valve + age + offset(log(exposure)), 
                       family = "poisson")
summary(mod.age.valve)
#expected death counts
predict(mod.age.valve, type="response")

#expected death rates
(1000/exposure)*predict(mod.age.valve, type="response")

#Rate ratio for valve
exp(coefficients(mod.age.valve)[2])

#deviance statistic
summary(mod.age.valve) # The residual deviance is the L^2 statistic
1-pchisq(3.2225, 1)

# To calculate the X^2 statistic, sum the squares of the Pearson residuals
sum(resid(mod.age.valve, type = "pearson")^2)
1-pchisq(3.115138, 1)

###

#model valve
heart.valve <- glm(deaths ~ valve + offset(log(exposure)), 
                 family = "poisson")
summary(heart.valve)
1-pchisq(9.8857, 2)

#model age
mod.age <- glm(deaths ~ age + offset(log(exposure)), 
                     family = "poisson")
anova(mod.age,mod.age.valve, test="Chisq")
summary(mod.age)
1-pchisq(3.7897, 2)

########################################################
# 2.3 Lung cancer rates example
########################################################

#illustration of function rep
rep(5, 3)
rep(1:3, 3:1) # "1" is repeated 3 times, then "2" twice and then "3" once

########entering the data

city <- factor( rep(c( "1", "2", "3", "4" ), 6 ) )
city # c( "1", "2", "3", "4" ) is repeated 6 times

age <- factor( rep( c("1", "2", "3", "4", "5", "6"), c(4, 4, 4, 4, 4, 4) ) )
age
# The first element of c("1", "2", "3", "4", "5", "6") is repeated 4 times, then
# the second, etc. Note that c(4, 4, 4, 4, 4, 4) could be replaced by rep(4, 6)

# Vector of cancer cases, 1st row, 2nd row, etc
cancer.cases <- c(11, 13, 4, 5, 11, 6, 8, 7, 11, 15, 7, 10, 10, 10, 11, 14, 11,
                  12, 9, 8, 10, 2, 12, 7)

# Look at cancer cases as a matrix
matrix(cancer.cases, ncol = 4, byrow = T) # T is short for TRUE

# Vector of inhabitants, 1st row, 2nd row, etc
inhabitants <- c(3059, 2879, 3142, 2520, 800, 1083, 1050, 878, 710, 923, 895, 839,
                 581, 834, 702, 631, 509, 634, 535, 539, 605, 782, 659, 619)

# Look at inhabitants as a matrix
matrix(inhabitants,ncol = 4, byrow = T)

#checking margins
rowSums(matrix(cancer.cases, ncol = 4, byrow = T)) # T is short for TRUE
colSums(matrix(cancer.cases, ncol = 4, byrow = T)) # T is short for TRUE

#Observed rates per 1000 as a matrix
round( 
  matrix( (cancer.cases/inhabitants)*1000,ncol = 4, byrow = T ), 
  2)

############ fitting sequence of models

# CONSTANT RATES MODEL
lung.constant <- glm( cancer.cases ~ offset( log(inhabitants) ), family = "poisson" )
summary(lung.constant)

round( matrix( fitted(lung.constant), ncol = 4, byrow = T ), 2 )
round( matrix( ( fitted(lung.constant)/inhabitants )*1000, ncol = 4, byrow = T ), 2 )

  # Calculate X^2 in two different ways
sum( residuals(lung.constant, type = "pearson")^2 )
sum( ( cancer.cases-fitted(lung.constant) )^2 / fitted(lung.constant) )

# AGE EFFECTS MODEL
lung.age <- glm( cancer.cases ~ age + offset( log(inhabitants) ), family = "poisson" )
summary( lung.age)

round( matrix(fitted(lung.age), ncol = 4, byrow = T ), 2 )
round( matrix( (fitted(lung.age)/inhabitants)*1000, ncol = 4, byrow = T ), 2 )

X2.age <- sum(residuals(lung.age,type="pearson")^2); X2.age
1-pchisq(X2.age, 18)

anova(lung.constant, lung.age, test = "Chisq")

# CITY EFFECTS MODEL
lung.city <- glm( cancer.cases ~ city + offset( log(inhabitants) ), family = "poisson" )
summary(lung.city)

X2.city <- sum( residuals(lung.city,type="pearson")^2 ); X2.city

round( matrix( fitted(lung.city), ncol = 4, byrow = T ), 2 )
round( matrix( ( fitted(lung.city)/inhabitants )*1000, ncol = 4, byrow = T ), 2 )

# Compare the constant model with the city effects model

anova(lung.constant, lung.city, test = "Chisq")

# ADDITIVE EFFECTS MODEL, intercept and age1 = 0 and city1 = 0
lung.age.city <- glm( cancer.cases ~ age + city +
                                   offset( log(inhabitants) ), family = "poisson" )
summary(lung.age.city)

round( matrix( fitted(lung.age.city), ncol = 4, byrow = T ), 2 )
round( matrix( ( fitted(lung.age.city)/inhabitants )*1000, ncol=4,
               byrow=T ), 2 )

anova(lung.age, lung.age.city,test = "Chisq")

# SATURATED MODEL

lung.saturated1 <- glm( cancer.cases ~ age * city + offset( log(inhabitants) ),
                        family = "poisson" )
summary(lung.saturated1, corr = F)

round( matrix( fitted(lung.saturated1), ncol = 4, byrow = T ), 2 )
round( matrix( ( fitted(lung.saturated1)/inhabitants )*1000, ncol = 4, byrow = T ), 2 )

# We can compare each model with the saturated model

anova(lung.constant, lung.saturated1, test = "Chisq")
anova(lung.city, lung.saturated1, test = "Chisq")
anova(lung.age, lung.saturated1, test = "Chisq")
anova(lung.additive.intercept1, lung.saturated1, test = "Chisq")


########################################################
# 3 Loglinear models
########################################################

# As the factor values repeat, it is more practical to enter the data using the
# expand.grid function to create a data frame
# LETTERS[1:6] are the first 6 capital letters, i.e., A, B, C, D, E, F
berk.data <- expand.grid( admit = c( "yes", "no" ),
                          sex=c( "male", "female" ), dept = LETTERS[1:6] )
berk.data

# Build a data frame by combining the existing data frame with a vector of counts
berk.data <- cbind.data.frame(berk.data, count = c(512, 313, 89, 19, 353, 207, 17,
                                                   8, 120, 205, 202, 391,138, 279, 131, 244, 53,138, 94, 299, 22, 351, 24, 317 ))
berk.data

# Note that expand.grid automatically defines admit, sex and dept as factors with
# the levels ordered as given rather than alphabetically
attributes(berk.data$admit) 
attributes(berk.data$sex) 
attributes(berk.data$dept)

# Display the counts in an array format
berk.table <- array(berk.data$count, dim = c(2,2,6), 
                    dimnames = list(admit=c("yes","no"),sex=c("male","female"),
                                    dept=LETTERS[1:6]))
berk.table



######################################
# Mutual independence model
######################################

fit.mutual <- glm( count ~ admit + sex + dept, family = poisson, data=berk.data)
summary( fit.mutual) 


######################################
# Joint independence model 
######################################


fit.sa <- glm( count ~ sex + admit + dept + sex:admit, family = poisson, data=berk.data)
summary(fit.sa)

#Instead of indicating all the terms in this model, you could fit the same model 
#specifying only the generating class 
fit.sa <- glm( count ~ sex*admit+dept, family = poisson, data=berk.data)
summary(fit.sa)


fit.da <- glm( count ~ admit + sex + dept + dept:admit, family = poisson, data=berk.data)
summary(fit.da)

fit.ds <- glm( count ~ admit + sex + dept + dept:sex, family = poisson, data=berk.data)
summary(fit.ds)


######################################
#Conditional independence models
######################################

fit.ds.da <- glm( count ~ dept*sex + dept*admit, family = poisson, data=berk.data)
summary(fit.ds.da)

fit.ds.sa <- glm( count ~ dept*sex + sex*admit, family = poisson, data=berk.data)
summary( fit.ds.sa)

fit.sa.da <- glm( count ~ sex*admit + dept*admit, family = poisson, data=berk.data)
summary( fit.sa.da )

######################################
# Homogeneous association model
######################################

fit.all.pairs <- glm( count ~ (sex + dept + admit)^2, family = poisson, data=berk.data)
summary(fit.all.pairs)


######################################
# Saturated model
######################################


fit.saturated<- glm(count ~ dept*sex*admit, family = poisson, data=berk.data)
summary( fit.saturated)

fitted.saturated<- array(fitted(fit.saturated), dim = c(2,2,6), dimnames=dimnames(berk.table))
#notice that the saturated model fits perfectly the data!
fitted.saturated - berk.table


######################################
# Deviance table
######################################


dev.mod <- function(model){
  temp <- anova(model, fit.saturated, test = "LRT")
  c(round(temp$Deviance[2],2),temp$Df[2])
}

res <- NULL
models <- c("fit.all.pairs","fit.ds.sa","fit.sa.da","fit.ds.da","fit.da", "fit.ds","fit.sa","fit.mutual")
#models <- c(fit.all.pairs,fit.ds.sa,fit.sa.da,fit.ds.da,fit.da, fit.ds,fit.sa,fit.mutual)
for (i in 1:length(models))
  res <- rbind(res,dev.mod(get(models[i])))

res <- data.frame(models, res)
colnames(res) <- c("Model", "Deviance", "Df")
res

######################################
# Residual diagnostics
######################################


# Reconsider the model of sex & admission conditionally independent given department
# Investigate lack of fit using residual diagnostics
# As R does not calculate those automatically, we will first calculate 
# the pearson residuals and then divide them by the leverage h

pearson.resids <- residuals( fit.ds.da, type = "pearson" )
h <- lm.influence( fit.ds.da )$hat
adj.residuals <- pearson.resids / sqrt( 1 - h )
round(array(adj.residuals, dim = c(2,2,6), dimnames = dimnames(berk.table)),2)


# Calculate observed odds ratios for each department
or <- matrix(NA,6,1)
for(i in 1:6) or[i] <-  berk.table[1,1,i] * berk.table[2,2,i] /
                       (berk.table[1,2,i] * berk.table[2,1,i]) 
or


# Create new dataset excluding department A
berk.data.new <- berk.data[berk.data$dept!="A",]
# Fit the model SEX & ADMISSION CONDITIONALLY INDEPENDENT, GIVEN DEPARTMENT
# dept*sex + dept*admit = dept*(sex + admit) MODEL
fit.ds.da.new <- glm( count ~ dept*sex + dept*admit,
                      family = poisson , data=berk.data.new)
summary( fit.ds.da.new)
fit.saturated.new<- glm( count ~ dept*sex*admit, family = poisson, data=berk.data.new )
summary( fit.saturated.new )
anova(fit.ds.da.new,fit.saturated.new, test="LRT")


#Understanding the association between sex, admissions and department (B-F) 
obs <- array( berk.data.new$count, dim = c(2,2,5), dimnames =
                list(admit=c( "yes", "no" ), sex=c( "male", "female" ), 
                     dept=LETTERS[2:6]))
prop.table(apply(obs,c(1,3),sum),2) # Conditional distribution of admission given department
prop.table(apply(obs,c(2,3),sum),2) # Conditional distribution of sex given department
prop.table(apply(obs,c(1,2),sum),2) # Conditional distribution of admissions given sex
