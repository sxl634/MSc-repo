### libraries
library(readr) #read csv files into R
library(tibble) #class of object which is like data.frame but better
library(dplyr)
library(pscl) # R2 for binary logistic
library(ROCR) # ROCR curve
library(VGAM) #multinomial regression
library(openxlsx)

##############
# Question 1 #
##############
vocab <- read_csv("GLM/Assignment/vocabulary.csv")
vocab$native <- vocab$native - 1 # Just to match question
vocab <- as_tibble(lapply(vocab, factor)) # make all possible columns a factor
summary(vocab)

#contingency tables
table(vocab$vocab70, vocab$educ)
table(vocab$vocab70, vocab$native)
table(vocab$vocab70, vocab$age)
table(vocab$vocab70, vocab$sex)

table(vocab$educ, vocab$native)
table(vocab$educ, vocab$age)
table(vocab$educ, vocab$sex)

table(vocab$native, vocab$age)
table(vocab$native, vocab$sex)

table(vocab$age, vocab$sex)

v_empty_model <- glm(vocab70~1, family="binomial", data=vocab)
summary(v_empty_model) #AIC - 8246.8

v_native_only <- glm(vocab70~native, family="binomial", data=vocab)
summary(v_native_only) #AIC - 8209.1

v_educ_only <- glm(vocab70~educ, family="binomial", data=vocab)
summary(v_educ_only) #AIC - 7472.2

v_age_only <- glm(vocab70~age, family="binomial", data=vocab)
summary(v_age_only) #AIC - 8175.3

v_sex_only <- glm(vocab70~sex, family="binomial", data=vocab)
summary(v_sex_only) #AIC - 8247.5

anova(v_empty_model, v_native_only, test="LRT")

v_native_educ <- glm(vocab70~native + educ, family="binomial", data=vocab)
summary(v_native_educ) # 7434.2
anova(v_native_educ, v_native_only, test="LRT")

v_native_educ_age <- glm(vocab70~native + educ + age, family="binomial", data=vocab)
summary(v_native_educ_age) # AIC - 7367.2
anova(v_native_educ, v_native_educ_age, test="LRT")

v_native_educ_age_sex <- glm(vocab70~native + educ + age + sex, family="binomial", data=vocab)
summary(v_native_educ_age_sex) # AIC -   7367.5
anova(v_native_educ_age_sex, v_native_educ_age, test="LRT")

#just checking any interactions making sex a useful variable
v_native_educ_age_sex_x_native <- glm(vocab70~native + educ + age + sex * native, family="binomial", data=vocab)
summary(v_native_educ_age_sex_x_native) 
anova(v_native_educ_age_sex_x_native, v_native_educ_age, test="LRT")

v_native_educ_age_sex_x_educ <- glm(vocab70~native + educ + age + sex * educ, family="binomial", data=vocab)
summary(v_native_educ_age_sex_x_educ)
anova(v_native_educ_age_sex_x_educ, v_native_educ_age, test="LRT")

v_native_educ_age_sex_x_age <- glm(vocab70~native + educ + age + sex * age, family="binomial", data=vocab)
summary(v_native_educ_age_sex_x_age)
anova(v_native_educ_age_sex_x_age, v_native_educ_age, test="LRT")

# looking at interactions with native
v_educ_age_native_x_age <- glm(vocab70~native + educ + age + age * native, family="binomial", data=vocab)
summary(v_native_educ_age_sex_x_native) 
anova(v_native_educ_age_sex_x_native, v_native_educ_age, test="LRT")

v_educ_age_native_x_educ <- glm(vocab70~native + educ + age + educ * native, family="binomial", data=vocab)
summary(v_native_educ_age_sex_x_native) 
anova(v_native_educ_age_sex_x_native, v_native_educ_age, test="LRT")

# checking final possible interaction term educ * age
v_native_educ_age_educ_x_age <- glm(vocab70~native + educ + age + educ * age, family="binomial", data=vocab)
summary(v_native_educ_age_educ_x_age)
anova(v_native_educ_age_educ_x_age, v_native_educ_age, test="LRT")

# Final model
vocab_final_model <- glm(vocab70~native + educ + age, family="binomial", data=vocab)
vocab_final_model_summ <- summary(vocab_final_model) # AIC - 7367.2

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
writeDataTable(wb, "Sheet 1", as.data.frame(vocab_final_model_summ$coefficients), rowNames = TRUE)
saveWorkbook(wb, "GLM/Assignment/vocab_coefficients.xlsx", TRUE)

######residuals

r.stand <- rstandard(vocab_final_model) ####standardized residuals
r.pearson <- residuals(vocab_final_model,type=c("pearson"))
r.deviance <- residuals(vocab_final_model,type=c("deviance"))

plot(predict(vocab_final_model),rstandard(vocab_final_model), ylim = c(-2.5,2.5)) ###standardised residuals vs fitted values
abline(h=-1.96, lty=3, col=2)
abline(h=1.96, lty=3, col=2)


vocab.group <- aggregate(vocab70~native + educ + age, data=vocab, 
                        FUN = function(x) c(succ = length(x[x=="1"]), n= length(x)))
vocab.group <- data.frame(vocab.group[,1:3], n=vocab.group$vocab70[,2], 
                         p = vocab.group$vocab70[,1]/vocab.group$vocab70[,2] )
vocab_grouped_model <- glm(p~native + educ + age, data=vocab.group, family="binomial", weights=n)

#compare the outputs
summary(vocab_final_model)
summary(vocab_grouped_model)

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
writeDataTable(wb, "Sheet 1", as.data.frame(summary(vocab_grouped_model)$coefficients), rowNames = TRUE)
saveWorkbook(wb, "GLM/Assignment/vocab_grouped_coefficients.xlsx", TRUE)

#predicted values
vocab_groupdata <- expand.grid(levels(vocab$native),levels(vocab$educ), levels(vocab$age)) 
colnames(vocab_groupdata) <- c("native","educ","age")
vocab_groupdata$prob.ug <- predict(vocab_final_model,vocab_groupdata, type = "response")
vocab_groupdata$prob.g <- predict(vocab_grouped_model,vocab_groupdata, type = "response")
vocab_groupdata

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
writeDataTable(wb, "Sheet 1", vocab_groupdata)
saveWorkbook(wb, "GLM/Assignment/vocab_model_predictions.xlsx", TRUE)


#standardised residuals vs fitted values
plot(predict(vocab_grouped_model),rstandard(vocab_grouped_model))
abline(h=-1.96, lty=3, col=2)
abline(h=1.96, lty=3, col=2)

#predictive power
#R2 measures
pR2(vocab_final_model)
# McFadden
MF_R2 <- 1-logLik(vocab_final_model)/logLik(v_empty_model) 
MF_R2
###cox-snell (r2ML)
n<- nrow(vocab)
CS_R2 <- 1-exp(logLik(v_empty_model)*(2/n)-logLik(vocab_final_model)*(2/n)) 
CS_R2
###nagelkerke(r2CU)
NK_R2 <- r2ML*(1/(1-exp(logLik(v_empty_model)*(2/n)))) 
NK_R2

#Classification tables
vocab_predicted <- fitted(vocab_final_model)
boxplot(vocab_predicted~vocab$vocab70, ylab="Fitted probability", xlab="vocab70",
        ylim=c(0,1))
abline(h=0.5, lty=2, col=2)

########### Classification table using 0.5 as cut-off
c=0.5
vocab_predicted_cut<- cut(vocab_predicted,breaks=c(0,c,1),labels=c(0,1))
aa<-as.matrix(table(vocab_predicted_cut,vocab$vocab70))
(aa[1,1]+aa[2,2])/sum(aa) #proportion of correct classification
aa[2,2]/sum(aa[,2]) #sensitivity for c=0.5
aa[1,1]/sum(aa[,1]) #specificity for c=0.5

########### ROC curve
pred <- data.frame(vocab$vocab70,vocab_predicted)
pred <- pred[order(vocab$vocab70),]
colnames(pred) <- c("vocab70", "vocab_predicted")
pred <- prediction(pred$vocab_predicted,as.numeric(pred$vocab70))
perf<- performance(pred,"tpr", "fpr")
plot(perf); abline(0,1)
performance(pred,"auc")@y.values #area under the curve



##############
# Question 2 #
##############
bank <- read_csv("GLM/Assignment/bank.csv")
bank <- as_tibble(lapply(bank, factor))
summary(bank)

bank_mult<-vglm(interest ~ sex + q.income,family=multinomial(refLevel = "2"), data=bank)
bank_mult_summ <- summary(bank_mult)
bank_mult_summ


exp(bank_mult@coefficients[3])
exp(bank_mult@coefficients[4])

#predicted probabilities
(exp(bank_mult@coefficients[1]+bank_mult@coefficients[5]))/(1+exp(bank_mult@coefficients[1]+bank_mult@coefficients[5])+exp(bank_mult@coefficients[2]+bank_mult@coefficients[6]))
(exp(bank_mult@coefficients[2]+bank_mult@coefficients[6]))/(1+exp(bank_mult@coefficients[1]+bank_mult@coefficients[5])+exp(bank_mult@coefficients[2]+bank_mult@coefficients[6]))
1/(1+exp(bank_mult@coefficients[1]+bank_mult@coefficients[5])+exp(bank_mult@coefficients[2]+bank_mult@coefficients[6]))

bank_mult_exc_income <- vglm(interest ~ sex,family=multinomial(refLevel = "2"), data=bank)
bank_mult_exc_income_summ <- summary(bank_mult_exc_income)

#LRT
sprintf("%#0.4f",bank_mult_summ@criterion$deviance) #sprintf just to display the deviances nicely
sprintf("%#0.4f",bank_mult_exc_income_summ@criterion$deviance)

dev_diff <- bank_mult_exc_income_summ@criterion$deviance-bank_mult_summ@criterion$deviance
sprintf("%#0.4f",dev_diff)
1-pchisq(dev_diff, 6)
#Just checking that the LRT has been done correctly
anova(bank_mult_exc_income, bank_mult, type=1)

##############
# Question 3 #
##############
#####entering the data
medal_data <- tibble(country=c("USA","China","Russia"), athletes = c(530,396,436), medals = c(103,88,81))

# Model with only an intercept
medal_model <- glm( medals ~ offset(log(athletes)), family = "poisson", data = medal_data)
summary(medal_model)
medal_model$coefficients
exp(medal_model$coefficients)

medal_data$expect <- medal_data$athletes*exp(medal_model$coefficients[1])
medal_data$expect_rounded <- round(medal_data$athletes*exp(medal_model$coefficients[1]))

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
writeDataTable(wb, "Sheet 1", medal_data)
saveWorkbook(wb, "GLM/Assignment/medal_data.xlsx", TRUE)

medal_data$ind_dev <- medal_data$medals*log(medal_data$medals/medal_data$expect)
medal_deviance <- 2*sum(medal_data$ind_dev)
sprintf("%#0.6f",medal_deviance)

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
writeDataTable(wb, "Sheet 1", medal_data)
saveWorkbook(wb, "GLM/Assignment/medal_data.xlsx", TRUE)

1-pchisq(medal_deviance,2)

medal_data$ind_chisq <- ((medal_data$medals-medal_data$expect)/sqrt(medal_data$expect))^2
medal_chisq_stat <- sum(medal_data$ind_chisq)
1-pchisq(medal_chisq_stat,2)

