### libraries
library(readr) #read csv files into R
library(tibble) #class of object which is like data.frame but better
library(VGAM) #multinomial regression

##############
# Question 1 #
##############
vocab <- read_csv("GLM/Assignment/vocabulary.csv")


##############
# Question 2 #
##############
bank <- read_csv("GLM/Assignment/bank.csv")
bank <- as_tibble(lapply(bank, factor))
summary(bank)

mult<-vglm(interest ~ sex + q.income,family=multinomial(refLevel = 2), data=bank)
summary(mult)

##############
# Question 3 #
##############