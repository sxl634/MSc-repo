library(haven)
library(tidyverse)

life_expectancy_dataset <- read_sav("Regression Modelling/Regression Modelling Data/life.sav")

plot(life_expectancy_dataset$LN_GNP, life_expectancy_dataset$LEXPECM,
     main = "Scatterplot male life expectancy in acountry versus the log of GNP",
     xlab="Log Gross National Product", ylab = ("Life Expectancy (MAles)"))
abline(lm(life_expectancy_dataset$LEXPECM ~ life_expectancy_dataset$LN_GNP), col = "red")

cor(life_expectancy_dataset$LN_GNP, life_expectancy_dataset$LEXPECM, method = "pearson", use = "complete.obs")

y <- life_expectancy_dataset$LEXPECM
x <- life_expectancy_dataset$LN_GNP

b <- lm(y ~ x)
model <- lm(life_expectancy_dataset$LEXPECM ~ life_expectancy_dataset$LN_GNP)
a <- summary(lm(life_expectancy_dataset$LEXPECM ~ life_expectancy_dataset$LN_GNP))

a$coefficients[1]+a$coefficients[2]*6.5
a$coefficients[1]+a$coefficients[2]*8.5

confint(lm(life_expectancy_dataset$LEXPECM ~ life_expectancy_dataset$LN_GNP), level = 0.95)
confint(lm(life_expectancy_dataset$LEXPECM ~ life_expectancy_dataset$LN_GNP), "life_expectancy_dataset$LN_GNP", level = 0.95)
confint(lm(life_expectancy_dataset$LEXPECM ~ life_expectancy_dataset$LN_GNP), level = 0.99)

life_expectancy_orig <- life_expectancy_dataset

dummy_1 <- list(NA,NA,NA,NA,10)
dummy_2 <- list(NA,NA,NA,NA,6)

life_expectancy_dataset <- rbind(life_expectancy_dataset, dummy_1, dummy_2)

dummy <- c(10,6)

#predict seems very fussy about names being the same, bit weird if you ask me but whatevs
predict(b, data.frame(x = dummy), interval = "confidence") 
d <- as.data.frame(predict(model, life_expectancy_dataset, interval = "confidence"))
