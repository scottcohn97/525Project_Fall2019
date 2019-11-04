# Analysis
# Scott Cohn + Ruja Kambli

library(tidyverse) # duh. 
library(ggplot2) # plotting
library(gridExtra) # plotting options
library(ggsci)  # plot color palette
library(bbplot) # plot style
library(readr) # import csv
library(lmtest) # BP test

library(MASS)
library(faraway) # Box-Cox transform / vif


# Import Data -------------------------------------------------------------
life_exp_full <- read_csv("data/life_exp_full.csv")

# Visualizations ----------------------------------------------------------

# Life exp vs Birth Rate
life_exp_full %>% 
  ggplot(aes(x = `Birth Rate`,
             y = `Life Expectancy`
             )) + 
  geom_point(color = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  #scale_color_d3() + 
  bbc_style() +
  labs(
    title = "How long do we expect to live?",
    subtitle = "Birth Rate vs. Life Expectancy"
  ) 

# Life vs Cancer
life_exp_full %>% 
  ggplot(aes(x = `Cancer Rate`,
             y = `Life Expectancy`
  )) + 
  geom_point(color = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  #scale_color_d3() + 
  bbc_style() +
  labs(
    title = "How long do we expect to live?",
    subtitle = "Cancer Rate vs. Life Expectancy"
  ) 

# Life vs GDP 
# TODO Turn this into a histogram binned by GDP
life_exp_full %>% 
  ggplot(aes(x = `GDP(US$mil)`, # figure out units
             y = `Life Expectancy`
  )) + 
  geom_point(color = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  #scale_color_d3() + 
  bbc_style() +
  labs(
    title = "How long do we expect to live?",
    subtitle = "GDP (US $ Mil.) vs. Life Expectancy"
  ) 

# Life vs Heart Disease
life_exp_full %>% 
  ggplot(aes(x = `Heart Disease Rate`,
             y = `Life Expectancy`
  )) + 
  geom_point(color = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  #scale_color_d3() + 
  bbc_style() +
  labs(
    title = "How long do we expect to live?",
    subtitle = "Heart Disease Rate vs. Life Expectancy"
  ) 

# Regressions --------------------------------------------------------------

model1 <- lm(`Life Expectancy` ~ `Birth Rate`, data = life_exp_full)
summary(model1)

model2 <- lm(`Life Expectancy` ~ `Birth Rate` + GDP, data = life_exp_full)
summary(model2)

model3 <- lm(`Life Expectancy` ~ `Birth Rate` + `Health Expenditure`, data = life_exp_full)
summary(model3)

model4 <- lm(`Life Expectancy` ~ `Birth Rate` + GDP + `Health Expenditure`, data = life_exp_full)
summary(model4)
# Note: GDP doesn't seem to matter

model5 <- lm(`Life Expectancy` ~ `Birth Rate` + `Cancer Rate` + `Heart Disease Rate` + `Stroke Rate`, data = life_exp_full)
summary(model5)

model6 <- lm(`Life Expectancy` ~ `Birth Rate` + `Cancer Rate` + `Heart Disease Rate` + `Stroke Rate` + `Health Expenditure`, data = life_exp_full)
summary(model6)

model7 <- lm(`Life Expectancy` ~ `Birth Rate` + EPI, data = life_exp_full)
summary(model7)

model8 <- lm(`Life Expectancy` ~ `Birth Rate` + `Cancer Rate` + `Heart Disease Rate` + `Stroke Rate` + `Health Expenditure` + EPI, data = life_exp_full)
summary(model8)
# Rates of Cancer and Strokes don't seem to matter. Neither do health expenditures.
# Population and Pop Density don't seem to matter
# EPI and stroke rate seem to matter

model9 <- lm(`Life Expectancy` ~ `Birth Rate` +  `Stroke Rate` + EPI, data = life_exp_full)
summary(model9)

model10 <- lm(`Life Expectancy` ~ `Birth Rate` + `Stroke Rate` + EPI, data = life_exp_full)
summary(model10)
# Has violations of assumptions (see below)

model11 <- lm(log(`Life Expectancy`) ~ `Birth Rate` + `Stroke Rate` + EPI, data = life_exp_full)
summary(model11)
# Model 11 attempts to transform to log-scale -> See diagnostics; doesn't work.

# Model 12 --- Box-Cox Transform
model12 <- lm(`Life Expectancy` ~ `Birth Rate` + `Stroke Rate` + EPI, data = life_exp_full)
boxcox(model12, plotit = TRUE, lambda = seq(3.5, 5, by = 0.1))
# Here we see that lambda = 4 is both in the confidence interval, and is extremely close to the maximum. 
# This suggests a transformation of $\frac{y^\lamba - 1}{\lambda} = \frac{y^4 - 1}{4}$

transform_y <- ((life_exp_full$`Life Expectancy`)^3.75 - 1)/3.75
model13 <- lm(transform_y ~ `Birth Rate` + `Stroke Rate` + EPI, data = life_exp_full)

# Diagnostic Checks - Model 10 -------------------------------------------------------

# TODO Check for multicollinearity
# TODO Check for autocorrelation
# TODO Check for linearity


# TODO Check for constant variance

# Fitted vs Residuals --- model10
plot(fitted(model10), resid(model10), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 10")
abline(h = 0, col = "darkorange", lwd = 2)
# Looks like it has a inverse parabolic shape

# Breusch-Pagan Test for Homoskedasticity
bptest(model10)
# For model10 we see a small p-value, so we reject the null of homoscedasticity. 
# The constant variance assumption is violated. 
# This matches our findings with a fitted versus residuals plot.

# Normality of errors
hist(resid(model10),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, Model 10",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
# It does have a rough bell shape, however, it also has a very sharp peak.

# Q-Q Plot
qqnorm(resid(model10), main = "Normal Q-Q Plot, Model 10", col = "darkgrey")
qqline(resid(model10), col = "dodgerblue", lwd = 2)
# Deviates in smaller quantiles
# For Model 10, we have a suspect Q-Q plot. 
# We would probably not believe the errors follow a normal distribution.


# Shapiro-Wilk Test
shapiro.test(resid(model10))
# p = 7.152e-05
# A small p-value indicates we believe there is only a small probability 
# the data could have been sampled from a normal distribution.


# Diagnostic Checks - Model 11 --------------------------------------------

# Model 11 attempts to transform to log-scale
# Fitted vs Residuals --- model11
plot(fitted(model11), resid(model11), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 11")
abline(h = 0, col = "darkorange", lwd = 2)
# Looks like it has a inverse parabolic shape

# Breusch-Pagan Test for Homoskedasticity
bptest(model11)
# For model11 we see a small p-value, so we reject the null of homoscedasticity. 
# The constant variance assumption is violated. 
# This matches our findings with a fitted versus residuals plot.

# Normality of errors
hist(resid(model11),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, Model 10",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
# It does have a rough bell shape, however, it also has a very sharp peak.

# Q-Q Plot
qqnorm(resid(model11), main = "Normal Q-Q Plot, Model 11", col = "darkgrey")
qqline(resid(model11), col = "dodgerblue", lwd = 2)
# Deviates in smaller quantiles
# For Model 11, we have a suspect Q-Q plot. 
# We would probably not believe the errors follow a normal distribution.

# Shapiro-Wilk Test
shapiro.test(resid(model11))
# p = 7.822e-07
# A small p-value indicates we believe there is only a small probability 
# the data could have been sampled from a normal distribution.




# Diagnostic Checks - Model 13 --------------------------------------------

# Fitted vs Residuals
plot(fitted(model13), resid(model13), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 10")
abline(h = 0, col = "darkorange", lwd = 2)
# Looks random so all set

# Breusch-Pagan
bptest(model13)
# pass

# Shapiro-Wilks
shapiro.test(resid(model13))
# pass

# Box-cox
boxcox(model13)
# pass

# Variation Inflation Factor
vif(model13)
# All <5 so no multicollinearity problems

# Normality of errors
hist(resid(model13),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, Model 10",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
# It does have a rough bell shape. Looks Good.

# Q-Q Plot
qqnorm(resid(model13), main = "Normal Q-Q Plot, Model 13", col = "darkgrey")
qqline(resid(model13), col = "dodgerblue", lwd = 2)
# Deviates in slightly smaller quantiles
# For Model 13, we have an okay Q-Q plot. 
# We would probably believe the errors follow a mostly normal distribution.

# Linearity 
plot(transform_y ~ `Birth Rate`, data = life_exp_full, col = "dodgerblue", pch = 20, cex = 1.5)
# Linear (-)

plot(transform_y ~ `Stroke Rate`, data = life_exp_full, col = "dodgerblue", pch = 20, cex = 1.5)
# Linear (-)

plot(transform_y ~ EPI, data = life_exp_full, col = "dodgerblue", pch = 20, cex = 1.5)
# Linear (+)