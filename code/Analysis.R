# Analysis
# Scott Cohn + Ruja Kambli


library(tidyverse) # duh.
library(ggplot2) # plotting
library(gridExtra) # plotting options
library(ggsci)  # plot color palette
library(bbplot) # plot style
library(readr) # import csv
library(lmtest) # BP test
library(scales)
library(MASS)
library(faraway) # Box-Cox transform / vif

# Import Data -------------------------------------------------------------
life_exp_full <- read_csv("data/life_exp_full.csv")


# Data Transformations ----------------------------------------------------

# Capitalize letters in Country var
# Not perfect, but good enough
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

life_exp_full <- life_exp_full %>%  
  mutate(Country = apply(life_exp_full, 1, simpleCap))

# Visualizations ----------------------------------------------------------

# Top 10 life exp by country
life_exp_full %>%
  arrange(desc(`Life Expectancy`)) %>%
  slice(1:10) %>%
  ggplot(aes(x = Country,
             y = `Life Expectancy`)) +
  geom_bar(stat = 'identity',
           fill = "#1380A1") +
  #scale_fill_d3() +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 85),
    breaks = seq(0, 80, by = 20),
    labels = c("0", "20", "40", "60", "80 years")
  ) +
  geom_hline(yintercept = 0,
             size = 1,
             color = "#333333") +
  geom_label(
    aes(label = round(`Life Expectancy`, 0)),
    hjust = 1,
    vjust = 0.5,
    colour = "white",
    fill = NA,
    label.size = NA,
    family = "Helvetica",
    size = 6
  ) +
  bbc_style() +
  labs(title = "Life Expectancy",
       subtitle = "Top 10 Countries")

# Bottom 10 life exp by country
# life_exp_full %>% drop_na(`Life Expectancy`) %>% nrow() = 201 rows w/out NA
life_exp_full %>%
  drop_na(`Life Expectancy`) %>%
  arrange(desc(`Life Expectancy`)) %>%
  slice(192:201) %>%
  ggplot(aes(x = Country,
             y = `Life Expectancy`)) +
  geom_bar(stat = 'identity',
           fill = "#1380A1") +
  #scale_fill_d3() +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 85),
    breaks = seq(0, 80, by = 20),
    labels = c("0", "20", "40", "60", "80 years")
  ) +
  geom_hline(yintercept = 0,
             size = 1,
             color = "#333333") +
  geom_label(
    aes(label = round(`Life Expectancy`, 0)),
    hjust = 1,
    vjust = 0.5,
    colour = "white",
    fill = NA,
    label.size = NA,
    family = "Helvetica",
    size = 6
  ) +
  bbc_style() +
  labs(title = "Life Expectancy",
       subtitle = "Bottom 10 Countries")

# Distribution of Life Expectancy, Histogram
life_exp_full %>%
  ggplot(aes(x = `Life Expectancy`)) +
  geom_histogram(binwidth = 5,
                 color = "white",
                 fill = "#1380A1") +
  geom_hline(yintercept = 0,
             size = 1,
             color = "#333333") +
  bbc_style() +
  scale_x_continuous(
    limits = c(40, 95),
    breaks = seq(40, 90, by = 10),
    labels = c("40", "50", "60", "70", "80", "90 years")
  ) +
  labs(title = "How life expectancy varies",
       subtitle = "Distribution of life expectancy")

# Life exp vs Birth Rate
life_exp_full %>%
  ggplot(aes(x = `Birth Rate`,
             y = `Life Expectancy`)) +
  geom_point(color = "#1380A1") +
  geom_hline(yintercept = 0,
             size = 1,
             color = "#333333") +
  #scale_color_d3() +
  bbc_style() +
  labs(title = "How long do we expect to live?",
       subtitle = "Birth Rate vs. Life Expectancy") 

# Life vs Cancer
life_exp_full %>%
  ggplot(aes(x = `Cancer Rate`,
             y = `Life Expectancy`)) +
  geom_point(color = "#1380A1") +
  geom_hline(yintercept = 0,
             size = 1,
             colour = "#333333") +
  #scale_color_d3() +
  bbc_style() +
  labs(title = "How long do we expect to live?",
       subtitle = "Cancer Rate vs. Life Expectancy") 

# GDP Distribution, Histogram
hist(life_exp_full$GDP, col = "#1380A1")

life_exp_full %>%
  ggplot(aes(x = GDP)) +
  geom_histogram(
    color = "white",
    fill = "#1380A1",
    na.rm = TRUE,
    bins = 40
  ) +
  geom_hline(yintercept = 0,
             size = 1,
             color = "#333333") +
  bbc_style() +
  scale_x_continuous(labels = scales::comma) + 
  labs(title = "How GDP varies",
       subtitle = "Distribution of GDP (US $ Mil.)")

# Life vs Heart Disease
life_exp_full %>%
  ggplot(aes(x = `Heart Disease Rate`,
             y = `Life Expectancy`)) +
  geom_point(color = "#1380A1") +
  geom_hline(yintercept = 0,
             size = 1,
             colour = "#333333") +
  #scale_color_d3() +
  bbc_style() +
  labs(title = "How long do we expect to live?",
       subtitle = "Heart Disease Rate vs. Life Expectancy")

# Life vs EPI
life_exp_full %>%
  ggplot(aes(x = EPI,
             y = `Life Expectancy`)) +
  geom_point(color = "#1380A1") +
  geom_hline(yintercept = 0,
             size = 1,
             colour = "#333333") +
  #scale_color_d3() +
  bbc_style() +
  labs(title = "How long do we expect to live?",
       subtitle = "EPI vs. Life Expectancy")
# Regressions --------------------------------------------------------------

model_full <- lm(
  `Life Expectancy` ~ `Birth Rate` + `Cancer Rate` + `Heart Disease Rate` + `Stroke Rate` + `Health Expenditure` + EPI + GDP,
  data = life_exp_full
)
summary(model_full)
anova(model_full)

model_red <-
  lm(`Life Expectancy` ~ `Birth Rate` + `Stroke Rate` + EPI, data = life_exp_full)
summary(model_red)
# Has violations of assumptions (see below)

model_red_log <-
  lm(log(`Life Expectancy`) ~ `Birth Rate` + `Stroke Rate` + EPI, data = life_exp_full)
summary(model_red_log)
# Model 11 attempts to transform to log-scale -> See diagnostics; doesn't work.

# Model BC --- Box-Cox Transform
model_bc <-
  lm(`Life Expectancy` ~ `Birth Rate` + `Stroke Rate` + EPI, data = life_exp_full)
boxcox(model_bc, plotit = TRUE, lambda = seq(3.5, 5, by = 0.1))
# Here we see that lambda = 4 is both in the confidence interval, and is extremely close to the maximum. 
# This suggests a transformation of $\frac{y^\lamba - 1}{\lambda} = \frac{y^4 - 1}{4}$

transform_y <- ((life_exp_full$`Life Expectancy`)^4 - 1)/4
model_bc_transform <- lm(transform_y ~ `Birth Rate` + `Stroke Rate` + EPI, data = life_exp_full)
# Solves heterosked problem. See Diagnostics.
# Challenge: Interpreting Estimates


# Testing Model Fit -------------------------------------------------------

# TODO Find the model that fits best.





# Diagnostic Checks - Model Reduced -------------------------------------------------------

# TODO Check for multicollinearity
# TODO Check for autocorrelation
# TODO Check for linearity


# TODO Check for constant variance

# Fitted vs Residuals --- model_red
plot(fitted(model_red), resid(model_red), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 10")
abline(h = 0, col = "darkorange", lwd = 2)
# Looks like it has a inverse parabolic shape

# Breusch-Pagan Test for Homoskedasticity
bptest(model_red)
# For model_red we see a small p-value, so we reject the null of homoscedasticity. 
# The constant variance assumption is violated. 
# This matches our findings with a fitted versus residuals plot.

# Normality of errors
hist(resid(model_red),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, Model 10",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
# It does have a rough bell shape, however, it also has a very sharp peak.

# Q-Q Plot
qqnorm(resid(model_red), main = "Normal Q-Q Plot, Model 10", col = "darkgrey")
qqline(resid(model_red), col = "dodgerblue", lwd = 2)
# Deviates in smaller quantiles
# For Model 10, we have a suspect Q-Q plot. 
# We would probably not believe the errors follow a normal distribution.


# Shapiro-Wilk Test
shapiro.test(resid(model_red))
# p = 7.152e-05
# A small p-value indicates we believe there is only a small probability 
# the data could have been sampled from a normal distribution.


# Diagnostic Checks - Model Full --------------------------------------------

# Model Reduced Log attempts to transform to log-scale
# Fitted vs Residuals --- model_red_log
plot(fitted(model_red_log),
     resid(model_red_log),
     col = "grey",
     pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 11")
abline(h = 0, col = "darkorange", lwd = 2)
# Looks like it has a inverse parabolic shape

# Breusch-Pagan Test for Homoskedasticity
bptest(model_red_log)
# For model_red_log we see a small p-value, so we reject the null of homoscedasticity. 
# The constant variance assumption is violated. 
# This matches our findings with a fitted versus residuals plot.

# Normality of errors
hist(
  resid(model_red_log),
  xlab   = "Residuals",
  main   = "Histogram of Residuals, Model 10",
  col    = "darkorange",
  border = "dodgerblue",
  breaks = 20
)
# It does have a rough bell shape, however, it also has a very sharp peak.

# Q-Q Plot
qqnorm(resid(model_red_log), main = "Normal Q-Q Plot, Model 11", col = "darkgrey")
qqline(resid(model_red_log), col = "dodgerblue", lwd = 2)
# Deviates in smaller quantiles
# For Model 11, we have a suspect Q-Q plot. 
# We would probably not believe the errors follow a normal distribution.

# Shapiro-Wilk Test
shapiro.test(resid(model_red_log))
# p = 7.822e-07
# A small p-value indicates we believe there is only a small probability 
# the data could have been sampled from a normal distribution.




# Diagnostic Checks - Model Box Cox Transform -----------------------------

# Fitted vs Residuals
plot(
  fitted(model_bc_transform),
  resid(model_bc_transform),
  col = "grey",
  pch = 20,
  xlab = "Fitted",
  ylab = "Residuals",
  main = "Data from Model 10"
)
abline(h = 0, col = "darkorange", lwd = 2)
# Looks random so all set

# Breusch-Pagan
bptest(model_bc_transform)
# pass

# Shapiro-Wilks
shapiro.test(resid(model_bc_transform))
# pass

# Box-cox
boxcox(model_bc_transform)
# pass

# Variation Inflation Factor
vif(model_bc_transform)
# All <5 so no multicollinearity problems

# Normality of errors
hist(
  resid(model_bc_transform),
  xlab   = "Residuals",
  main   = "Histogram of Residuals, Model 10",
  col    = "darkorange",
  border = "dodgerblue",
  breaks = 20
)
# It does have a rough bell shape. Looks Good.

# Q-Q Plot
qqnorm(resid(model_bc_transform), main = "Normal Q-Q Plot, Model 13", col = "darkgrey")
qqline(resid(model_bc_transform), col = "dodgerblue", lwd = 2)
# Deviates in slightly smaller quantiles
# For Model 13, we have an okay Q-Q plot. 
# We would probably believe the errors follow a mostly normal distribution.

# Linearity 
plot(
  transform_y ~ `Birth Rate`,
  data = life_exp_full,
  col = "dodgerblue",
  pch = 20,
  cex = 1.5
)
# Linear (-)

plot(
  transform_y ~ `Stroke Rate`,
  data = life_exp_full,
  col = "dodgerblue",
  pch = 20,
  cex = 1.5
)
# Linear (-)

plot(
  transform_y ~ EPI,
  data = life_exp_full,
  col = "dodgerblue",
  pch = 20,
  cex = 1.5
)
# Linear (+)