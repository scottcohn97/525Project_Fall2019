# Analysis
# Scott Cohn + Ruja Kambli

# TODO create a variable dictionary .txt with what units everything is in etc.

library(tidyverse) # duh. 
library(ggplot2) # plotting
library(gridExtra) # plotting options
library(ggsci)  # plot color palette
library(bbplot) # plot style
library(readr) # import csv
library(magrittr)


# Import Data -------------------------------------------------------------
life_exp_full <- read_csv("data/life_exp_full.csv")

# Convert all to numeric
life_exp_full[] <- lapply(life_exp_full, function(x) as.numeric(as.character(x)))

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

model2 <- lm(`Life Expectancy` ~ `Birth Rate` + `GDP(US$mil)`, data = life_exp_full)
summary(model2)

model3 <- lm(`Life Expectancy` ~ `Birth Rate` + `Health Expenditure`, data = life_exp_full)
summary(model3)

model4 <- lm(`Life Expectancy` ~ `Birth Rate` + `GDP(US$mil)` + `Health Expenditure`, data = life_exp_full)
summary(model4)
# Note: GDP doesn't seem to matter


# Diagnostic Checks -------------------------------------------------------

# TODO Check for multicollinearity
# TODO Check for autocorrelation
# TODO Check for linearity
# TODO ...
