# Analysis
# Scott Cohn + Ruja Kambli

# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyverse) # duh.
library(ggplot2) # plotting
library(gridExtra) # plotting options
library(ggsci)  # plot color palette
library(ggthemes) # Themes
library(bbplot) # plot style
library(readr) # import csv
library(lmtest) # BP test
library(scales) # Scale x-axis
library(MASS)
library(faraway) # Box-Cox transform / vif

# Colors
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

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
topten_lifeexp_country <- life_exp_full %>%
  arrange(desc(`Life Expectancy`)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(Country, `Life Expectancy`),
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

# Save graph
finalise_plot(plot_name = topten_lifeexp_country,
              source = "Source: JNYH/Project Luther",
              save_filepath = "figures/topten_lifeexp_country.pdf",
              width_pixels = 640,
              height_pixels = 450)
              #logo_image_path = "placeholder.png")

# Bottom 10 life exp by country
# life_exp_full %>% drop_na(`Life Expectancy`) %>% nrow() = 201 rows w/out NA
bottomten_lifeexp_country <- life_exp_full %>%
  drop_na(`Life Expectancy`) %>%
  arrange(desc(`Life Expectancy`)) %>%
  slice(192:201) %>%
  ggplot(aes(x = reorder(Country, -`Life Expectancy`),
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

# Save graph
finalise_plot(plot_name = bottomten_lifeexp_country,
              source = "Source: JNYH/Project Luther",
              save_filepath = "figures/bottomten_lifeexp_country.pdf",
              width_pixels = 640,
              height_pixels = 450)
#logo_image_path = "placeholder.png")

# Distribution of Life Expectancy, Histogram
lifeexp_distro <- life_exp_full %>%
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

# Save graph
finalise_plot(plot_name = lifeexp_distro,
              source = "Source: JNYH/Project Luther",
              save_filepath = "figures/lifeexp_distro.pdf",
              width_pixels = 640,
              height_pixels = 450)
#logo_image_path = "placeholder.png")

# Life exp vs Birth Rate
life_exp_full %>%
  ggplot(aes(x = `Birth Rate`,
             y = `Life Expectancy`)) +
             xlab("Birth Rate per 1000 People") +       #RK I tried labelling these axes multiple times, not sure why it isn't showing up? 
             ylab("Life Expectancy") + 
  geom_point(color = "#1380A1") +
  geom_hline(yintercept = 0,
             size = 1,
             color = "#333333") +
  #scale_color_d3() +
  bbc_style() +
  labs(title = "How long can we expect to live?",
       subtitle = "Birth Rate vs. Life Expectancy",
       ylab = "Life Expectancy",
       xlab = "Births Per 1000 People") 

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

GDP_distro <- life_exp_full %>%
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

# Save graph
finalise_plot(plot_name = GDP_distro,
              source = "Source: JNYH/Project Luther",
              save_filepath = "figures/GDP_distro.pdf",
              width_pixels = 640,
              height_pixels = 450)
#logo_image_path = "placeholder.png")

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


# Plot Variables v Life Expectancy ----------------------------------------

# Birth
life_exp_full %>% 
  ggplot() +
  geom_point(
    aes(y = `Life Expectancy`, x = `Birth Rate`), 
    color = COLA[3]) +
  theme_clean()

hist(life_exp_full$`Birth Rate`,
     xlab   = "Birth Rate",
     main   = "Histogram of Birth Rate",
     col    = "dodgerblue",
     border = "black",
     breaks = 20)

# Cancer
life_exp_full %>% 
  ggplot() +
  geom_point(
    aes(y = `Life Expectancy`, x = `Cancer Rate`), 
    color = COLA[3]) +
  theme_clean()

hist(life_exp_full$`Cancer Rate`,
     xlab   = "Cancer Disease",
     main   = "Histogram of Cancer Disease",
     col    = "dodgerblue",
     border = "black",
     breaks = 20)

# Heart Disease
life_exp_full %>% 
  ggplot() +
  geom_point(
    aes(y = `Life Expectancy`, x = `Heart Disease Rate`), 
    color = COLA[3]) +
  theme_clean()

hist(life_exp_full$`Heart Disease Rate`,
     xlab   = "Heart Disease",
     main   = "Histogram of Heart Disease",
     col    = "dodgerblue",
     border = "black",
     breaks = 20)

# Stroke
life_exp_full %>% 
  ggplot() +
  geom_point(
    aes(y = `Life Expectancy`, x = `Stroke Rate`), 
    color = COLA[3]) +
  theme_clean()

# Health Expenditure
life_exp_full %>% 
  ggplot() +
  geom_point(
    aes(y = `Life Expectancy`, x = `Health Expenditure`), 
    color = COLA[3]) +
  theme_clean()

# EPI
life_exp_full %>% 
  ggplot() +
  geom_point(
    aes(y = `Life Expectancy`, x = EPI), 
    color = COLA[3]) +
  theme_clean()

# GDP
life_exp_full %>% 
  ggplot() +
  geom_point(
    aes(y = `Life Expectancy`, x = GDP), 
    color = COLA[3]) +
  theme_clean()

# Regressions --------------------------------------------------------------

model_full <- lm(
  `Life Expectancy` ~ `Birth Rate` + `Cancer Rate` + `Heart Disease Rate` + `Stroke Rate` +  `Health Expenditure` + EPI + GDP,
  data = life_exp_full
)

# Testing Model Fit -------------------------------------------------------

# compare model full and model reduced.
anova(model_red, model_full)
# F = 0.8399
# Pr(>F) = 0.5018
# Failed to reject H0: that removed var are zero.

# Diagnostic Checks - Model Full -------------------------------------------

# Model Summary and ANOVA
summary(model_full)
anova(model_full)

# 1 The regression function is linear (the relationship is linear).
# Yes 
vif(model_full)

# 2 The error terms have a constant variance

# Fitted vs Residuals --- model_full
plot(fitted(model_full), resid(model_full), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Full Model")
abline(h = 0, col = "darkorange", lwd = 2)
# Looks like it has a inverse parabolic shape


# 3 The error terms are independent (there is no relationship among the error terms).

# Breusch-Pagan Test for Homoskedasticity
bptest(model_full)
# For model_full we see a small p-value, so we reject the null hypothesis of 
#     homoskedasticity is rejected and heteroskedasticity assumed.
# The constant variance assumption is violated. 
# This matches our findings with a fitted versus residuals plot.

# 4 The error terms are normally distributed

hist(resid(model_full),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, Full Model",
     col    = "dodgerblue",
     border = "black",
     breaks = 20)
# It does have a rough bell shape, however, it also has a semi-sharp peak.

# Q-Q Plot
qqnorm(resid(model_full), main = "Normal Q-Q Plot, Full Model", col = "darkgrey")
qqline(resid(model_full), col = "dodgerblue", lwd = 2)
# Deviates in smaller quantiles
# For Model Full, we have a suspect Q-Q plot. 
# We would probably not believe the errors follow a normal distribution.

# Shapiro-Wilk Test
shapiro.test(resid(model_full))
# p = 7.152e-05
# A small p-value indicates we believe there is only a small probability 
# the data could have been sampled from a normal distribution.

#RK 5 Outlier Check Via Boxplots- how should we deal with those outliers? Which countries are they? 
outlierAssumption <- ggplot(model_full, aes(x=fitted(model_full), y=resid(model_full))) + 
  geom_boxplot() + 
  coord_flip()
outlierAssumption

# 6 There is no important predictor that have been omitted from the model
# RK I think for this one since she's just looking for a logical explanation, we can say
# that there very well maybe be other factors that are contributing to the life expectancy 
# of a country, but it is impossible to state them all. I'm going to put a better explanation
# and possible alternative predictors in the actual paper. 


# Diagnostic Checks - Model Reduced -------------------------------------------------------

# Model Summary and ANOVA
summary(model_red)
anova(model_red)

# Fitted vs Residuals --- model_red
plot(fitted(model_red), resid(model_red), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model Reduced")
abline(h = 0, col = "darkorange", lwd = 2)
# Looks like it has a inverse parabolic shape

# Breusch-Pagan Test for Homoskedasticity
bptest(model_red)
# For model_red we see a small p-value, so we reject the null of homoskedasticity. 
# The constant variance assumption is violated. 
# This matches our findings with a fitted versus residuals plot.

# Normality of errors
hist(resid(model_red),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, Model Reduced",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
# It does have a rough bell shape, however, it also has a very sharp peak.

# Q-Q Plot
qqnorm(resid(model_red), main = "Normal Q-Q Plot, Model Reduced", col = "darkgrey")
qqline(resid(model_red), col = "dodgerblue", lwd = 2)
# Deviates in smaller quantiles
# For Model Reduced, we have a suspect Q-Q plot. 
# We would probably not believe the errors follow a normal distribution.


# Shapiro-Wilk Test
shapiro.test(resid(model_red))
# p = 7.152e-05
# A small p-value indicates we believe there is only a small probability 
# the data could have been sampled from a normal distribution.

# End of File -------------------------------------------------------------







