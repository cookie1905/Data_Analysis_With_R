# R Lecture 9
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\Data_Analysis_With_R")  # Set working directory


# Linear Mixed Models (LMMs)
# ==========================================
#
# Why do we need them?
#
# General linear models (GLMs) assume all observations are independent.
# But in many cases this assumption is violated:
#   i) repeated measurements from the same individual
#   ii) data from related individuals (e.g. family studies)
#
# Why it matters:
# If repeated measurements are wrongly treated as independent in a GLM,
# results may look significant due to relatedness when they are not.
# This leads to spurious relationships (FPs) and overestimation of statistical power.
#

# Linear Models: Response ~ Predictor
#
# example:
#
freq.data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
head(freq.data)

unique(freq.data$subject)

# frequency(of voice) ~ attitude (polite vs informal) + gender + error term
# why include gender ?
# To account for the fact that females have much higher frequency of voice compared to males.
#

boxplot(frequency ~ subject, data = freq.data,
        main = "Frequency by Subject",
        xlab = "Subject",
        ylab = "Frequency",
        cex.main = 0.8)

# Note
library(ggplot2)

ggplot(freq.data, aes(x = subject, y = frequency, fill = gender)) +
  geom_boxplot() +
  labs(title = "Frequency by Subject",
       x = "Subject",
       y = "Frequency") +
  scale_fill_manual(values = c("F" = "orange", "M" = "lightblue")) +
  theme_minimal()

# As per the design of the experiment, they had multiple measurements per subject.
# (subjects measured in different settings where they need to be polite or informal.)
# per-subject variation

# A GLM won't know this and will consider each observation independent.
#
# Random Effects
# ==========================================
#
# All responses from the same person will share similarities,
# and will differ from responses of other individuals.
# This makes repeated measures interdependent rather than independent.
# From the above box plot we can see clearly that there is variation between M3 M4 and M7
# between F1 and F2 and between F1 and F3

# Extending the Model
#
# In GLM, intercept is the predicted value for the response variable,
# when all predictors are equal to 0. Meaning:
# it’s the shared baseline response or prediction for all individuals,
# before accounting for predictors.
# - fixed intercept
#
# In LMM
# frequency ~ attitude + gender + (1|subject) + error term
# The extra term (1|subject) is a random intercept. 
# It allows each subject to have their own individual intercept,
# random intercept

# As per the design they also asked each subject different questions
# depending on question attitude can be different
# As a result, frequency of voice can also change.
# per-item variation
# 

boxplot(frequency ~ scenario, data = freq.data,
        main = "Frequency by Scenario/Question",
        xlab = "Scenario",
        ylab = "Frequency",
        cex.main = 0.8)

#Note
ggplot(freq.data, aes(x = scenario, y = frequency)) +  # won't work as expected
  geom_boxplot() 

# why?
levels(freq.data$scenario) # is NULL
# ggplot needs us to explicitly tell when a numeric variable should be treated as categorical.
# Base r does this automatically

ggplot(freq.data, aes(x = as.factor(scenario), y = frequency)) + 
  geom_boxplot() + 
  labs(title = "Frequency by Scenario",
       x = "Scenario",
       y = "Frequency")+
  theme_minimal()


boxplot(frequency ~ attitude, data = freq.data,
        main = "Frequency by Attitude",
        xlab = "Attitude",
        ylab = "Frequency",
        cex.main = 0.8)

# Extending the Model again:
# frequency ~ attitude + gender + (1|subject) + (1|item) + error term
#
# each individual is allowed to have its own intercept.
# each question is also allowed to have its own intercept.
# So we take into account variation within subjects and within questions.

# LMM in R
# lmer() in package lme4
# similar to lm()

install.packages("lme4")
library(lme4)
?lmer

head(freq.data)

boxplot(frequency ~ attitude + gender,
        col = c("orange", "lightblue"),
        data = freq.data)

lmer(formula = frequency ~ attitude + gender,
     data = freq.data)
# Error: No random effects terms specified in formula
# lmr() expects a LMM model so it needs at least 1 random effect.

lmm.model <- lmer(formula = frequency ~ attitude + gender  + (1|subject) + (1|scenario),
     data = freq.data)

summary(lmm.model)

# What we are interested in :-
#
# Fixed effects:
# Estimate Std. Error t value
# (Intercept)  256.846     16.116  15.938
# attitudepol  -19.721      5.584  -3.532
# genderM     -108.516     21.013  -5.164

#1. attitudepol (-19.721)
# If you are talking politely your voice will go down by 19Hz compared to when you speak informally. 
# Or in other words
# Speaking politely is associated with a ~19.7 Hz lower frequency compared to informal speech, holding gender constant.

#2. genderM (-108.516)
# males have a ~108.5 Hz lower frequency compared to females, holding attitude constant.

# To view all 7 intercepts for each scenario and subject
#
ranef(lmm.model)

# Compare 2 Models
# Check Model Significance
# ==========================================
#
# First model doesnot include factor of interest (no attitude)
lmm.moel1 <- lmer(formula = frequency ~ gender  + (1|subject) + (1|scenario),
                  data = freq.data,
                  REML = FALSE)

# Second model includes factor of interest (attitude)
lmm.moel2 <- lmer(formula = frequency ~ attitude + gender  + (1|subject) + (1|scenario),
                  data = freq.data,
                  REML = FALSE)

anova(lmm.moel1, lmm.moel2)

# look at the AIC    
# lmm.moel1   816.72
# lmm.moel2   807.10

# There is a difference of 10 between the models
# lmm.model2 is better than lmm.model1

# p ~ 0.0006532 ***
# lmm.model2 fits significantly better than lmm.model1 

# Above models are Random Intercept models
# intercept is different for each subject and scenario, but
# fixed effects are same for all subjects and scenarios

# So the model accounted baselines differences in frequency but assume
# the effects of attitude and gender on frequency is same for all subjects and scenarios. 

# So in Random Intercept model, there are
# Random intercepts: each subject and scenario has its own baseline starting frequency.
# Fixed effects: the effect of attitude (≈ –19 Hz when polite) and gender (≈ –108 Hz for males compared to females) are constant across all subjects and scenarios.

# Regardless of the speaker or the scenario, 
# speaking politely leads to an average drop of about 19 Hz in voice frequency, 
# and men’s voices are on average 108 Hz lower than women’s.

# But this is a strong assumption
# because it forces the effect of attitude and gender to be identical across all subjects and scenarios
# Some people may be more polite than others
# or react differently to same scenario 

# A random slope model relaxes this by allowing the effect of a predictor (attitude or gender)
# to vary across subjects or scenarios, not just the intercept.
# So each subject/scenario can have its own baseline and its own strength of effect.
# So different intercepts and different slopes

#
# Random Slope Model
# ==========================================
# 
freq.model <- lmer(formula = frequency ~ attitude + gender  + 
                     (1 + attitude | subject) + (1 + attitude | scenario),
                   data = freq.data,
                   REML = FALSE)
# 
# here we assume gender effect is constant 
# but different slope for the effect of attitude across subjects and scenarios

summary(freq.model)

# Random effects:
# Groups   Name        Variance Std.Dev. Corr
# scenario (Intercept) 182.083  13.494       
# attitudepol  31.244   5.590   0.22
# subject  (Intercept) 392.344  19.808       
# attitudepol   1.714   1.309   1.00

# For scenarios, variance of attitude slope ≈ 31.2, ie
# effect of attitude varies across scenarios.
# For subjects, variance of attitude slope is only ≈ 1.7 
# suggesting less variation across subjects.

ranef(freq.model)        # see we now have different slopes as well

# Note
# The fixed effect for attitude is the average effect across all groups.
# Random slopes show whether that effect differs across subjects/scenarios


# Attitude (politeness) lowers frequency overall,#
# Men’s voices are lower,
# Size of the politeness effect (the drop in frequency) is not constant across scenarios.
# Some scenarios show a stronger drop, others a weaker one.
# Effect of attitude doesn't vary much across subjects. 
