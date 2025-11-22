# R Lecture 10
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\Data_Analysis_With_R")  # Set working directory

# Logistic Regression 
# When Response variable is not continuous (discrete, categorical)

# Load in the data
data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(data)

# response variable: admit (0 / 1)
# predictor variables: gre, gpa and rank 

unique(data$rank)            # 1 2 3 4

# Contingency Table
#  for each rank (1–4), the counts of admit = 0 and admit = 1.

xtabs(~admit + rank, data = data)

# If for any rank, count = 0 for admit == 0 or 1 then 
# we cannot use that rank in logistic regression.
# each predictor category needs variation in the outcome (both 0 and 1) to estimate an effect.

# Note: need to do this when predictor is a category

#
levels(data$rank)                 # Not a factor
data$rank <- as.factor(data$rank) # change to a factor

levels(data$rank) 

# Model the data
?glm

glm.model <- glm(formula = admit ~ gre + gpa + rank,
    data = data, 
    family = "binomial")   # response variable is binary (0/1)

summary(glm.model)
# gra and gpe are significant predictors
# 
# rank2       -0.675443   0.316490  -2.134 0.032829 *  
# rank3       -1.340204   0.345306  -3.881 0.000104 ***
# rank4       -1.551464   0.417832  -3.713 0.000205 ***
#
# Why no rank1?
# rank1 is the reference category (since its the first level of rank)
# because for ranks 2 3 and 4 the parameters are given relative to that of rank1 (since its the first level)
#
# rank2       -0.675443
# means the log‑odds of admission for rank2 are 0.675 lower than rank1, holding other variables constant.
# and that for rank 3 and 4 are even lower.
# 
# But how significant is the rank as a whole to determine admit?
#
install.packages("aod")
library(aod)

coef(glm.model) # or
glm.model$coefficients

glm.model$coefficients[c("rank2", "rank3", "rank4")]
# or
glm.model$coefficients[4:6]

# Combine significance of  3 ranks into 1
wald.test(b = glm.model$coefficients, 
          Sigma = vcov(glm.model),   # variance–covariance matrix of the estimated coefficients of glm.model
          Terms = 4:6)

# After this we get a single p.value (0.00011) very significant
# Chi-squared test:
# X2 = 20.9, df = 3, P(> X2) = 0.00011

# 
# In linear regression, we report the coefficients directly.
# Each coefficient shows how much the outcome changes when that predictor increases by one unit (holding others constant).
# if coefficient = 2, then Y goes up by 2 for every +1 in X.


# In logistic regression, we report odds ratio.
# example: the chance of someone being admitted.

# so instead of reporting
glm.model$coefficients

# we report
exp(glm.model$coefficients)

# gre 1.0022670
# means: for every +1 increase in GRE,
# the odds of admission are multiplied by 1.0022670

# Percent change in odds = (Odds ratio − 1 ) × 100%
# (1.0022670 − 1) × 100 = 0.2267%
# So each +1 GRE point increases the odds of admission only by about 0.23%.

# gpa   2.2345448
# Higher gpa increases odds by approximately 123.45%.

# Note
# Percent change measures how much larger something becomes relative to the original.
# If something doubles, 
# Percent change = (2 - 1) * 100 = 100%
# If it goes from 1 to 3, (3 - 1) * 100 = 200%

# rank2   0.5089310
# rank 2 decreases the chance of getting admitted by 50% (percent change in odds = −49.1069%)
# rank3        0.2617923
# and rank 3 decreases the chance by 73.82077%

# To get a 95% Confidence Interval
exp(cbind(odds.ratio = glm.model$coefficients, confint(glm.model)))


# glm(...., family = "binomial"): if response variable is  binomial but 
# it can be:
# Gaussian
# gamma
# inverse.Gaussian
# Poisson

# glm(...., family = "gaussian") is exactly same as using lm()
#
# which distribution? depends on data
# if we have count data then Poisson 


# Long vs Wide Format
# ==========================================
# Wide format: each variable has its own column.
# most commonly used

# Long format: one column for the variable name, another for its values.

data("airquality")
head(airquality)      # Wide format, each variable has its own column.

library(ggplot2)
#  ggplot works best with long (tidy) format data.

# For a basic scatter plot, wide format will do
ggplot(airquality, aes(x = Temp, y = Ozone)) +
  geom_point() +
  labs(x = "Temperature", y = "Ozone") +
  theme_minimal()

# But to plot more than 2 variables, ggplot need long format

library(tidyr)

airquality.long <- airquality %>%          # pipe operator
  pivot_longer(
    cols = c(Ozone, Solar.R, Wind, Temp),  # columns to reshape
    names_to = "Variable",                 # new column name
    values_to = "Value"                    # new column for values
  )
head(airquality.long)

library(ggplot2)

ggplot(airquality.long, aes(x = Day, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap(~ Month) +
  labs(x = "Day", y = "Value", title = "Airquality variables by day (May-Sep)") +
  theme_minimal()

# Back to wide format

airquality.wide <- airquality.long %>% pivot_wider(
  names_from = Variable,
  values_from = Value
)
head(airquality.wide)
head(airquality)
