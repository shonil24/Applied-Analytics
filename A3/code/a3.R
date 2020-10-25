library(readr)
library(dplyr)
library(magrittr)
library(epitools)
library(car)

bdims <- read_csv("F:/MS/Sem 1/AA/Assignment 3/bdims.csv")
View(bdims)

# Sapply is used to find and sum all the null values
sapply(bdims, function(x) sum(is.na(x)))

bdims$che.di <- as.numeric(bdims$che.di)
bdims$hgt <- as.numeric(bdims$hgt)

# Descriptive statistics
bdims %>% summarise(
  Min = min(che.di, na.rm = TRUE),
  Q1 = quantile(che.di, probs = .25, na.rm = TRUE),
  Median = median(che.di, na.rm = TRUE),
  Q3 = quantile(che.di, probs = .75, na.rm = TRUE),
  Max = max(che.di, na.rm = TRUE),
  Mean = mean(che.di, na.rm = TRUE),
  SD = sd(che.di, na.rm = TRUE),
  n = n(),
  Missing = sum(is.na(che.di))
)

bdims %>% summarise(
  Min = min(hgt, na.rm = TRUE),
  Q1 = quantile(hgt, probs = .25, na.rm = TRUE),
  Median = median(hgt, na.rm = TRUE),
  Q3 = quantile(hgt, probs = .75, na.rm = TRUE),
  Max = max(hgt, na.rm = TRUE),
  Mean = mean(hgt, na.rm = TRUE),
  SD = sd(hgt, na.rm = TRUE),
  n = n(),
  Missing = sum(is.na(hgt))
)

# Plotting the ched.di and hgt (box)
bdims$che.di %>% boxplot(ylab = "Chest diameter")
bdims$hgt %>% boxplot(ylab = "Height")

# scatter plot
plot(che.di ~ hgt, data = bdims, xlab = "hgt", ylab = "che.di")
plot(bdims$che.di)
plot(bdims$hgt)

# Histogram with overlay to check normalization
hist(bdims$che.di, xlim = c(20,40),
     main = "Chest diameter",
     col = "skyblue", probability = TRUE)
bdimsov = rnorm(length(bdims$che.di), mean(bdims$che.di), sd(bdims$che.di))
lines(density(bdimsov, adjust = 2), col = "red", lwd = 2)
bdims$che.di %>% mean() %>% abline(v = ., col = 'red', lw = 2)


hist(bdims$hgt, xlim = c(140, 210),
     main = "Height",
     col = "skyblue", probability = TRUE)
bdimsov = rnorm(length(bdims$hgt), mean(bdims$hgt), sd(bdims$hgt))
lines(density(bdimsov, adjust = 2), col = "red", lwd = 2)
bdims$hgt %>% mean() %>% abline(v = ., col = 'red', lw = 2)

bdims_mod <- lm(che.di ~ hgt, data = bdims)
bdims_mod %>% summary()

pf(q = 327,1,505,lower.tail = FALSE)

plot(bdims_mod)

#bdims_mod %>% anova()
bdims_mod %>% summary() %>% coef()
bdims_mod %>% confint(level = 0.95)


2*pt(q = -1.90,df = 505, lower.tail=FALSE)

2*pt(q = 18.08,df = 505, lower.tail=FALSE)

# che.di 31.4 and hgt 192 observed 
-3.29 + 0.18*192

predict(bdims_mod, new = data.frame(hgt = 192))
31.4 - 31.78

######

library(stringr)
# Load data
Hand_length <- read.csv("Hand_length.csv")

# Exercise 1 --------------------------------------------------------------

plot(foot_length ~ hand_length, data = Hand_length)


#### Data cleaning 1####


# A few people have used cms! Let's scan the data and fix these values up.
Hand_length$hand_length <- ifelse(Hand_length$hand_length < 100,
                                  Hand_length$hand_length*10,
                                  Hand_length$hand_length)
Hand_length$foot_length <- ifelse(Hand_length$foot_length < 100,
                                  Hand_length$foot_length*10,
                                  Hand_length$foot_length)

#### Data cleaning 2####

# Someone add an extra 0 to mm...we can trim these off by dividing by 10.
Hand_length$hand_length <- ifelse(Hand_length$hand_length > 1000,
                                  Hand_length$hand_length/10,
                                  Hand_length$hand_length)
Hand_length$foot_length <- ifelse(Hand_length$foot_length > 1000,
                                  Hand_length$foot_length/10,
                                  Hand_length$foot_length)

#### Data cleaning 3####
sort(Hand_length$foot_length)
sort(Hand_length$hand_length)

# We have an unusal foot lenght of 108 mm, most likely a data entry error
Hand_length$foot_length <- ifelse(Hand_length$foot_length < 110,
                                  NA,
                                  Hand_length$foot_length)

# We have an unusal foot lenght of 750 mm, most likely a data entry error
Hand_length$foot_length <- ifelse(Hand_length$foot_length >310,
                                  NA,
                                  Hand_length$foot_length)

# We have an unusal hand lenght of 720 and 800 mm, most likely a data entry error
Hand_length$hand_length <- ifelse(Hand_length$hand_length > 300,
                                  NA,
                                  Hand_length$hand_length)

#Re-check
plot(foot_length ~ hand_length, data = Hand_length)

# Exercise 2 --------------------------------------------------------------

model1 <- lm(foot_length ~ hand_length-1, data = Hand_length)
model1 %>% summary()

#### interpretations ####

# The linear model was statistically significant, F(1,122) = 59.91, p < .001. 
# Hand length explained 32.9 % of the variability in foot length.


# Exercise 3 --------------------------------------------------------------
plot(model1)


#### interpretations ####
# Independence: 
# Independence was assumed as each foot and hand measurement came from different people.
# Linearity: 
# The scatter plot suggested a linear relationship. Other non-linear relationships
# were ruled out. There were no non-linear trends in the Residual vs. fitted plot.
# Normality of residuals: 
# Normal Q-Q plot didn't show any obvious departures from normality.
# Influential cases: 
# There appeared to be no influential cases.
# Homoscedasticity: 
# Homoscedasticity


# Homoscedasticity looks violated according to the scale-location plot. 
# The variance in residuals appeared to decrease across predicted values.


# Exercise 4 --------------------------------------------------------------

model1 %>% summary()
model1 %>% confint()

#### interpretations ####
# The estimated average MEAN foot length when hand length = 0 was 84.32 mm.
# The intercept of the regression was statistically significant, 
#a = 87.42, p = <.001, 95% CI (44.59, 130.26).
# For every one unit increase in hand length (mm), 
# the mean foot length was estimated to increase on average by 0.93 mm.
# The slope of the regression for hand length was statistically significant, 
#b = 0.91, p < .001, 95% CI (0.67, 1.14).


# Exercise 5 --------------------------------------------------------------

# Say your hand the foot measurement were 179 and 217 mm respectively.
# Using the estimated regression equation:
# Predicted Foot length = 84.35 + 0.93*179
# Predicted Foot length = 250.82
# This prediction has an error or residual of 217 - 250.82 = -33.82 mm. 
# The regression model did not predict this value well.

predict(model1, new =data.frame(hand_length=179))

# Exercise 6 --------------------------------------------------------------

# Overall, there was a statistically significant positive linear 
# relationship between hand length and foot length. 
# A person's hand length was estimated to explain up to 32.9 % 
# of the variability in foot length.
