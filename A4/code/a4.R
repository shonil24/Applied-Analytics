library(readr)
library(dplyr)
library(magrittr)
library(epitools)
library(car)

Assignment_4A <- read_csv("F:/MS/Sem 1/AA/Assignment 4/Assignment 4A.csv")

# Selecting specific columns and removing columns with all null values
calos <- Assignment_4A %>% dplyr::select(1:8, 10, 12, 14, 16, 18)
View(calos)

# Changing dataype to numeric
calos[,11] <- sapply(calos[,11], as.numeric)

# Filtered South Western Sydney  
calosLHN <- calos %>% filter(`Local Hospital Network (LHN)` == "South Western Sydney")

# Changing dataype to factor after filtering. 
calosLHN$`Local Hospital Network (LHN)` <- as.factor(calosLHN$`Local Hospital Network (LHN)`)

# Descriptive statistics
calosLHN %>% filter(`Local Hospital Network (LHN)` == "South Western Sydney" ) %>%
  summarise(
    Min = min(`Average length of stay (days)`, na.rm = TRUE),
    Q1 = quantile(`Average length of stay (days)`, probs = .25, na.rm = TRUE),
    Median = median(`Average length of stay (days)`, na.rm = TRUE),
    Q3 = quantile(`Average length of stay (days)`, probs = .75, na.rm = TRUE),
    Max = max(`Average length of stay (days)`, na.rm = TRUE),
    Mean = mean(`Average length of stay (days)`, na.rm = TRUE),
    SD = sd(`Average length of stay (days)`, na.rm = TRUE),
    n = n(),
    Missing = sum(is.na(`Average length of stay (days)`))
  )

# Plotting the avg length of stay and Local Hospital Network
calosLHN %>% boxplot(`Average length of stay (days)` ~ `Local Hospital Network (LHN)`, 
                       data = ., main = "Average length of stays in South Western Sydney hospitals", 
                       ylab = "Average length of stay (days) ", 
                       xlab = "Local Hospital Network", 
                       col = "red")

# According to CLT, When the sample size we use is large, typically defined as n>30, the sampling distribution of the mean is approximately normal, regardless of the variable's underlying population distribution.
# Module 5

# H0:- South Western Sydney hospitals have an average of length of stay (ALOS) of 4.5 days
t.test(calosLHN$`Average length of stay (days)`, mu = 4.5, conf.level = .95, alternative="two.sided")

#----------------------------------------------------------------------

tustud <- read_csv("F:/MS/Sem 1/AA/Assignment 4/Assignment 4b-3.csv")
View(tustud)

# Descriptive statistics
tustud %>%  summarise(
  Min = min(`Score before tutorial`, na.rm = TRUE),
  Q1 = quantile(`Score before tutorial`, probs = .25, na.rm = TRUE),
  Median = median(`Score before tutorial`, na.rm = TRUE),
  Q3 = quantile(`Score before tutorial`, probs = .75, na.rm = TRUE),
  Max = max(`Score before tutorial`, na.rm = TRUE),
  Mean = mean(`Score before tutorial`, na.rm = TRUE),
  SD = sd(`Score before tutorial`, na.rm = TRUE),
  n = n(),
  Missing = sum(is.na(`Score before tutorial`))
)

tustud %>%  summarise(
  Min = min(`Score after tutorial`, na.rm = TRUE),
  Q1 = quantile(`Score after tutorial`, probs = .25, na.rm = TRUE),
  Median = median(`Score after tutorial`, na.rm = TRUE),
  Q3 = quantile(`Score after tutorial`, probs = .75, na.rm = TRUE),
  Max = max(`Score after tutorial`, na.rm = TRUE),
  Mean = mean(`Score after tutorial`, na.rm = TRUE),
  SD = sd(`Score after tutorial`, na.rm = TRUE),
  n = n(),
  Missing = sum(is.na(`Score after tutorial`))
)

tustud$`Score before tutorial` %>% qqPlot(dist = "norm")
tustud$`Score after tutorial` %>% qqPlot(dist = "norm")

# You can see points falling outside the tails of the distribution. This suggests the tails are heavier than what we would expect under a normal distribution. You should be cautious about assuming normality for females. Fortunately, due to the large sample size, n=65, we don't have to worry too much.

# Central Limit Theorem

# Thanks to the CLT, introduced back in Module 5,
# we know that the sampling distribution of a mean will be approximately normally distributed, 
# regardless of the underlying population distribution when the sample size is large (i.e. n>30).
# This means that we can proceed with the two-sample t-test if the normality assumption is violated when the sample sizes in each group are greater than 30. 
# In this example, the sample sizes are much greater than 30 in each group, 
# so we can effectively ignore the issue with normality for the female data. 
# The CLT is extra incentive not to rely on small samples.

t.test(
  tustud$`Score after tutorial`, tustud$`Score before tutorial`,
  alternative = "two.sided",
  paired = TRUE
)

boxplot(
  tustud$`Score before tutorial`,
  tustud$`Score after tutorial`,
  ylab = "Scores"
)
axis(1, at = 1:2, labels = c("Score before tutorial", "Score after tutorial"))
#When we report the results of hypothesis testing for professional or scientific writing, we won't need to mention statistical hypotheses or our decision to reject or fail to reject H0. Instead we use the term "statistical significance" to refer to the rejection of H0. When we fail to reject H0, we would write that the test was NOT statistically significant. Read the following example summarising the results of the one-sample t-test.

# The paired-samples t-test, also known as the dependent samples t-test, is used to check for a statistically significant mean change or difference in these situations.

# A paired-samples t-test was used to test for a significant mean difference between stress levels before and after exercise. The mean difference following exercise was found to be -3.93 (SD = 5.68). Visual inspection of the Q-Q plot of the difference scores suggested that the data were approximately normally distributed. The paired-samples t-test found a statistically significant mean difference between stress levels before and after exercise, t(df=14)=???2.68, p<.018, 95% [-7.08 -0.79]. Stress levels were found to be significantly reduced after prisoners engaged in one hour of exercise