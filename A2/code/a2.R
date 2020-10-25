library(readr)
library(dplyr)
library(magrittr)
library(epitools)
library(car)

avgstay <- read_csv("F:/MS/Sem 1/AA/Assignment 2/avgstay.csv", skip = 12)

# Sapply is used to find and sum all the null values
sapply(avgstay, function(x) sum(is.na(x)))

# Selecting specific columns and removing columns with all null values
calos <- avgstay %>% dplyr::select(1:8, 10, 12, 14, 16, 18)
View(calos)

# Unique value counts
sapply(calos, function(x) length(unique(x)))
sapply(calos, function(x) unique(x))

# select Average length of stays(days) and display all NP values, display the count
avglosNP <- calos[,11] %>% filter(calos[,11] == 'NP')
count(avglosNP)

# Changing dataype to numeric
calos[,11] <- sapply(calos[,11], as.numeric)

# Assuming NP (i.e. NA after converting to numeric)  as outliers. This is because the reporting data include criteria where patients reported deaths, trasnfer to another hospital
# Replacing value to reported data which doesn't meet criteria is unlogical.
# Therefore, the reported data which doesn't meet criteria are considered as outliers

# Filtered Large and Medium peer groups 
calosPeer <- calos %>% filter(`Peer group` == "Large hospitals" | `Peer group` == "Medium hospitals")

# Changing dataype to factor (Peer group) after filtering. Changing to factor will include all the groups even after filtering. So change it to factor after splitting.
calosPeer$`Peer group` <- as.factor(calosPeer$`Peer group`)


# Removing outliers for Hypothesis testing
calosPeerF <- calosPeer %>% filter(!is.na(calosPeer$`Average length of stay (days)`))

# Descriptive statistics
calosPeerF %>% filter(`Peer group` == "Large hospitals" ) %>%
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

calosPeerF %>% filter(`Peer group` == "Medium hospitals" ) %>%
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

# Plotting the avg length of stay and peer group
calosPeerF %>% boxplot(`Average length of stay (days)` ~ `Peer group`, 
                      data = ., main = "Average length of stays", 
                      ylab = "Average length of stay (days)", 
                      xlab = "Peer group", 
                      col = "red")

# Histogram for Large Hospitals
calosPeerLarge <- calosPeerF %>% filter(`Peer group` == "Large hospitals")
hist(calosPeerLarge$`Average length of stay (days)`,
     xlab = "Average length of stay (days)",
     ylab = "Density",
     main = "Average length of stay (days) in Medium hospitals",
     col = "skyblue")
calosPeerLarge$`Average length of stay (days)` %>% mean() %>% abline(v = .,col = 'red',lw = 2)

# Histogram for Medium Hospitals
calosPeerMedium <- calosPeerF %>% filter(`Peer group` == "Medium hospitals")
hist(calosPeerMedium$`Average length of stay (days)`,
     xlab = "Average length of stay (days)", 
     ylab = "Density",
     main = "Average length of stay (days) in Medium hospitals",
     col = "skyblue")
calosPeerMedium$`Average length of stay (days)` %>% mean() %>% abline(v = .,col = 'red',lw = 2)

# H0:- Mean  of average length of stay (ALOS) in large hospitals greater than medium hospitals. > 0
t.test(
  `Average length of stay (days)` ~ `Peer group`,
  data = calosPeerF,
  var.equal = FALSE,
  alternative = "two.sided",
  paired = FALSE
)
