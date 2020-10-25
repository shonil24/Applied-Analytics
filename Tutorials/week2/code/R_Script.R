library(readr)
library(magrittr)
library(dplyr)
cdc <- read_csv("cdc.csv")
cdc$exerany <- factor(cdc$exerany,levels = c(1,0),
                      labels = c("Yes","No")) 
cdc$hlthplan <- factor(cdc$hlthplan,levels = c(1,0),
                       labels = c("Yes","No")) 
cdc$smoke100 <- factor(cdc$smoke100,levels = c(1,0),
                       labels = c("Yes","No")) 
cdc$gender <- factor(cdc$gender,levels = c("m","f"),
                     labels = c("Male","Female"))

levels(cdc$genhlth)

cdc$genhlth <- cdc$genhlth %>% factor(
  levels=c('excellent','very good','good','fair','poor'),
  ordered=TRUE) 

genhlth<- cdc$genhlth %>% table() %>% prop.table()
genhlth %>% barplot()

exerany<- cdc$exerany %>% table() %>% prop.table()
exerany %>% barplot()

hlthplan<- cdc$hlthplan %>% table() %>% prop.table()
hlthplan %>% barplot()

smoke<- cdc$smoke100 %>% table() %>% prop.table()
smoke %>% barplot()

gender<- cdc$gender %>% table() %>% prop.table()
gender %>% barplot()

healthgender <- table(cdc$genhlth, cdc$gender) %>% prop.table(margin = 2)
healthgender

healthgender %>%barplot(beside=TRUE,legend=rownames(healthgender))

healthsmoke <- table(cdc$genhlth, cdc$smoke100) %>% prop.table(margin = 2)
healthsmoke

healthsmoke %>%barplot(beside=TRUE,legend=rownames(healthsmoke))

bmi <- (cdc$weight / cdc$height^2) * 703
# Or also you can use mutate() function from the dplyr package.
cdc <- cdc %>% mutate(bmi= (cdc$weight / cdc$height^2) * 703)

cdc$bmi %>% boxplot()

cdc$bmi %>% hist()

cdc$bmi %>% summary

cdc$bmi %>% quantile(c(.25,.5,.75))

cdc$bmi %>% IQR()

cdc$bmi %>% sd()

#We can define the upper and lower fence for an outlier as values 1.5 times the IQR
#(Q3 - Q1 = 6.178516) above the third quartile, Q3 + (1.5*IQR) and below the first qu
#artile, Q1 - (1.5*IQR) . In this case the upper fence for an outlier is 38.15777.
UpperFence <- 28.89 + (1.5* 6.178516) #Calculate upper fence
UpperFence

# The lower fence for an outlier is 13.44223.
LowerFence <- 22.71 - (1.5* 6.178516) #Calculate lower fence
LowerFence 

# Now let's filter data to remove outliers. I used here between() function under dplyr package
cdc_clean <- cdc %>% filter(., between(cdc$bmi, 13.44223, 38.15777))

cdc_clean %>% boxplot( cdc_clean$bmi ~ cdc_clean$genhlth, by = cdc_clean$gender, data = .,ylab = "BMI", xlab = "General Health")