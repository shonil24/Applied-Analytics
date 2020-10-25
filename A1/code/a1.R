library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)



bdims <- read_csv("F:/MS/Sem 1/AA/Assignment1/bdims.csv")

bdims$sex <- 
  bdims$sex %>% factor(levels = c(0,1), 
                       labels = c("Female","Male")) 


bdimsint <- bdims[c("che.di","sex")]
bdimsint %>% summary()

#$che.di %>% mean()
#bdims$che.di %>% median()
bdims$che.di %>% summary()
bdims$che.di %>% sd()
bdims$che.di %>% IQR()
#bdims$che.di %>% range()
#bdims$che.di %>% quantile()

bdimsmale <- bdims %>% filter(sex == "Male")
#bdimsmale$che.di %>% mean()
#bdimsmale$che.di %>% median()
bdimsmale$che.di %>% summary()
bdimsmale$che.di %>% sd()
bdimsmale$che.di %>% IQR()
#bdimsmale$che.di %>% range()
#bdimsmale$che.di %>% quantile()

bdimsfemale <- bdims %>% filter(sex == "Female")
#bdimsfemale$che.di %>% mean()
#bdimsfemale$che.di %>% median()
bdimsfemale$che.di %>% summary()
bdimsfemale$che.di %>% sd()
bdimsfemale$che.di %>% IQR()
#bdimsfemale$che.di %>% range()
#bdimsfemale$che.di %>% quantile()

# Summary statistics grouped by Sex for Male and Female
bdims %>% group_by(sex) %>% summarise(Min = min(che.di, na.rm = TRUE),
                                      Q1 = quantile(che.di, probs = .25, na.rm = TRUE),
                                      Median = median(che.di ,na.rim = TRUE),
                                      Q3 = quantile(che.di, probs = .75, na.rm = TRUE),
                                      Max = max(che.di, na.rm = TRUE),
                                      Mean = mean(che.di, na.rm = TRUE),
                                      SD = sd(che.di , na.rm = TRUE),
                                      InterquartileRange = IQR(che.di, na.rm = TRUE),
                                      N = n())
                                           
                                          
                              


bdims %>% histogram(~che.di|sex, col = "skyblue",
                       layout = c(1,2), data= .,
                       xlab = "Chest Diameter(cm)", 
                       ylab = "Frequency",
                       main = "Histogram of Male and Female Chest Diameter(cm)")

hist(bdimsmale$che.di, breaks = 15, xlim = c(20,40),
      xlab = "Chest Diameter(cm)", 
      ylab = "Density",
      main = "Histogram of Male Chest Diameter(cm)",
      col = "skyblue", probability = TRUE)

bdimsov = rnorm(length(bdimsmale$che.di), mean(bdimsmale$che.di), sd(bdimsmale$che.di))
lines(density(bdimsov, adjust = 2), col = "red", lwd = 2)
bdimsmale$che.di %>% mean() %>% abline(v = .,col = 'red',lw = 2)

hist(bdimsfemale$che.di, breaks = 15,
     xlab = "Chest Diameter(cm)", 
     ylab = "Density",
     main = "Histogram of Female Chest Diameter(cm)",
     col = "skyblue", probability = TRUE)

bdimsovf = rnorm(length(bdimsfemale$che.di), mean(bdimsfemale$che.di), sd(bdimsfemale$che.di))
lines(density(bdimsovf, adjust = 2), col = "red", lwd = 2)
bdimsfemale$che.di %>% mean() %>% abline(v = .,col = 'red',lw = 2)
