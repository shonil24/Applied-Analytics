library(readr)
library(dplyr)
library(epitools)
ames <- read_csv("F:/MS/Sem 1/AA/week6/ames.csv")

population <- ames$area
sample <- sample(population, 60)

# Q1
summary(sample)
hist(sample)
# Every student will have different values and mean

# Q2
mean.ci <- function(x, conf = 0.95) {
  
  alpha <- 1- conf
  t_crit <- qt(p = (1-alpha/2), df = (length(x)-1) )
  
  lb <- mean(x) - t_crit * ( sd(x) / sqrt (length(x) ) )
  ub <- mean(x) + t_crit * ( sd(x) / sqrt (length(x) ) )
  
  return(c(lb,ub))
}
# mean.ci is function which is being defining

# Q3
mean.ci(sample)

# Q4
mean.ci(sample, conf = 0.9)

#sorry maybe let me rephrase my question with an example. I am trying to understand the correlation between significance level and confidence level by using a t test. Let me propose a situation where a new drug is introduced, to find out the if the drug is effective, I decide to use the t test at 95% confidence level, which is the alpha at 5%. we test on patients. H0 = no effect, H1 = effective. if the result of the t-test is p-value > 5%, theoretically we reject H1, and conclude the new drug is ineffective. My question is, because the p value is greater than 5% so that means, more that 5% of the mean of the sampling distribution is completely out of the 95% confidence interval range, therefore, I can also say, the drug is only effective to less than 95% of the sample, am I correct?

# Q5
t.test(sample, conf = 0.95)
t.test(sample, conf = 0.9)

samp_mean <- rep(NA, 100)
samp_sd <- rep(NA, 100)
lower_vector <- rep(NA, 100)
upper_vector <- rep(NA, 100)
n <- 60

for(i in 1:100){
  samp <- sample(population, n)
  samp_mean[i] <- mean(samp)
  samp_sd[i] <- sd(samp)
  ci<- mean.ci(samp)
  lower_vector[i] <-ci[1]
  upper_vector[i] <-ci[2]
}

plot_ci <- function(lo, hi, m) {
  par(mar=c(2, 1, 1, 1), mgp=c(2.7, 0.7, 0))
  k <- length(lo)
  ci.max <- max(rowSums(matrix(c(-1*lo,hi),ncol=2)))
  
  xR <- m + ci.max*c(-1, 1)
  yR <- c(0, 41*k/40)
  
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE)
  abline(v=m, lty=2, col='#00000088')
  axis(1, at=m, paste("mu = ",round(m,4)), cex.axis=1.15)
  #axis(2)
  for(i in 1:k){
    x <- mean(c(hi[i],lo[i]))
    ci <- c(lo[i],hi[i])
    if((m < hi[i] & m > lo[i])==FALSE){
      col <- "#F05133"
      points(x, i, cex=1.4, col=col)
      # points(x, i, pch=20, cex=1.2, col=col)
      lines(ci, rep(i, 2), col=col, lwd=5)
    } else{
      col <- 1
      points(x, i, pch=20, cex=1.2, col=col)
      lines(ci, rep(i, 2), col=col)
    }
  }
}

plot_ci(lower_vector, upper_vector, mean(population))

cdc <- read_csv("F:/MS/Sem 1/AA/week6/cdc.csv")

# Q7
cdc$exerany <- factor(cdc$exerany, levels = c(1,0), labels = c("Yes", "No"))

cdc$exerany %>% table() %>% prop.table()

# Q8
prop.ci <- function(x,n, conf = 0.95) {
  
  alpha <- 1- conf
  prop=x/n
  z_crit <- qnorm(p = (1-alpha/2) )
  lb <- prop - z_crit *sqrt (prop*(1-prop)/n) 
  ub <- prop + z_crit * sqrt (prop*(1-prop)/n) 
  
  return(c(lb,ub))
}

# Q9
#p=x/n which means x=14914, n = total of both
#rounde decimal points at the end
prop.ci(14914, 14914+5086, conf = 0.95)

# Q10
binom.approx(14914, 14914+5086, conf.level = 0.95)
