#Q2 pr(x=5)
dbinom(x=5, size=25, prob=0.2)

#q3 pr(x<=10)
pbinom(q=10, size=25, prob=0.2, lower.tail = TRUE)
1-pbinom(q=10, size=25, prob=0.2, lower.tail = FALSE)

#q4 pr(x>20)
pbinom(20, 25, 0.2, lower.tail = FALSE)

#q5 pr(x=0) . lambda is mean which is 1.1
dpois(x=0, lambda = 1.1)

#q6 pr(x>0)
ppois(q=0, lambda = 1.1, lower.tail = FALSE)
1-ppois(q=0, lambda = 1.1, lower.tail = TRUE)#1 - pr(x<=q)
1-dpois(x=0, lambda = 1.1)

#q7 pr(x<3) or pr(x<=2)
ppois(q=2, lambda = 1.1) #lower trail=true is by default
1-ppois(q=2, lambda = 1.1, lower.tail = FALSE)#1-pr(x<3)=pr(x<=2)

#q8 pr(x>5)
ppois(q=5, lambda = 1.1, lower.tail = FALSE)

#q9 same as q7
ppois(q=2, lambda = 1.1)

#q10 Pr(1<=X<=2) ..lower trail=true
ppois(q=2, lambda = 1.1)-ppois(q=0, lambda = 1.1) #pr(x<=2)-pr(x<=0)
dpois(x = 2, lambda = 1.1) + dpois(x = 1, lambda = 1.1)

#q11 pr(x=2) + pr(x=4) ... 2,10,2 ..2-10 values will have 2 intervals
dpois(x=seq(2, 10, 2), lambda = 1.1)
sum(dpois(x=seq(2, 10, 2), lambda = 1.1))

#q12 vehicle..continuous distri pr(x<50)
pnorm(q=50 , mean=54.6 , sd=4.3)

#q13 vehicle..pdisb pr(x<55)
pnorm(q=55 , mean=54.6 , sd=4.3)

#q14 vehicle..pdisb pr(x<60)
pnorm(q=60 , mean=54.6 , sd=4.3)

#q15 pr(X=x)=0 infinity no of values p(x>60)
pnorm(q=60 , mean=54.6 , sd=4.3, lower.tail = FALSE)
#dnorm is density. curve of normal dis. density is not probability. its just for a curve. its continous so density will give discreate
#Also I think density is suitable to calculate individual values. disreate. for single probab

#q16  pr(x>60)
pnorm(q=63 , mean=54.6 , sd=4.3, lower.tail = FALSE)

#q17 pr(55>X>65)
pnorm(q = 65, mean = 54.6, sd = 4.3) - pnorm(q = 55, mean = 54.6, sd = 4.3)

#q18 pr(53>X>67)
pnorm(q = 67, mean = 54.6, sd = 4.3) - pnorm(q = 53, mean = 54.6, sd = 4.3)

#q19 
qnorm(0.95,54.6,4.3)

#q20 
qnorm(0.05,54.6,4.3)

#q21 
# so pnorm here is to get where the first car's speed lie in the percentile?
p1 <- pnorm(59,54.6,4.3)-0.29
qnorm (p1, 54.6, 4.3)

#---
#Q2- Pr(X=5)
dbinom(x=5,size=25,prob=0.2)

#Q3- Pr(X<=10)
pbinom(10, 25, 0.2, lower.tail = TRUE)
1-pbinom(10, 25, 0.2, lower.tail = FALSE)

#Q4- Pr(X>20)
pbinom(20, 25, 0.2, lower.tail = FALSE)
1-pbinom(20, 25, 0.2, lower.tail = TRUE)

#Q5 -Pr(X=0)
dpois(x=0,lambda = 1.1)

#Q6 Pr(X>0)
ppois(q=0, lambda = 1.1, lower.tail = FALSE)
1-ppois(q=0, lambda = 1.1, lower.tail = TRUE)#1- Pr(X<=q)
1-dpois(x=0,lambda = 1.1)

#Q7 Pr(X<3) OR Pr(X<=2)
ppois(2,1.1, lower.tail = TRUE)
1-ppois(2,1.1, lower.tail = FALSE)#1-Pr(X>3)=Pr(X<=2)

#Q8 Pr(X>5)
ppois(5,1.1, lower.tail = FALSE)

#Q9 Pr(X<=2)
ppois(2,1.1)

#Q10 Pr(1<=X<=2)
ppois(2,1.1)-ppois(0,1.1)#Pr(X<=2)-Pr(X<=0)
dpois(x = 2, lambda = 1.1) + dpois(x = 1, lambda = 1.1)

#Q11 Pr(X=2)+Pr(X=4)+....
sum(dpois(x=seq(2,1000000,2),lambda = 1.1))

#Q12 Pr(X<50)
pnorm(50,mean=54.6, sd=4.3)

#Q13 Pr(X<55)
pnorm(55,mean=54.6, sd=4.3)

#Q14 Pr(X<60)
pnorm(60,mean=54.6, sd=4.3)

#Q15 Pr(X>60)
pnorm(60,mean=54.6, sd=4.3, lower.tail = FALSE)

#Q16 Pr(X>63)
pnorm(63,mean=54.6, sd=4.3, lower.tail = FALSE)

#Q17 Pr(55<X<65)
pnorm(q = 65, mean = 54.6, sd = 4.3) - pnorm(q = 55, mean = 54.6, sd = 4.3)

#Q18 Pr(53<X<67)
pnorm(q = 67, mean = 54.6, sd = 4.3) - pnorm(q = 53, mean = 54.6, sd = 4.3)


#19
qnorm(0.95,54.6,4.3)

#20
qnorm(0.05,54.6,4.3)

#21

p1<-pnorm(59,54.6,4.3)-0.29
qnorm (p1,54.6,4.3)