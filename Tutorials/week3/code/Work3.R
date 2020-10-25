library(readr)
library(magrittr)
library(dplyr)

Mobilephone <- read_csv("F:/MS/Sem 1/AA/week3/Mobilephone.csv")
View(Mobilephone)

#Q1

Mobilephone$usr <- 
  Mobilephone$usr %>% factor(levels = c("R", "S", "U"), 
                             labels = c("Rural", "Suburban", "Urban")) 

head(Mobilephone$usr)

Mobilephone$SmartPhone <- 
  Mobilephone$SmartPhone %>% factor(levels = c(1, 2, 3), 
                             labels = c("Cell, smartphone", "Cell, not a smartphone", "No cell")) 

#Q2

Mobilephone %>% xtabs(~SmartPhone+usr, data=.) %>% addmargins()
#or
table(Mobilephone$SmartPhone, Mobilephone$usr) %>% addmargins()
#Add margins gives sums

#Q3
#pr(Rural)=389/2166
#Q3-Q6

table(Mobilephone$SmartPhone, Mobilephone$usr) %>% prop.table() %>% addmargins()
389/2166
#389/2166 = 0.1795937

t1 <- table(Mobilephone$SmartPhone, Mobilephone$usr) %>% prop.table() %>% addmargins()

t1[1][1]
#.401
#.126
#0.86
#.285

#Q7
table(Mobilephone$SmartPhone, Mobilephone$usr) %>% prop.table(margin = 2) %>% addmargins()
#proportion means any value/2166
#margin  2 mean any value by that column total i.e any/389

#q8
#.424

#pr(urban | SmartPhone) -> .43 (given a smartphone whats the probabilty that its urban)

table(Mobilephone$SmartPhone, Mobilephone$usr) %>% prop.table(margin = 1) %>% addmargins()
#.31
#margin about the smartphone as its given

#q9
t2 <- table(Mobilephone$SmartPhone, Mobilephone$usr) %>% prop.table(margin = 2) %>% addmargins()
t2 %>% barplot(main="Smartphone ownership by region",
               ylab = "proportions within region",
               ylim = c(0,0.8),
               legend = rownames(t2), beside = TRUE,
               args.legend = c(x="top", horiz=TRUE, title="Smartphone Ownership"),
               xlab = "Region")

#P(A|B)=P(A)
#P(A and B)= p(A)*P(B)
#How could we workout the probability that a person has a smartphone given they owned a cellphone? :869/(869+997)
#Pr(smartphone|cell)=869/(869+997)