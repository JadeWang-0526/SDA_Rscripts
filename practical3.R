# sampling and hypothesis tests

# question 1 --------------------------------------------------------------

box <- 1:5
choose(5,3)

combs <-combn(box, 3)
rownames(combs) <- c("Ball1","Ball2","Ball3")
combs

colSums(combs)

sMeans <- round(colSums(combs)/3,2)
table(sMeans)

barplot(table(sMeans)/10, xlab="Sample Means", ylab="Probability", main="Probability distribution of the sample mean")

combs

sMeds <- combs[2,]
table(sMeds)

barplot(table(sMeds)/10, xlab="Sample Medians", ylab="Probability", main="Probability distribution of the sample median")

mean(sMeans)
mean(sMeds)

sum((sMeans)^2 * 1/10) - (mean(sMeans))^2
sum((sMeds)^2 * 1/10) - (mean(sMeds))^2


# question 2 --------------------------------------------------------------

die <- c(1,2,3,4,5,6)
m <- mean(die)
m

Esum <- m * 2 * 100
Esum

SD <- sqrt(sum((die - m)^2/6))
SD

SE <- sqrt(2 * 100) * SD
SE

q1 <- qnorm(0.005)
q2 <- qnorm(0.995)
q1
q2

pnorm(q2) - pnorm(q1)

CI <- c(630+q1*SE, 630+q2*SE)
CI

# question 3  -------------------------------------------------------------

O <- matrix(c(19,497,53,829), 2, 2)
O

E <- matrix(c(72 * 516/1398, 1326 * 516/1398, 72 * 882/1398, 1326 * 882/1398), 2, 2)
E

chi2 <- sum((O-E)^2/E)
chi2

pchisq(chi2, df=1, lower.tail=FALSE)

chisq.test(O)
chisq.test(O, correct=FALSE)


# question 4 --------------------------------------------------------------

rain<-c(139,130,131,141,140,125,132)
chisq.test(rain)

chisq.test(rain*16)

# question 5 --------------------------------------------------------------

bottles <- c(101, 103, 98, 104, 99, 103, 106, 104, 102, 100)

sum(bottles)/10
m <- mean(bottles)
m

sqrt(sum((bottles-m)^2/9))

SD <- sd(bottles)

SE <- SD/sqrt(10)
SE

t <- (m-100)/SE
t

2*pt(t, df=9, lower.tail=FALSE)

t.test(bottles, mu=100)

t.test(bottles-100)


# question 6 --------------------------------------------------------------

install.packages("datarium")
data(mice2, package="datarium")

data(mice2, package="datarium")
mice2

mice2$diff <- mice2$after - mice2$before
mice2

m <- mean(mice2$diff)
m

SD <- sd(mice2$diff)
SE <- SD/sqrt(nrow(mice2))
SE

t <- (m - 0)/SE
t

pt(t, df=nrow(mice2)-1, lower.tail=FALSE)

t.test(mice2$after, mice2$before, paired=TRUE, alt="greater")


# question 7 --------------------------------------------------------------

O <- c(224, 705)
O

E <- c(0.25 * (224+705), 0.75 * (224+705))
E

chi2 <- sum((O-E)^2/E)
chi2

pchisq(chi2, df=1, lower.tail=FALSE)

chisq.test(O, p=c(1/4, 3/4))

binom.test(O, p=1/4)
