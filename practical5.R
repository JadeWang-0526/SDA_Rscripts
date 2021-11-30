Fertiliser <- read.table("/Users/jadewang/Desktop/SDA/fertil.txt", header = TRUE)
View(Fertiliser)
Fertiliser$FERTIL.LEVEL<-as.factor(Fertiliser$FERTIL)

summary(Fertiliser$FERTIL)
summary(Fertiliser$FERTIL.LEVEL)

anov <- lm(YIELD~FERTIL.LEVEL, data = Fertiliser)

fitted.values(anov)
residuals(anov)
?fitted.values

summary(anov)
plot(anov)
anova(anov)

options("contrasts")
contrasts(Fertiliser$FERTIL.LEVEL)
anov <- lm(YIELD~FERTIL.LEVEL, data = Fertiliser)
summary(anov)
contrasts(Fertiliser$FERTIL.LEVEL) <- "contr.sum"
anov1 <- lm(YIELD~FERTIL.LEVEL, data = Fertiliser)
summary(anov1)
contrasts(Fertiliser$FERTIL.LEVEL)
anov <- lm(YIELD~FERTIL.LEVEL, data = Fertiliser)
summary(anov)

breed <- factor(rep(c("Blackface", "Welsh", "Cross"),c(5,5,5)))
Cu <- c(6.5,7.9,7.4,6.8,8.1,10.4,9.8,11.1,10.6,9.2,6.9,9.2,8.4,7.6,9.7)
fit <- lm(Cu ~ breed)
anova(fit)

install.packages("R.matlab")
library(R.matlab)

A <- matrix( c(1, 0, 0, 0,
               1, 1, 0, 0,
               1, 0, 1, 0,
               1, 0, 0, 1), nrow=4, byrow=TRUE)
AI  <- solve(A)
AI
B <- AI %*% A
B

Trees <- read.table('trees.txt',header = TRUE)
View(Trees)
linmod <- lm(VOLUME~HEIGHT, data=Trees)
summary(linmod)
anova(linmod)
hist(rstandard(linmod))
plot(linmod)
qqnorm(linmod$VOLUME[linmod$HEIGHT], pch = 1, frame = FALSE)
residuals(linmod)
fitted.values(linmod)


plot(Trees$DIAMETER, Trees$VOLUME)
reg<-lm(Trees$VOLUME~Trees$DIAMETER)
hist(rstandard(reg))
plot(log(Trees$HEIGHT),log(Trees$VOLUME))
mreg <- lm(log(Trees$VOLUME)~log(Trees$HEIGHT)+log(Trees$DIAMETER))
plot(mreg)
summary(mreg)
hist(rstandard(mreg))

ad <- read.table("antidotes.txt", header = TRUE)
View(ad)
ad$ANTIDOTE.LEVEL <- as.factor(ad$ANTIDOTE)
ad$DOSE.LEVEL <- as.factor(ad$DOSE)
View(ad)
summary(ad$ANTIDOTE.LEVEL)
summary(ad$DOSE.LEVEL)
contrasts(ad$ANTIDOTE.LEVEL)
contrasts(ad$DOSE.LEVEL)
