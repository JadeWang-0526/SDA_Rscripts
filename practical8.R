library(sda)
View(rats)

# question a --------------------------------------------------------------

wilcox.test(y~diet, data=rats)

# question b --------------------------------------------------------------

rats$ranky<-rank(rats$y)

T<-coef(lm(ranky~diet, data=rats))["dietB"]

n<-1000  # number of iterations

Ts<-1:n  # vector for storing the test statsitics.

for(i in 1:n){
  rats$new_diet<-sample(rats$diet)
  Ts[i]<-coef(lm(ranky~new_diet, data=rats))["new_dietB"]
}

Ts

sum(abs(T)<Ts | -abs(T)>Ts)/n

# question c --------------------------------------------------------------

m1<-lm(y~diet, data=rats)
m2<-lm(y~diet+x, data=rats)
summary(m1)
summary(m2)

# question a --------------------------------------------------------------

ncl<-sum(darwin$cross>darwin$self)
ncl

binom.test(ncl, nrow(darwin))

# question b --------------------------------------------------------------

p<-sum(dbinom(ncl:15, size=15, prob=0.5))
p

1-pbinom(ncl-1, size=15, prob=0.5)
