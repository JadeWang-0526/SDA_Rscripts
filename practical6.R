install.packages("devtools", repos = "https://cloud.r-project.org")
devtools::install_github("jarrodhadfield/sda")
library(sda)
library(help=sda)


# question a ---------------------------------------------------------------


head(collage_photo)
View(collage_photo)

m1 <- lm(y~as.factor(speed), data = collage_photo)
summary(m1)

coef(m1)[2]

e3<-coef(m1)[1]
e3
# expected number in three seconds

e5<-sum(coef(m1)[1:2])
e5
# expected number in five seconds

e5/e3


# question b --------------------------------------------------------------

collage_photo$trial<-rep(1:6,24)

m2<-lm(y~as.factor(speed)+trial, data=collage_photo)
summary(m2)

coef(m2)#["trial"]*6
coef(m2)["trial"]

coef(m2)["trial"]*1

coef(m2)["trial"]*6-coef(m2)["trial"]*1

coef(m2)["trial"]*6-(coef(m2)["trial"]*1+coef(m2)["as.factor(speed)5"])


# question c --------------------------------------------------------------

# The experiment was set up so each person assessed three photos at 5 seconds followed by three photos at 3 seconds. 
# The variable trial and the indicator variable speed==5 are therefore partly correlated/confounded. 
# This means it is harder to assess which variable is having a direct effect on the outcome, and the increase in the standard errors reflects this. 
# A better sampling strategy would be to get each person to alternate between 3 second and 5 second trials (so the 6 trials a person conducted might last 3,5,3,5,3 and 5 seconds) and make 50% of people start with 3 seconds and the other 50% (chosen at a random) start with 5 seconds. 
# This way trial and the indicator variable speed==5 would be uncorrelated and we could measure their independent effects more precisely.


# question d --------------------------------------------------------------

plot(m2,2)

# The residuals are clustered within certain quantiles (because the data are counts) and there seem to be too many small numbers and too few large numbers than what we expect from the normal.


# question e --------------------------------------------------------------

m3<-glm(y~as.factor(speed), data=collage_photo, family="poisson")
summary(m3)

coef(m3)[2]

exp(coef(m3)[2])


# question f --------------------------------------------------------------

m4<-glm(y~speed, data=collage_photo, family="poisson")

(log(2)-coef(m4)["(Intercept)"])/coef(m4)["speed"]


# question g --------------------------------------------------------------

m4<-glm(cbind(y,100-y)~as.factor(speed), data=collage_photo, family="binomial")
summary(m4)

e3<-100*plogis(coef(m4)[1])
e5<-100*plogis(sum(coef(m4)))

e5/e3


# question h --------------------------------------------------------------

res<-rep(NA,10)

for(i in 1:10){
  res[i]<-i^2
}

p<-seq(0,0.1,length=100)
p
logL<-rep(NA, 100)

for(i in 1:100){
  logL[i]<-sum(dbinom(collage_photo$y,size=100, prob=p[i], log=TRUE))
}

logL[50]

m1<-glm(cbind(y, 100-y)~1, data=collage_photo, family=binomial)

plot(logL~p, type="l")
abline(v=plogis(m1$coef))
