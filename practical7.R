devtools::install_github("jarrodhadfield/sda")
library(sda)
library(lme4)


# question a --------------------------------------------------------------
View(photo_long)
m1<-lm(y~type, data=photo_long)
summary(m1)

m2<-lmer(y~type+(1|person), data=photo_long)
summary(m2)


# question b --------------------------------------------------------------

# The standard error goes down in the mixed model because each type of photo
# This increases the precision with which we can measure the difference
# because the precision only depends on the within-person variance 
# but not the total variance (the within-person + the between-person variance)


# question c --------------------------------------------------------------

t.test(photo_wide$y.happy, photo_wide$y.grumpy ,paired=TRUE)

t.test(photo_wide$y.happy-photo_wide$y.grumpy)


# question d --------------------------------------------------------------

mu<-fixef(m2)[1]
person_variance<-VarCorr(m2)[[1]][1]
person_variance
residual_variance<-attr(VarCorr(m2), "sc")^2
residual_variance

pnorm(5, mu, sqrt(person_variance+residual_variance))

# whether this agrees with the proportion of grumpy photos that a score of 5 or less:
no_grumpy<-sum(photo_long$type=="grumpy")
no_grumpy_l5<-sum(photo_long$y[which(photo_long$type=="grumpy")]<5)
no_grumpy_l5/no_grumpy


# question a --------------------------------------------------------------
View(photo_long_full)

m3<-lmer(y~type+(1|person)+(1|respondent), data=photo_long_full)
# (1|person): assume same slope but different intercept (range between 1)
# (1+x|person): assume different slope and different intercept. 
# will calculate Vi, Vs and Cis
summary(m3)

m3np<-lmer(y~type+(1|respondent), data=photo_long_full)
anova(m3np,m3)

m3nr<-lmer(y~type+(1|person), data=photo_long_full)
anova(m3nr,m3)


# question b --------------------------------------------------------------

m4<-lmer(y~type+(1|person)+(1|respondent)+(1|respondent:person), data=photo_long_full)
summary(m4)
anova(m4,m3)
