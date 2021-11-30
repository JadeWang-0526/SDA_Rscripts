x <- c(0.8153888, 2.0757053, 2.8659843, 4.1106560, 5.0027966, 4.8531551, 5.9702902, 6.0748110, 6.8004483, 8.0787996, 8.8812575, 8.8883301, 9.172951, 9.9268888)
y <- c(5.653991e+01, 1.693266e+02, 7.852305e+02, 1.296308e+04, 1.006482e+05, 7.153738e+04, 9.314459e+05, 1.191031e+06, 6.333554e+06, 1.200157e+08, 7.598571e+08, 7.711421e+08, 1.491392e+09, 8.433627e+09)

lm(y~x)
plot(x,y)
plot(x, y^0.5)
a = lm(y^0.5~x)
plot(x, log(y))
f = lm(log(y)~x)
summary(f)
summary(a)


Reduced.fats <- read.table("/Users/jadewang/Desktop/SDA/reduced_fats.txt", header=T, sep="\t")
View(Reduced.fats)
plot(Reduced.fats$WEIGHT,Reduced.fats$FAT)
reg <- lm(FAT~WEIGHT, data=Reduced.fats)
anova(reg)
summary(reg)

qt(p=0.05/2, df=29, lower.tail=FALSE)

seeds <- read.table("/Users/jadewang/Desktop/SDA/seeds.txt", header=T, sep="\t")
View(seeds)
plot(seeds$PLANDEN,seeds$SEEDWGHT)
reg <- lm(PLANDEN~SEEDWGHT, data=seeds)
reg <- lm(SEEDWGHT~PLANDEN, data = seeds)
anova(reg)
summary(reg)
