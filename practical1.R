# intro to R and probability
print("This function simply prints some text to your screen")
?print
sqrt(4)
ages <- c(40,36,10,8,1,1,88)
ages
sum(ages)
mean(ages)
max(ages)
min(ages)
range(ages)
sort(ages)
unique(ages)
??deviation
sd(ages)
plot(ages)


nameObj <- c("Homer","Marge","Bart","Lisa","Maggie","Snowball","Abraham")
names(ages) <- nameObj
ages
ages[1]
ages[c(1,5)]
ages[2:5]
ages[-c(1,5)]
ages[-c(2)]
mean(ages[-c(2)])
no_snowball<-ages[-c(6)]
no_snowball
mean(no_snowball)

ages>=10
ages == 1
sum(ages > 10)
ages[ages > 10]
ages[ages > mean(ages)]
class(ages)
ages

barplot(ages)
pie(ages)
hist(ages)
hist(ages, col="skyblue", main="My blue histogram", ylab="Number of individuals")
pdf("histogram.pdf")
hist(ages, col="skyblue", main="My blue histogram", ylab="Number of individuals")
dev.off()
getwd()
list.files()
num <- scan("num.txt")
num
tab <- read.table("tab.txt")
tab
boxplot(tab, col=c("red","green","blue"))
tab[c(1,6,10), c(1,3)]
tab[1:5, 1:3]
tab[c(1,6,10), ] 
tab[, c(1,3)]  
summary(tab)
class(tab)
lapply(tab, sd)#, na.rm = TRUE)
sapply(tab, mean)#, na.rm = TRUE)
colMeans(tab)#, na.rm = TRUE)
mean(tab[,1])
?lapply
tab[tab[1]>tab[3]]


coin <- c(1,0)
sample(coin, size=1)
sample(coin, size=1)
sample(coin, size=1)
sample(coin, size=1)
s <- sample(coin, size=20, replace=TRUE)
s
sum(s)
s[s == 1]
length(s[s == 1])
length(s)

tosses <- sample(coin, size=10, replace=TRUE)
sum(tosses)
tosses <- sample(coin, size=10, replace=TRUE)
sum(tosses)


tosses <- sample(coin, size=100, replace=TRUE)
sum(tosses) / 100
tosses <- sample(coin, size=1000, replace=TRUE)
sum(tosses) / 1000
tosses <- sample(coin, size=10000, replace=TRUE)
sum(tosses) / 10000
tosses <- sample(coin, size=100000, replace=TRUE)
sum(tosses) / 100000
tosses <- sample(coin, size=1000000, replace=TRUE)
sum(tosses) / 1000000
tosses <- sample(coin, size=1000000, replace=TRUE)

frac1 <- sum(tosses[1])/1
frac2 <- sum(tosses[1:2])/2
frac3 <- sum(tosses[1:3])/3
frac4 <- sum(tosses[1:4])/4
frac5 <- sum(tosses[1:5])/5
frac6 <- sum(tosses[1:6])/6
frac7 <- sum(tosses[1:7])/7

frac_heads <- c(frac1, frac2, frac3, frac4, frac5, frac6, frac7)
plot(frac_heads, type="b", ylim=c(0,1), xlab="Number of tosses", ylab="Fraction of Heads")

abline(h=0.5, col='red')
frac10 <- sum(tosses[1:10])/10
frac100 <- sum(tosses[1:100])/100
frac1k <- sum(tosses[1:1000])/1000
frac10k <- sum(tosses[1:10000])/10000
frac100k <- sum(tosses[1:1e5])/1e5
frac1000k <- sum(tosses[1:1e6])/1e6

frac_heads_big <- c(frac10, frac100, frac1k, frac10k, frac100k, frac1000k)
plot(frac_heads_big, type="b", ylim=c(0,1), xlab="Larger number of tosses", ylab="Fraction of Heads")

abline(h=0.5, col='red')


dice<-c(1,2,3,4,5,6)
test <- sample(dice, size=10000, replace=TRUE)
frac10 <- sum(test[1:10])/10
frac100 <- sum(test[1:100])/100
frac1k <- sum(test[1:1000])/1000
frac10k <- sum(test[1:10000])/10000
frac100k <- sum(test[1:1e5])/1e5
frac1000k <- sum(test[1:1e6])/1e6

frac_heads_big <- c(frac10, frac100, frac1k, frac10k, frac100k, frac1000k)
plot(frac_heads_big, type="b", ylim=c(0,1), xlab="Larger number of tosses", ylab="Fraction of Heads")

