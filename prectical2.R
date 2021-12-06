# Probability distribution


# question 1 --------------------------------------------------------------

n <- 200
p <- 12/1000
dbinom(0, size=n, prob=p)

# question 2 --------------------------------------------------------------

n <- 10
p <- 0.5
dbinom(5, size=n, prob=p)

n <- 10
p <- 0.5
sum(dbinom(6:10, size=n, prob=p))

pbinom(5, size=n, prob=p, lower.tail=FALSE)

(1-dbinom(5, size=n, prob=p))/2

# question 3 --------------------------------------------------------------

l <- 3
1-sum(dpois(0:2, lambda=l))

1-ppois(2, lambda=l)

ppois(2, lambda=l, lower.tail=FALSE)


# question 4 --------------------------------------------------------------

den <- curve(dnorm, -2, 2, n=999, ylim=c(0,0.45), yaxs="i")
toShade <- den$x > -1 & den$x < 1
polygon(c(-1,den$x[toShade],1), c(0,den$y[toShade],0), col="grey80", border=1)
abline(v=c(-1,1), col='darkred', lty=2, lwd=2)

x <- -0.75
1 - 2*pnorm(x)

x <- -1
1 - 2*pnorm(x)

x <- -1.5
1 - 2*pnorm(x)

x <- -2
1 - 2*pnorm(x)

x <- -c(0.75, 1, 1.5, 2)
for (i in x) {
  p <- 1 - 2*pnorm(i)
  print( paste("The result for |X| <",i ,"is", round(p,3) ))
}

x <- -c(0.75, 1, 1.5, 2)
1 - 2*pnorm(x)

centralP <- c(0.5, 0.9, 0.95)
tailP <- (1-centralP)/2
-qnorm(tailP)

q <- c(0.25, 0.5, 0.75)
qnorm(q)


# question 5 --------------------------------------------------------------

mean <- 750
sd <- 100
curve(dnorm(x, mean=mean, sd=sd), from=mean-3*sd, to=mean+3*sd)
abline(v=c(650, 850), col='darkred')

left <- pnorm(650, mean=mean, sd=sd)
right <- pnorm(850, mean=mean, sd=sd, lower.tail=FALSE)
1 - (left+right)

pnorm(600, mean=mean, sd=sd)


# question 6 --------------------------------------------------------------

x <- 0:16
n <- 16
p <- 1/4
probs <- dbinom(x, size=n, prob=p)

m <- sum(x*probs)
m

sum(x^2*probs) - m^2

barplot(probs, names.arg=x, ylab="P(x)", xlab="x")

# question 7 --------------------------------------------------------------

curve(dnorm, from=-3, to=3)

curve(dnorm, from=-3, to=3)
quartiles <- qnorm(c(0.25,0.5,0.75))
abline(v=quartiles, col='darkred', lwd=2)

curve(pnorm, from=-3, to=3)

curve(pnorm, from=-3, to=3)
abline(v=quartiles, col='darkred', lwd=2)
abline(h=c(0.25, 0.5, 0.75), col='darkred', lwd=2)
