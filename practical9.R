# last week!!
x <- 1:10
y <- c(1,-1)
x * y

plot(x, x**2, main="A simple plot")

mu    <- 100 
stdev <- 5   
genes <- 100  
reps  <- 3   

control <- rnorm(genes*reps, mean=mu, sd=stdev)
treated <- rnorm(genes*reps, mean=mu, sd=stdev)

control <- matrix(data=control, nrow=genes, ncol=reps)
treated <- matrix(data=treated, nrow=genes, ncol=reps)

head(control)
head(treated)

plot(rowMeans(control), rowMeans(treated), xlab="control", ylab="treated", main="Average gene expression")
abline(h=mu, v=mu, lty=2)

t.test(control[1,], treated[1,], var.equal=TRUE)

t.test(control[1,], treated[1,], var.equal=TRUE)$p.value

pvalues <- rep(1, times=genes) 
for (i in 1:genes) {
  t.test(control[1,], treated[1,], var.equal=TRUE)$p.value
  pvalues[i] <- 0.5
}

range(pvalues)

hist(pvalues)

alpha <- 0.05

any(pvalues < alpha)
sum(pvalues < alpha)

sum(pvalues < alpha) / genes

minips <- c(0.9, 0.04, 0.7, 0.3, 0.01, 0.6)

ifelse(minips < 0.05, "red", "black")

plot(minips, col=ifelse(minips < 0.05, "red", "black"))
abline(h=0.05, lty=2)

mu1   <- 100
mu2   <- 110

stdev <- 5
genes <- 100
reps  <- 3
