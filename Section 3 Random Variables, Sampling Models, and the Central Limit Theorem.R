### Section 3: Random Variables, Sampling Models, and the Central Limit Theorem

beads <- rep( c("red", "blue"), times = c(2,3))
X <- ifelse(sample(beads,1) == "blue", 1, 0)
X


## Roulette
color <- rep(c("Black", "Red", "Green"), c(18,18,2))

# 1000 Independant Draws
n <- 1000
X <- sample(ifelse(color =="Red", -1, 1), replace = TRUE)
X[1:10]

## Sampling Model
X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))
S <- sum(X)
X
S

# Experiment
n <- 1000
B <- 10000
S <- replicate(B, {
  X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))
  sum(X)
})
mean(S<0)
hist(S)


## Distributions versus Probability Distributions

## Averages and Proportions

## The Big Short: Interest Rates Explained
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n , prob=c(1-p,p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

## The Big Short
# Monte Carlo
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob = c(1-p,p), replace = TRUE)
  sum(defaults * loss_per_foreclosure)
})
losses

E <- n*(p*loss_per_foreclosure + (1-p)*0)
E

SE <- sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))
SE

-loss_per_foreclosure*p/(1-p)

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l *(n*p - z*sqrt(n*p*(1-p)))/(n*(1-p) + z*sqrt(n*p*(1-p)))
x

## Example - More loans
p <- 0.04
r <- 0.05
x <- r * 180000
loss_per_foreclosure*p + x * (1-p)

# Larger number of loans
z <- qnorm(0.01)
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n

# Non-independant draws
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01,0.01,length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n,
                   prob=c(1-new_p, new_p), replace = TRUE)
})
mean(profit)
mean(profit>0)
