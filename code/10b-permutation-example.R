# why does the permutation test work?
set.seed(2)

# generate some simple data where a relationship exists between y and x
x <- rnorm(20)
y <- 1 + 3 * x + rnorm(20, sd = 0.5)
# a clear linear relationship
plot(y ~ x)

# permute response and compare to x
sy <- sample(y)
plot(sy ~ x)

# extract f statistic for observed data set
fobs <- summary(lm(y ~ x))$fstat[1]
nreps <- 4000
fsim <- numeric(nreps)
# nreps times:
# 1. permute the response
# 2. regress the permuted response on the predictor
# 3. extract the f statistic from the model fit
# 4. store the f statistic for later use
for(i in 1:nreps){
  fsim[i] <- summary(lm(sample(y) ~ x))$fstat[1]
}

# look at distribution of simulated f statistics
plot(density(fsim), xlab = "fsim", main = "permutation distribution of fsim")

# compare to observed f statistic (on appropriate scale)
plot(density(fsim), xlab = "fsim", main = "permutation distribution of fsim", xlim = c(0, max(fsim, fobs)))
abline(v = fobs)
mean(fsim >= fobs)

# generate some simple data where a relationship DOES NOT exist between y and x
y <- 1 + rnorm(20, sd = 0.5)
# no clear linear relationship
plot(y ~ x)

# permute response and compare to x
# looks similar to previous plot
sy <- sample(y)
plot(sy ~ x)

# extract f statistic for observed data set
fobs <- summary(lm(y ~ x))$fstat[1]

nreps <- 4000
fsim <- numeric(nreps)
# nreps times:
# 1. permute the response
# 2. regress the response on the predictor
# 3. extract the f statistic from the model fit
# 4. store the f statistic for later use
for(i in 1:nreps){
  fsim[i] <- summary(lm(sample(y) ~ x))$fstat[1]
}

# look at distribution of simulated f statistics
plot(density(fsim), xlab = "fsim", main = "permutation distribution of fsim")

# compare to observed f statistic (on appropriate scale)
plot(density(fsim), xlab = "fsim", main = "permutation distribution of fsim", xlim = c(0, max(fsim, fobs)))
abline(v = fobs)
mean(fsim >= fobs)



