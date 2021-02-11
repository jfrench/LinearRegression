# Some examples related to hypothesis test and confidence intervals

# Sample 10 observations from a N(1, 1) population.
set.seed(8) # for reproducible results
y <- rnorm(10, mean = 1, sd = 1)

# Assume the mean is unknown, but the variance is known.
# Test whether H0: mu = 0 versus Ha: mu != 0 at alpha = 0.10.
# If H0 is true, then (ybar - 0)/(1/sqrt(10)) = sqrt(10)*ybar ~ N(0, 1).

# plot sampling distribution of test statistic when H0 is true.
# i.e., plot the null distribution
# show with 0.05 and 0.95 quantiles
curve(dnorm, -3, 3, xlab = "z*", 
      ylab = "density", 
      main = "null distribution of test statistic") 
abline(v = qnorm(c(0.05, 0.95))) # quantiles

# compare to approximation from real data
nsim = 10000
sim_stats = numeric(nsim)
for (i in 1:nsim) {
  ysim = rnorm(10, 0, sd = 1)
  sim_stats[i] = sqrt(10) * mean(ysim)
}
lines(density(sim_stats), col = "orange")
legend("topleft", legend = c("true", "simulated"), lty = 1, col = c("black", "orange"))

# compute test tstatistic
tstat <- sqrt(10) * mean(y) # compute test statistic for the observed data

# plot test statistic on null distribution
abline(v = tstat, col = "blue") # test statistic in rejection region

# compute p-value
2 * (1 - pnorm(tstat))
# there is weak evidence that the population mean differs from zero

# A confidence interval for the population mean
mean(y) - qnorm(0.975) * 1/sqrt(10)
mean(y) + qnorm(0.975) * 1/sqrt(10)

# Produce 100 95% confidence intervals (using a sample of size n = 10)
# for the population mean when the actual population is N(0, 1)

# obtain 100 samples from the population
samples = matrix(0, nrow = 100, ncol = 10) 
for (i in 1:100) samples[i,] = rnorm(10)	

# calculate the sample mean for each sample
means = rowMeans(samples) #calculates the mean of each row

#calculate the lower and upper bounds for the 95% CIs
lb = means - qnorm(.975) * 1/sqrt(10)
ub = means + qnorm(.975) * 1/sqrt(10)

#plot the confidence intervals.  Highlight the ones that don't
#contain the true population mean (zero) in orange.

#create blank plot
plot(range(c(lb,ub)), c(1,100), xlab = "", ylab = "interval", type = "n")
title("Interpretation of a Confidence Interval") # title plot
abline(v = 0, col = "blue") # plot true mean
#plot each interval
for (i in 1:100) lines(c(lb[i],ub[i]), c(i,i))
#highlight intervals missing 0 in orange
for (i in 1:100) {
  if (lb[i] > 0 | ub[i] < 0 )   {
    lines(c(lb[i],ub[i]), c(i,i), col = "orange")
  }
}

### Bootstrap confidence intervals
nsim = 4000
# Suppose data are i.i.d. N(0, 1)
# Then the sample mean for 10 observations has a N(0, 1/10) sampling distribution

# calculate the sample mean of nsim random samples of size 10
ybar  = numeric(nsim)
for (i in 1:nsim) ybar[i] = mean(rnorm(10))

# plot empirical distribution versus true sampling distribution
plot(density(ybar), xlab = "sample mean", col = "blue",
     main = "empirical distribution of sample mean")
x = seq(-1, 1, len = 100) # vector for plotting
lines(x, dnorm(x, sd = 1/sqrt(10)))
legend("topleft", legend = c("truth", "estimate"), 
       lty = 1, col = c("black", "blue"), bty = "n")

# generate bootstrap sample
boot_ybar = numeric(nsim)
for (i in 1:nsim) boot_ybar[i] = mean(sample(y, replace = TRUE))
# a comparison of the empirical, bootstrap, and true sampling distribution
# of ybar for this example (centered at zero for the bootstrap)
x = seq(-1, 1, len = 100)
plot(x, dnorm(x, sd = 1/sqrt(10)), xlab = "sample mean", ylab = "density", type = "l")
title("Comparison of Empirical, Bootstrap,\nand True Sampling Distributions of the Sample Mean")
lines(density(ybar), col = "blue")
lines(density(boot_ybar - mean(y)), col = "orange")
legend("topleft", legend = c("true", "empirical", "bootstrap"), 
       col = c("black", "blue", "orange"), lty = 1, bty = "n")

# 95% bootstrap confidence interval from bootstrap distribution
quantile(boot_ybar, prob = c(0.025, 0.975))
plot(density(boot_ybar), col = "orange", 
     xlab = "sample mean", 
     main = "bootstrap distribution of sample mean")
abline(v = quantile(boot_ybar, prob = c(0.025, 0.975)))