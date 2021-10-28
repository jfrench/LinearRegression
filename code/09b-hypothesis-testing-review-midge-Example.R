### Midge Example
### data are i.i.d. N(mu, sigma^2), 
### mu and sigma^2 unknown
set.seed(1)
y <- c(1.64, 1.70, 1.72, 1.74, 1.82, 1.82, 1.82, 1.90, 2.08)
n <- length(y)
s <- sd(y)
m <- mean(y)

# 98% parameter ci for mu
t.test(y, conf.level = 0.98)
# [1.679014, 1.929875]
m + c(-1, 1) * s/sqrt(n) * qt(0.99, df = n - 1)

# 98% bootstrap ci
nsim <- 10000
ybar_boot <- numeric(nsim)
for (i in 1:nsim) {
  ybar_boot[i] <- mean(sample(y, replace = TRUE))
}
# quantile bootstrap confidence interval
quantile(ybar_boot, prob = c(0.01, 0.99))
# [1.720000, 1.906667]

# t test at alpha = 0.02
t.test(y, alternative = "greater", mu = 2)
tstat <- (m - 2)/(s/sqrt(n))
1 - pt(tstat, df = n - 1)
# fail to reject H0 since p-value is large