data(teengamb, package = "faraway") #load data

# fit model regressing gamble on all predictors
lmod <- lm(gamble ~ sex + status + income + verbal, 
           data = teengamb)

# find average of predictor valuesas.
(mavg <- colMeans(teengamb[teengamb$sex == 0, ]))
(favg <- colMeans(teengamb[teengamb$sex == 1, ]))

# mavg and favg aren't data frames, so we need to
# convert them
mavg = as.data.frame(t(mavg))
favg = as.data.frame(t(favg))

# confidence interval for mean of male w/ average values of predictors
predict(lmod, new = mavg, interval = "confidence")
# confidence interval for mean of female w/ average values of predictors
predict(lmod, new = favg, interval = "confidence")
# prediction interval for actual response of male w/ average values of predictors
predict(lmod, new = mavg, interval = "prediction")
# prediction interval for actual response of female w/ average values of predictors
predict(lmod, new = favg, interval = "prediction")

# find 0.05 quantiles for each column of predictors
(m.05 <- apply(teengamb[teengamb$sex == 0, ], 2, quantile, prob = 0.05))
(f.05 <- apply(teengamb[teengamb$sex == 1, ], 2, quantile, prob = 0.05))
m.05 = as.data.frame(t(m.05))
f.05 = as.data.frame(t(f.05))

# At 98% confidence level
# confidence interval for mean of male w/ .05 quantiles of predictors
predict(lmod, new = m.05, interval = "confidence", level = 0.98)
# confidence interval for mean of female w/ .05 quantiles of predictors
predict(lmod, new = f.05, interval = "confidence", level = 0.98)
# prediction interval for actual response of male w/ .05 quantiles of predictors
predict(lmod, new = m.05, interval = "prediction", level = 0.98)
# prediction interval for actual response of female w/ .05 quantiles of predictors
predict(lmod, new = f.05, interval = "prediction", level = 0.98)

