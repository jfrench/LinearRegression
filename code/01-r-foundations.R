## --------------------------------------------------------------------------------
# determine basic data type
typeof(1)
typeof(1L)
typeof("hello world!")


## --------------------------------------------------------------------------------
# is the object numeric?
is.numeric("hello world!")
is.numeric(1)
is.numeric(1L)


## --------------------------------------------------------------------------------
# compute the mean of 1, 2, ..., 10 and assign the name m
m <- mean(1:10)
m # print m
print(m) # print m a different way


## ---- eval=FALSE-----------------------------------------------------------------
## # vector creation
## c(1, 2, 5.3, 6, -2, 4)
## c("one", "two", "three")
## c(TRUE, TRUE, FALSE, TRUE)
## # sequences of values
## seq(1, 10)
## 1:10
## seq(1, 20, by = 2)
## seq(10, 20, len = 100)
## # replicated values
## rep(1:3, times = 3)
## rep(c("trt1", "trt2", "trt3"), times = 1:3)
## rep(1:3, each = 3)


## --------------------------------------------------------------------------------
v1 <- 1:5 # create a vector
v1 # print the vector
print(v1)
v2 <- c(1, 10, 11) # create a new vector
new <- c(v1, v2) # combine and assign the combined vectors
new # print the combined vector


## --------------------------------------------------------------------------------
# create some factor variables
f1 <- factor(rep(1:6, times = 3))
f1
f2 <- factor(c("a", 7, "blue", "blue", FALSE))
f2


## --------------------------------------------------------------------------------
# define a sequence 2, 4, ..., 16
a <- seq(2, 16, by = 2)
a


## --------------------------------------------------------------------------------
# extract subset of vector
a[c(2, 4, 6)]


## --------------------------------------------------------------------------------
# extract subset of vector using minus
a[-c(2, 4, 6)]


## --------------------------------------------------------------------------------
a[-(3:6)]


## ---- eval=FALSE-----------------------------------------------------------------
## # common functions
## x <- rexp(100) # sample 100 iid values from an Exponential(1) distribution
## length(x) # length of x
## sum(x) # sum of x
## mean(x) # sample mean of x
## var(x) # sample variance of x
## sd(x) # sample standard deviation of x
## range(x) # range of x
## log(x) # logarithm of x
## summary(x) # summary of x
## str(x) # structure of x


## ---- eval=FALSE-----------------------------------------------------------------
## # statistical calculations
## pnorm(1.96, mean = 0, sd = 1)
## qunif(0.6, min = 0, max = 1)
## dbinom(2, size = 20, prob = .2)
## dexp(1, rate = 2)
## rchisq(100, df = 5)


## ---- paged.print=FALSE----------------------------------------------------------
# create basic data frame
d <- c(1, 2, 3, 4)
e <- c("red", "white", "blue", NA)
f <- c(TRUE, TRUE, TRUE, FALSE)
df <- data.frame(d,e,f)
df


## ---- paged.print=FALSE----------------------------------------------------------
# name columns of data frame
names(df) <- c("ID", "Color", "Passed")
df


## ---- paged.print=FALSE----------------------------------------------------------
# create data frame with better column names
df2 <- data.frame(ID = d, Color = e, Passed = f)
df2


## ---- eval=FALSE, paged.print=FALSE----------------------------------------------
## # Extract parts of a data frame
## df3 <- data.frame(numbers = 1:5,
##                   characters = letters[1:5],
##                   logicals = c(TRUE, TRUE, FALSE, TRUE, FALSE))
## df3 # print df
## df3$logicals # access the logicals vector of df3
## df3[1, ] # access the first column of df3
## df3[, 3] # access the third column of df3
## df3[, 2:3] # access the column 2 and 3 of df3
## df3[, c("numbers", "logicals")] # access the numbers and logical columns of df3


## --------------------------------------------------------------------------------
# import data as data frame
dtf <- read.table(file = "https://raw.githubusercontent.com/jfrench/DataWrangleViz/master/data/covid_dec4.csv",
                  header = TRUE,
                  sep = ",")
str(dtf)


## ---- eval=FALSE-----------------------------------------------------------------
## # logical statements
## # a <- seq(2, 16, by = 2) # creating the vector a
## a
## a > 10
## a <= 4
## a == 10
## a != 10


## --------------------------------------------------------------------------------
# relationship between logicals & (and), | (or)
TRUE & TRUE
FALSE & TRUE
FALSE & FALSE
TRUE | TRUE
FALSE | TRUE
FALSE | FALSE


## ---- eval=FALSE-----------------------------------------------------------------
## # complex logical statements
## (a > 6) & (a <= 10)
## (a <= 4) | (a >= 12)


## ---- eval = FALSE---------------------------------------------------------------
## # accessing parts of a vector using logicals
## a
## a < 6
## a[a < 6]
## a == 10
## a[a == 10]
## (a < 6) | ( a == 10)
## a[(a < 6) | ( a == 10)]
## # accessing parts of a data frame
## # create a logical vector based on whether
## # a state_abb in dtf is "CA" or "CO"
## ca_or_co <- is.element(dtf$state_abb, c("CA", "CO"))
## ca_or_co
## # access the CA and CA rows of dtf
## dtf[ca_or_co,]

