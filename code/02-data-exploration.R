#options(width = 45) #nice Word formatting at 20pts
options(digits = 5, scipen = 2, show.signif.stars = FALSE)

##### Crash course in R

### Help in R

#gets help on the "lm" command
help(lm)
#another way to get help on the "lm" command
?lm
#searches for help on functions related to logarithm
??logarithm

###Creating vectors using c
c(1, 2, 5.3, 6, -2, 4)
c("one", "two", "three")
c(TRUE, TRUE, FALSE, TRUE)

###Creating vectors using seq

#sequence from 1 to 10 in increments of 1
seq(1, 10)
#same as seq(1,10)
1:10
#sequence of numbers from 1 to 20 in increments of 2
seq(1, 20, by = 2)
#sequence of numbers from 10 to 20 of length 100
seq(10, 20, len = 100)

###Creating vectors using rep
#repeat the vector 1:3 three times
rep(1:3, times = 3)
#trt1 once, trt2 twice, trt3 thrice
rep(c("trt1", "trt2", "trt3"), times = 1:3)
# repeat 1, 2, 3 3 times each
rep(1:3, each = 3)

###Assigning and accessing objects
#assign 1, 2, 3, 4, 5 to the variable v1
v1 <- 1:5
#accessing data stored in variable v1
v1
#create two vectors, then join them together in a new vector
v2 <- c(9, 10, 1)
new <- c(v1, v2)
new

###Creating factors
f1 <- factor(rep(1:6,3))
f1
f2 <- factor(c("a",7,"blue", "blue"))
f2

##Commonly Used Functions
x <- rnorm(50, mean = 5, sd = 1)#Other useful data commands
length(x) #return the length of x
sum(x) #sum the numbers in x
mean(x) #calculate the mean of the numbers in x
var(x) #calculate the variance of the numbers in x
sd(x) #calculate the standard deviation of x
median(x) #calculate the median of x
range(x) #calculate the range of x
log(x) #calculate the natural log of x
summary(x) #return 6-number summary of x

##Functions related to statistical distributions
#returns the probability that a normal random variable with
#mean 0 and standard deviation 1 is less than or equal to 1.96.
pnorm(1.96, mean = 0, sd = 1)
#returns the value x such that P(X <= x)=0.6 for which a uniform
#random variable on the interval [0, 1]
qunif(0.6, min = 0, max = 1)
#returns the probability that P(X=2) for X ~ Binom(n=20, p = 0.2).
dbinom(2, size = 20, prob = .2)
#returns the density of an exponential random variable with mean = 1/2.
dexp(1, rate = 2)
#returns a sample of 100 observations from a chi-squared random
#variable with 5 df
rchisq(100, df = 5)

###Creating data frames
#create a few vectors
d <- c(1, 2, 3, 4)
e <- c("red", "white", "blue", NA)
f <- c(TRUE, TRUE, TRUE, FALSE)
#creates dataframe and assigns it to mydataframe
mydataframe <- data.frame(d,e,f)
mydataframe
#rename columns of data frame
names(mydataframe) <- c("ID", "Color", "Passed")
mydataframe

#name columns while creating data frame
dataframe2 <- data.frame(ID = d, Color = e, Passed = f)
dataframe2

###Accessing data in dataframes
#access Color column
mydataframe$Color
#access first row
mydataframe[1,]
#access third column
mydataframe[,3]
#access ID column of dataframe2 and store it in newID
newID <- dataframe2$ID
newID

###Importing data from file
data <- read.table("~/Dropbox/UCD_Files/Teaching/R Course/example.txt",
                   header = TRUE, sep = "\t")
data

# data2 <- read.table(file = file.choose(), header = TRUE, sep = "\t")
# data2

###Accessing vectors using index vector
a <- seq(2, 16, by = 2)
a

#access the 2nd, 4th, and 6th elements of a
a[c(2, 4, 6)]
#access elements 3 through 6 of a
a[3:6]
#access all elements in a except the 2nd, 4th, and 6th elements
a[-c(2, 4, 6)]
#access all elements in a except elements 3 through 6
a[-(3:6)]

###Logical index vectors and operators
#values greater than 10
a > 10
#values less than or equal to 4
a <= 4
#values equal to 10
a == 10
#values not equal to 10
a != 10
#values greater than 6 and less than or equal to 10
(a > 6) & (a <= 10)
#values less than or equal to 4 or greater or equal to 12
(a <= 4) | (a >= 12)
#elements of a less than 6
a[a < 6]
#elements of a equal to 10
a[a == 10]
#elements of a less than 6 or equal to 10
a[(a < 6) | (a == 10)]

# 1.2 Kidney example
data(pima, package = "faraway") # load data from faraway package
str(pima) # view values and class of each variable in data.frame.

summary(pima) #numerical summaries of each variable
sort(pima$diastolic) #ordered blood pressure

# setting 0 that should be missing values to NA
pima$diastolic[pima$diastolic == 0]  <- NA
pima$glucose[pima$glucose == 0] <- NA
pima$triceps[pima$triceps == 0]  <- NA
pima$insulin[pima$insulin == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA

# converting test varible to factor, look at summary
pima$test <- factor(pima$test)
summary(pima$test)

# provide more descriptive labels
levels(pima$test) <- c("negative","positive")
summary(pima)

# a histogram of diastolic blood pressure
hist(pima$diastolic, xlab = "Diastolic", main = "")

# a density plot of diastolic blood pressure
plot(density(pima$diastolic, na.rm = TRUE), main = "")

# plot sorted values vs index
plot(sort(pima$diastolic), ylab = "Sorted Diastolic")

# bivariate plots.  scatterplot then parallel boxplot
plot(diabetes ~ diastolic, data = pima)
plot(diabetes ~ test, data = pima)

# the same plots using ggplot2
# create ggplot objects with necessary information about variables
library(ggplot2)
# ggpima  = ggplot(pima, aes(x = diastolic))
ggpima = ggplot(pima)

# histogram and density plot for diastolic
ggpima + geom_histogram(aes(x = diastolic))
ggpima + geom_density(aes(x = diastolic))

# scatterplot of diabetes vs diastolic
ggpima + geom_point(aes(x = diastolic, y = diabetes))

# scatterplot of diabetes vs diastolic
# different shapes for each diagnosis
# move the legend around and customize
ggpima +
  geom_point(aes(x = diastolic, y = diabetes, shape = test)) +
  theme(legend.position = "top", legend.direction = "horizontal")

# facet scatterplot by diagnosis
ggpima +
  geom_point(aes(x = diastolic, y = diabetes)) +
  facet_grid(~ test)

# the same plots using the lattice package
library(lattice)
histogram(~ diastolic, data = pima)
densityplot(~ diastolic, data = pima)
xyplot(diabetes ~ diastolic, data = pima)
xyplot(diabetes ~ diastolic, data = pima,
       groups = test, auto.key = TRUE)
xyplot(diabetes ~ diastolic | test, data = pima)
densityplot(~ diabetes, data = pima,
            groups = test, auto.key = TRUE)

# horrible sequence of commands for doing this in
# base graphics
dpos <- density(pima$diabetes[pima$test == "positive"])
dneg <- density(pima$diabetes[pima$test == "negative"])
plot(dpos, col = "blue", main = "", ylim = c(0, 2.2))
lines(dneg, col = "orange")
legend("topright", legend = c("positive", "negative"),
       col = c("blue", "orange"), lty = 1, lwd = 1)

# 1.4 More discussion of scatterplots

### Heights data
data(Heights, package = "alr4") #load data
str(Heights)

# scatterplot of data
plot(dheight ~ mheight, data = Heights,
     xlab = "mother's height (in)",
     ylab = "daughter's height (in)",
     xlim = c(55, 75), ylim = c(55, 75))

### snowfall
data("ftcollinssnow", package = "alr4")
str(ftcollinssnow)
plot(Late ~ Early, data = ftcollinssnow)
# ols line
abline(lm(Late ~ Early, data = ftcollinssnow), lty = 2)
# sample mean line
abline(mean(ftcollinssnow$Late), 0)

### turkey data
data(turkey, package = "alr4")
str(turkey)
summary(turkey) # the source factor (S) is not a factor
turkey$S = factor(turkey$S)
levels(turkey$S) <- c("control", "new source a", "new source b")
names(turkey) <- c("Dose", "Gain", "Source", "Replications", "SD") # rename variables
# create turkey data ggplot
gg_turkey <- ggplot(turkey,
                    mapping = aes(x = Dose, y = Gain,
                                  color = Source, shape = Source))
gg_turkey + geom_point() + geom_line()

# lattice version of plot
xyplot(Gain ~ Dose, data = turkey, groups = Source,
       auto.key = TRUE, type = "b")
