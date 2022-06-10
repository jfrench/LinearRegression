#library(owidR) # Our World in Data
library(wbstats) # World Bank Data

#owid_search("economic inequality")
#wealth <- owid("gdp-per-capita-vs-economic-inequality")

# Load most recent literacy rate
edu <- wb_data(
  country = "countries_only",
  indicator = "SE.ADT.LITR.ZS",  # Literacy rate  (% of pop above 15)
  mrnev = 1 #returns most recent non-empty value
)
edu <- edu[,c(3,5)]
names(edu) <- c("country", "literacy_rate")


# Life Expectancy - SP.DYN.LE00.IN (years)
health <- wb_data(
  country = "countries_only", indicator = "SP.DYN.LE00.IN", mrnev = 1)
health <- health[,c(3,5)]
names(health) <- c("country", "life_exp")

# Land Area - AG.LND.TOTL.K2 (sq.km)
area <- wb_data(
  country = "countries_only", indicator = "AG.LND.TOTL.K2", mrnev = 1)
area <- area[,c(3,5)]
names(area) <- c("country", "land_area")


# GDP per cap (ppp) - NY.GDP.PCAP.PP.CD
wealth <- wb_data(
  country = "countries_only",  indicator =  "NY.GDP.PCAP.PP.CD",mrnev = 1)
wealth <- wealth[,c(3,5)]
names(wealth) <- c("country", "gdp_per_cap")


country <- merge(edu, merge(health, merge(area, wealth)))
head(country)

#mydata2 <- country[,c(2,5)]
#mydata2 <- mydata2[c(2,1)]

summary(country[c(2:5)])
cor(country[c(2:5)])
pairs(country[c(2:5)])

plot(density(country$literacy_rate))


plot(gdp_per_cap ~ literacy_rate, data = country,
     pch = 20,
     col=ifelse(country$literacy_rate > 94.50318 & country$literacy_rate < 94.5032, "red", "sea green"),
     main = "Scatterplot of Education vs Wealth",
     xlab = "Literacy Rate (%)",
     ylab = "GDP per Capita")
abline(lm(country))
