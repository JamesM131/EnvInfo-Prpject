library(tidyverse)
library(here)
library(forecast)
library(lmtest)
fires_a <- read_rds(here::here("data", "fires_clean_all.rds"))
View(fires_x)



MA <- NULL
CMA <- NULL

for(i in 1:238){
  MA[i+1] <- (fires_a$fires[i] + fires_a$fires[i+1]
              + fires_a$fires[i+2])/3
}

for(i in 1:237){
  CMA[i+2] <- (MA[i+1] + MA[i+2])/2
}



n_alt <- NULL

for(i in 3:238){
  n_alt[i] <- fires_a$fires[i]/CMA[i]
}

n_alt[239] <- NA
n_alt[which(is.nan(n_alt))] <- 0

month <- NULL
for(i in 1:239){
  month[i] <- i%%12
}

fires_b <- as.data.frame(cbind(fires_a, month, MA, CMA, n_alt))

winter <- subset(fires_b,
                 fires_b$month == 12 |
                   fires_b$month == 1 |
                   fires_b$month == 2)
spring <- subset(fires_b,
                 fires_b$month == 3 |
                   fires_b$month == 4 |
                   fires_b$month == 5)
summer <- subset(fires_b,
                 fires_b$month == 6 |
                   fires_b$month == 7 |
                   fires_b$month == 8)
autumn <- subset(fires_b,
                 fires_b$month == 9 |
                   fires_b$month == 10 |
                   fires_b$month == 11)

m_alt_winter <- mean(winter$n_alt, na.rm = TRUE)
m_alt_spring <- mean(spring$n_alt, na.rm = TRUE)
m_alt_summer <- mean(summer$n_alt, na.rm = TRUE)
m_alt_autumn <- mean(autumn$n_alt, na.rm = TRUE)

m_alt <- c(m_alt_winter, m_alt_spring, m_alt_summer, m_alt_autumn)
m_alt_sum <- sum(m_alt)

for(i in 1:4){
  m_alt[i] <- m_alt[i]*4/m_alt_sum
}

for(i in 1:length(winter$fires)){
  winter[i,7] <- winter$fires[i]/m_alt[1]
}

for(i in 1:length(spring$fires)){
  spring[i,7] <- spring$fires[i]/m_alt[2]
}

for(i in 1:length(summer$fires)){
  summer[i,7] <- summer$fires[i]/m_alt[3]
}

for(i in 1:length(autumn$fires)){
  autumn[i,7] <- autumn$fires[i]/m_alt[4]
}


fires_des <- as.data.frame(rbind(winter,spring,summer,autumn))
fires_des$index <- as.numeric(row.names(fires_des))

fires_des %>%
  count(date, wt = V7) %>%
  ggplot(aes(x = date, y = n)) +
  geom_point()

# linear model
fit1 <- lm(fires_des$fires ~ fires_des$index)
summary(fit1) # significant
fitA <- fit1
plot(fires_des$index, fires_des$fires)

# ARIMA models
library(lmtest)
acf(fires_des$fires, lag.max = 50) # outside conf int until 50
pacf(fires_des$fires) # spikes at 2,3,6

arima101 <- arima(fires_des$fires, order = c(1,0,1))
coeftest(arima101) # significant
arima202 <- arima(fires_des$fires, order = c(2,0,2))
coeftest(arima202) # inasignificant
# try differencing
arima111 <- arima(fires_des$fires, order = c(1,1,1))
coeftest(arima111) # significant
acf(arima202$residuals) # needs more ma
pacf(arima202$residuals) # needs more ar

arima212 <- arima(fires_des$fires, order = c(2,1,2))
coeftest(arima212) # significant
acf(arima212$residuals) # needs more ma
pacf(arima212$residuals) # perfect

arima213 <- arima(fires_des$fires, order = c(2,1,3))
coeftest(arima213) # ma1 term insignificant. Other terms super significant.
acf(arima213$residuals) # perfect
pacf(arima213$residuals) # perfect
fit_B <- arima213

# forecasting December 2017
View(fires_des)
prediction_A <- (1700.243 + 3.371*240)*m_alt[1]
prediction_A # 2109.442
prediction_B <- forecast(fit_B, h = 1)
prediction_B # 1915.295
