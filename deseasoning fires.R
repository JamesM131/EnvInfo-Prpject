fires_a <- fires_tsibble[fires_tsibble$state == "Acre",]
plot.ts(fires_a$date_month, fires_a$number)

MA <- c()
CMA <- c()

for(i in 1:239-3){
  MA[i+1] <- (fires_a$number[i] + fires_a$number[i+1]
              + fires_a$number[i+2] + fires_a$number[i+3])/4
}

for(i in 1:(239)-4){
  CMA[i+2] <- (MA[i+1] + MA[i+2])/2
}

MA[238] <- NA
MA[239] <- NA
CMA[238] <- NA
CMA[239] <- NA


n_alt <- c()

for(i in 3:(239)-1){
  n_alt[i] <- fires_a$number[i]/CMA[i]
}
n_alt[239] <- NA

fires_b <- as.data.frame(cbind(fires_a, MA, CMA, n_alt))

winter <- subset(fires_b,
                 fires_b$month_clean == "December" | fires_b$month_clean == "January" | fires_b$month_clean == "February")
spring <- subset(fires_b,
                 fires_b$month_clean == "March" | fires_b$month_clean == "April" | fires_b$month_clean == "May")
summer <- subset(fires_b,
                 fires_b$month_clean == "June" | fires_b$month_clean == "July" | fires_b$month_clean == "August")
autumn <- subset(fires_b,
                 fires_b$month_clean == "September" | fires_b$month_clean == "October" | fires_b$month_clean == "November")

m_alt_winter <- mean(winter$n_alt, na.rm = TRUE)
m_alt_spring <- mean(spring$n_alt, na.rm = TRUE)
m_alt_summer <- mean(summer$n_alt, na.rm = TRUE)
m_alt_autumn <- mean(autumn$n_alt, na.rm = TRUE)

m_alt <- c(m_alt_winter, m_alt_spring, m_alt_summer, m_alt_autumn)

for(i in 1:4){
  m_alt[i] <- m_alt[i]*4/2.837892
}
sum(m_alt)

for(i in 1:length(winter$number)){
  winter[i,12] <- winter$number[i]/m_alt[1]
}

for(i in 1:length(spring$number)){
  spring[i,12] <- spring$number[i]/m_alt[2]
}

for(i in 1:length(summer$number)){
  summer[i,12] <- summer$number[i]/m_alt[3]
}

for(i in 1:length(autumn$number)){
  autumn[i,12] <- autumn$number[i]/m_alt[4]
}

data1 <- as.data.frame(rbind(winter,spring,summer,autumn))
data1$index <- as.numeric(row.names(data1))
View(data1)

data1 %>%
  count(date, wt = V12) %>%
  ggplot(aes(x = date, y = n)) +
  geom_point()
