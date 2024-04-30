# read historical_data
install.packages("data.table")
library(data.table)
data_2018 <- fread("2018.csv")
data_2019 <- fread("2019.csv")
data_2020 <- fread("2020.csv")
data_2021 <- fread("2021.csv")
data_2022 <- fread("2022.csv")
data_2023 <- fread("2023.csv")
data_2024 <- fread("2024.csv")
library(forecast)
#bind all the historical data
historical_data <- rbind(data_2020, data_2021, data_2022, data_2023)
# convert the date
historical_data$DATE <- as.Date(historical_data$DATE, format='%Y/%m/%d')
# choose the tempreture
historical_temp <- historical_data$TEMP
ts_data <- ts(historical_temp, frequency = 365)
# using auto.arima to fit the ARIMA model
fit <- auto.arima(ts_data, seasonal=TRUE, trace=TRUE)

# make acf plot
Acf(fit$residuals)
# qq-plot
qqnorm(fit$residuals)
qqline(fit$residuals)
# prediction
predictions <- forecast(fit, h=112)


historical_start_date <- as.Date("2020-01-01")
predicted_start_date <- as.Date("2024-01-01")
predicted_dates <- as.Date(time(predictions), origin = predicted_start_date)



# get the actual data of 2024
actual_data <- data_2024
actual_data$DATE <- as.Date(actual_data$DATE, format='%Y/%m/%d')
dates <- seq(as.Date("2024-01-01"), as.Date("2024-04-21"), by = "day")
c<-actual_data$TEMP
a <- c(c)
b <- c(predictions$mean ) # 去除预测结果的起始点，使其与实际数据对齐
length(a)
length(b)
plot(dates, b, type = "l", col = "red",lwd = 2.0, xlab = "Date", ylab = "Temperature")
lines(dates, a, col = col <- "blue",lwd = 2.0)
legend("topright", legend = c("Predictions","Actual Data"), col = c("red", "blue"), lty = 1)

mape <- mean(abs((predictions$mean -c) /c)) * 100
print(paste("MAPE:", mape))





# try to use randomForest for predicting 
install.packages("randomForest")
library(randomForest)
train_temp <- historical_data$TEMP
rf_model <- randomForest(train_temp ~ ., data = historical_data)
print(rf_model)
predictions <- predict(rf_model, actual_data)
plot(predictions, type = "l")
predicted_temp <- predict(rf_model, n.ahead = 112)
predicted_temp <- predicted_temp[1:112]

predicted_dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 112)

plot(actual_data$DATE, actual_data$TEMP, type = "l", col = "red", xlab = "Date", ylab = "Temperature", ylim = range(c(actual_data$TEMP, predicted_temp)))
lines(predicted_dates, predicted_temp, col = "blue")
legend("topright", legend = c("Actual Data", "Predictions"), col = c("red", "blue"), lty = 1)
l
actual_values <- actual_data$TEMP
r_squared <- 1 - sum((actual_values - predicted_temp)^2) / sum((actual_values - mean(actual_values))^2)
print(paste("R^2:", r_squared))
mape <- mean(abs((actual_values - predicted_temp) / actual_values)) * 100
print(paste("MAPE:", mape))





# Define the colors for each year
colors <- c("2020" = "red", "2021" = "blue", "2022" = "green", "2023" = "orange")

# Create a new plot
plot(1:12, historical_data$TEMP[1:12], type = "l", col = colors["2020"], xlab = "Month", ylab = "Temperature", xlim = c(1, 12), ylim = range(historical_data$TEMP), lwd = 2)

# Add lines for each year
for (year in unique(substr(historical_data$DATE, 1, 4))) {
  lines(1:12, historical_data$TEMP[substr(historical_data$DATE, 1, 4) == year][1:12], col = colors[year], lwd = 2)
}

# Add legend
legend("topright", legend = unique(substr(historical_data$DATE, 1, 4)), col = unique(colors), lty = 1, lwd = 2)

# Add title
title("Temperature Comparison for 2020-2023")



###MLR
data <-  rbind(data_2020, data_2021, data_2022, data_2023)
data$DATE <- as.Date(data$DATE, format = "%Y/%m/%d")
combined_data <- data.frame()
for (year in 2020:2023) {
  year_data <- data[data$DATE >= as.Date(paste(year, "-01-01", sep = "")) & 
                      data$DATE <= as.Date(paste(year, "-04-30", sep = "")) |
                      data$DATE >= as.Date(paste(year, "-10-01", sep = "")) &
                      data$DATE <= as.Date(paste(year, "-12-31", sep = "")), ]
  

  combined_data <- rbind(combined_data, year_data)
}

combined_data <- combined_data[order(combined_data$DATE), ]

model <- lm(TEMP ~ WDSP + DEWP +PRCP +SLP+VISIB, data = combined_data)

summary(model)

plot(model, which = 1, col="blue")

predicted <- predict(model, combined_data)


plot(combined_data$TEMP, predicted, main = "Predicted vs Actual Temperature", xlab = "Actual Temperature", ylab = "Predicted Temperature", col = "skyblue")
abline(0, 1, col = "red",lwd=1.5) 


r_squared <- summary(model)$r.squared
print(paste("R-squared:", round(r_squared, 3)))


plot(combined_data$WDSP, combined_data$TEMP, main = "Temperature vs Wind Speed", xlab = "Wind Speed", ylab = "Temperature", col = "skyblue")
abline(lm(TEMP ~ WDSP, data = combined_data), col = "violet",lwd=1.5)  


plot(combined_data$DEWP, combined_data$TEMP, main = "Temperature vs Dew Point Temperature", xlab = "Dew Point Temperature", ylab = "Temperature", col = "skyblue")
abline(lm(TEMP ~ DEWP, data = combined_data), col = "violet",lwd=1.5)  

plot(combined_data$SLP, combined_data$TEMP, main = "Temperature vs SLP", xlab = "SLP", ylab = "Temperature", col = "skyblue")
abline(lm(TEMP ~ SLP, data = combined_data), col = "violet",lwd=1.5)  # 添加线性回归线








library(ggplot2)

visib_data <- data_2021$VISIB


data <- data.frame(VISIB = visib_data)
library(ggplot2)


data_2021 <- data_2021[, c("TEMP", "VISIB", "DEWP", "SLP", "WDSP", "PRCP","DATE")]
data_2021$DATE <- as.Date(data_2021$DATE, format='%Y/%m/%d')



library(ggplot2)
data_2021$DATE <- as.Date(data_2021$DATE, format='%Y/%m/%d')

ggplot(data_2021, aes(x = DATE)) +
  geom_line(aes(y = TEMP),col="skyblue", size=1.0) +

  labs(title = "Variation of Meteorological Variables in 2021",
       x = "Date",
       y = "Value") +

  theme_minimal()


ggplot(data_2021, aes(x = DATE)) +
  geom_line(aes(y = TEMP), col = "skyblue", size = 1.5) +
  labs(title = "Variation of Temperature in 2021",
       x = "Date",
       y = "Temperature") +
  scale_x_date(date_format = "%b %d") +  
  theme_minimal()

ggplot(data_2021, aes(x = DATE)) +
  geom_line(aes(y = TEMP), col = "skyblue", size = 1.5) +
  labs(title = "Variation of Temperature in 2021",
       x = "Date",
       y = "Temperature") +
  scale_x_date(labels = scales::date_format("%b %d")) + 
  theme_minimal()

ggplot(data_2021, aes(x = DATE)) +
  geom_line(aes(y = VISIB), col = "lavender", size = 1.5) +
  labs(title = "Variation of Temperature in 2021",
       x = "Date",
       y = "Visibility") +
  scale_x_date(date_labels = "%m/%d") + 
  theme_minimal()


 

ggplot(data_2021, aes(y = VISIB)) +
  geom_boxplot(fill = rgb(0, 0.9, 1, alpha = 0.3),color= rgb(0, 0, 1, alpha = 0.3))+
  labs(title = "Boxplot of Wind Speed in 2021",
       x = "",
       y = "Visibility") +
  theme_minimal()

ggplot(data_2021, aes(y = TEMP)) +
  geom_boxplot(fill = "lavender", color = "purple") +
  labs(title = "Boxplot of Precipitation in 2021",
       x = "",
       y = "Temperature") +
  theme_minimal()




