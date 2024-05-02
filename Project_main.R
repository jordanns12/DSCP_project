#This is R script that process the raw data, visualize, and use S-ARIMA model and multiple linear regression to analyse.
#Yifan Zhang zhang2873@wisc.edu

# read the raw data, and process it
install.packages("data.table")
library(data.table)

data_2019 <- fread("madison2019.csv")
data_2020 <- fread("madison2020.csv")
data_2021 <- fread("madison2021.csv")
data_2022 <- fread("madison2022.csv")
data_2023 <- fread("madison2023.csv")
data_2024 <- fread("madison2024.csv")
#Here I process the data 
library(dplyr)
#as.numeric(data_2024$V14[255 :291])

for (year in 2019:2024) {

  file_name <- paste0("madison", year, ".csv")
  data <- fread(file_name)

  data <- data[!data$V16 == "99999,9", ]
  data <- data[!grepl("^999,", data$V11), ]
  data <- data[!grepl("^999999,", data$V13), ]
  data <- data[!data$V14 == "+9999,9", ]
  data <- data[!data$V15 == "+9999,9", ]
  data$V14 <- as.numeric(gsub("\\+|,.*", "", data$V14)) / 10
  data$V16 <- as.numeric(gsub("\\+|,.*", "", data$V16)) / 10
  data$V15 <- as.numeric(gsub("\\+|,.*", "", data$V15)) / 10
  data$V13 <- as.numeric(sub("^([0-9]+).*", "\\1", data$V13)) / 1000
  data$V11 <- as.numeric(sub("^.*,(\\d+),.*$", "\\1", data$V11)) / 10

  data$V2 <- as.numeric(format(as.POSIXct(data$V2, format="%Y-%m-%d %H:%M:%S"), "%Y%m%d%H%M"))

  data <- data[, c(2, 11, 13, 14, 15, 16)]


  data_grouped <- split(data, substr(data$V2, 1, 8))
  data_grouped <- lapply(data_grouped, function(x) x[1,])
  data <- do.call(rbind, data_grouped)

  weighted_average <- function(df) {
    weights <- diff(c(df$V2, as.numeric(format(as.POSIXct("202401012400", format="%Y%m%d%H%M"), "%Y%m%d%H%M"))))
    weighted_mean_V11 <- weighted.mean(df$V11, weights)
    weighted_mean_V13 <- weighted.mean(df$V13, weights)
    weighted_mean_V14 <- weighted.mean(df$V14, weights)
    weighted_mean_V15 <- weighted.mean(df$V15, weights)
    weighted_mean_V16 <- weighted.mean(df$V16, weights)
    return(data.frame(V2 = df$V2[1], 
                      Weighted_Mean_V11 = weighted_mean_V11,
                      Weighted_Mean_V13 = weighted_mean_V13,
                      Weighted_Mean_V14 = weighted_mean_V14,
                      Weighted_Mean_V15 = weighted_mean_V15,
                      Weighted_Mean_V16 = weighted_mean_V16))
  }
  

  weighted_averages <- lapply(data_grouped, weighted_average)
  final_result <- do.call(rbind, weighted_averages)
  

  assign(paste0("data_", year), final_result)
}


for (year in 2019:2024) {
  data_name <- paste0("data_", year)
  data <- get(data_name)
  data[, 2:6] <- round(data[, 2:6], 2)
  setnames(data, c("DATE", "WDSP", "VISIB", "TEMP", "DEWP", "SLP"))
  data$DATE <- substr(data$DATE, 1, 8)
  assign(data_name, data)
}









#S-ARIMA we use the daily mean from 2020-2023 to predict temperature for 2024/1/1-2024/4/28
library(forecast)
#bind all the historical data
historical_data <- rbind(data_2019, data_2020, data_2021, data_2022, data_2023)
# convert the date
historical_data$DATE <- as.Date(historical_data$DATE, format='%Y/%m/%d')
# choose the tempreture
historical_temp <- historical_data$TEMP
ts_data <- ts(historical_temp, frequency = 365)
# using auto.arima to fit the ARIMA model
fit <- auto.arima(ts_data, seasonal=TRUE, trace=TRUE)
# make acf plot
Acf(fit$residuals, col = "skyblue")
# qq-plot
qqnorm(fit$residuals,col = "violet")
qqline(fit$residuals, col = "skyblue",lwd=2.0)
# prediction
predictions <- forecast(fit, h=119)
historical_start_date <- as.Date("2019-01-01")
predicted_start_date <- as.Date("2024-01-01")
predicted_dates <- as.Date(time(predictions), origin = predicted_start_date)
# get the actual data of 2024
actual_data <- data_2024
actual_data$DATE <- as.Date(actual_data$DATE, format='%Y/%m/%d')
dates <- seq(as.Date("2019-01-01"), as.Date("2024-04-28"), by = "day")
c<-actual_data$TEMP
a <- c(historical_temp,c)
b <- c(historical_temp,predictions$mean ) 
length(a)
length(b)
plot(dates, b, type = "l", col = "violet",lwd = 2.0, xlab = "Date", ylab = "Temperature")
lines(dates, a, col = col <- "skyblue",lwd = 2.0)
legend("topright", legend = c("Predictions","Actual Data"), col = c("violet", "skyblue"), lty = 1)
a <- c(c)
b <- c(predictions$mean ) 
length(a)
length(b)
dates <- seq(as.Date("2024-01-01"), as.Date("2024-04-28"), by = "day")
plot(dates, b, type = "l", col = "violet",lwd = 2.0, xlab = "Date", ylab = "Temperature")
lines(dates, a, col = col <- "skyblue",lwd = 2.0)
legend("topright", legend = c("Predictions","Actual Data"), col = c("violet", "skyblue"), lty = 1)
r_squared <- 1 - sum((c - predictions$mean)^2) / sum((c - mean(c))^2)
print(paste("R^2:", r_squared))


# try to use randomForest for predicting 
#install.packages("randomForest")
#library(randomForest)
#train_temp <- historical_data$TEMP
#rf_model <- randomForest(train_temp ~ ., data = historical_data)
#print(rf_model)
#predictions <- predict(rf_model, actual_data)
#plot(predictions, type = "l")
#predicted_temp <- predict(rf_model, n.ahead = 112)
#predicted_temp <- predicted_temp[1:112]

#predicted_dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 112)

#plot(actual_data$DATE, actual_data$TEMP, type = "l", col = "red", xlab = "Date", ylab = "Temperature", ylim = range(c(actual_data$TEMP, predicted_temp)))
#lines(predicted_dates, predicted_temp, col = "blue")
#legend("topright", legend = c("Actual Data", "Predictions"), col = c("red", "blue"), lty = 1)

#actual_values <- actual_data$TEMP
#r_squared <- 1 - sum((actual_values - predicted_temp)^2) / sum((actual_values - mean(actual_values))^2)
#print(paste("R^2:", r_squared))
#mape <- mean(abs((actual_values - predicted_temp) / actual_values)) * 100
#print(paste("MAPE:", mape))





# Define the colors for each year
#colors <- c("2020" = "red", "2021" = "blue", "2022" = "green", "2023" = "orange")

# Create a new plot
#plot(1:12, historical_data$TEMP[1:12], type = "l", col = colors["2020"], xlab = "Month", ylab = "Temperature", xlim = c(1, 12), ylim = range(historical_data$TEMP), lwd = 2)

# Add lines for each year
#for (year in unique(substr(historical_data$DATE, 1, 4))) {
#  lines(1:12, historical_data$TEMP[substr(historical_data$DATE, 1, 4) == year][1:12], col = colors[year], lwd = 2)
#}

# Add legend
#legend("topright", legend = unique(substr(historical_data$DATE, 1, 4)), col = unique(colors), lty = 1, lwd = 2)

# Add title
#title("Temperature Comparison for 2020-2023")



###Multiple Linear Regression: we choose the daily mean data from 2020-2023,to see temperature is correlated with which factors
data <-  rbind(data_2019,data_2020, data_2021, data_2022, data_2023)
data$DATE <- as.Date(as.character(data$DATE), format = "%Y%m%d")
data$DATE <- as.Date(data$DATE, format = "%Y/%m/%d")
combined_data <- data.frame()
for (year in 2019:2023) {
  year_data <- data[data$DATE >= as.Date(paste(year, "-01-01", sep = "")) & 
                      data$DATE <= as.Date(paste(year, "-04-30", sep = "")) |
                      data$DATE >= as.Date(paste(year, "-10-01", sep = "")) &
                      data$DATE <= as.Date(paste(year, "-12-31", sep = "")), ]
  

  combined_data <- rbind(combined_data, year_data)
}

combined_data <- combined_data[order(combined_data$DATE), ]

model <- lm(TEMP ~ WDSP + DEWP  +SLP + VISIB, data = combined_data)

summary(model)
plot(model, which = 1, col="skyblue")
predicted <- predict(model, combined_data)
plot(combined_data$TEMP, predicted, main = "Predicted vs Actual Temperature", xlab = "Actual Temperature", ylab = "Predicted Temperature", col = "skyblue")
abline(0, 1, col = "violet",lwd=2.0) 
r_squared <- summary(model)$r.squared
print(paste("R-squared:", round(r_squared, 3)))
plot(combined_data$WDSP, combined_data$TEMP, main = "Temperature vs Wind Speed", xlab = "Wind Speed", ylab = "Temperature", col = "skyblue")
abline(lm(TEMP ~ WDSP, data = combined_data), col = "violet",lwd=2.0)  
plot(combined_data$VISIB, combined_data$TEMP, main = "Temperature vs Visibility", xlab = "SLP", ylab = "Temperature", col = "skyblue")
abline(lm(TEMP ~ VISIB, data = combined_data), col = "violet",lwd=2.0)  


plot(combined_data$DEWP, combined_data$TEMP, main = "Temperature vs Dew Point Temperature", xlab = "Dew Point Temperature", ylab = "Temperature", col = "skyblue")
abline(lm(TEMP ~ DEWP, data = combined_data), col = "violet",lwd=2.0)  

plot(combined_data$SLP, combined_data$TEMP, main = "Temperature vs SLP", xlab = "SLP", ylab = "Temperature", col = "skyblue")
abline(lm(TEMP ~ SLP, data = combined_data), col = "violet",lwd=2.0)  


###some visualization plots
#library(ggplot2)
#visib_data <- data_2021$VISIB
#data <- data.frame(VISIB = visib_data)
#data_2021$DATE <- as.Date(data_2021$DATE, format='%Y/%m/%d')


 

#ggplot(data_2021, aes(y = VISIB)) +
##  geom_boxplot(fill = "blue", color = "skyblue")+
 # labs(title = "Boxplot of VISIBILITY in 2021",
 #      x = "",
#       y = "Visibility") +
#  theme_minimal()
#ggplot(data_2021, aes(y = WDSP)) +
 # geom_boxplot(fill = "lightblue", color = "purple")+
 # labs(title = "Boxplot of WIND SPEED in 2021",
#       x = "",
#       y = "Visibility") +
#  theme_minimal()


#ggplot(data_2021, aes(y = TEMP)) +
 # geom_boxplot(fill = "lavender", color = "purple") +
#  labs(title = "Boxplot of Temp in 2021",
 #      x = "",
 #      y = "Temperature") +
#  theme_minimal()

#ggplot(data_2021, aes(y = DEWP)) +
#  geom_boxplot(fill = "lavender", color = "purple") +
##  labs(title = "Boxplot of DEWP in 2021",
 #      x = "",
#       y = "Dew point temperature
#") +
#  theme_minimal()


