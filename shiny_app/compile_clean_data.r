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
as.numeric(data_2024$V14[255 :291])

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

capture.output(data_2019, file = "clean2019.csv")
capture.output(data_2020, file = "clean2020.csv")
capture.output(data_2021, file = "clean2021.csv")
capture.output(data_2022, file = "clean2022.csv")
capture.output(data_2023, file = "clean2023.csv")
capture.output(data_2024, file = "clean2024.csv")