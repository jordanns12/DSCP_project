library(tidyverse)
library(ggplot2)
rm(list = ls()) # initialization

setwd("/Users/lixiangyi/Documents/学习/大四（VISP）下/STAT 605/Project")
folder_path <- "/Users/lixiangyi/Documents/学习/大四（VISP）下/STAT 605/Project/Madison_data"
file_list <- list.files(path = folder_path, pattern = ".csv$", full.names = TRUE)
data_list <- lapply(file_list, read.csv)


i=1
# Wind
data_18 <- data_list[i]
data_18[[1]]$WND[1]

# 提取风速data
wind_data <- data_18[[1]]$WND
split_data <- strsplit(wind_data, ",")
wind_df <- data.frame(matrix(unlist(split_data), nrow = length(split_data), byrow = TRUE))
colnames(wind_df) <- c("Direction", "Direction Quality Code", "Type", "Speed", "Speed Quality Code")
print(wind_df)

# 计算平均风速
wind_df$Speed %>% length()
wind_df$Speed[wind_df$Speed == "9999"] <- 0
wind_df$Speed <- as.numeric(wind_df$Speed)
wind_average_speed <- sapply(seq(1, nrow(wind_df), by = 38), function(i) mean(wind_df$Speed[i:(i+23)]))
wind_average_speed

# 可视化
# 绘制直方图
hist(wind_average_speed, main = "Wind Speed Distribution", xlab = "Average Wind Speed", ylab = "Frequency", col = "skyblue", border = "white")




