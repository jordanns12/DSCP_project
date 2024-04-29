library(tidyverse)
library(ggplot2)
rm(list = ls()) # initialization

LosAngeles_path <- "/Users/lixiangyi/Documents/学习/大四（VISP）下/STAT 605/Project/LosAngeles_data"
LosAngeles_list <- list.files(path = LosAngeles_path, pattern = ".csv$", full.names = TRUE)
LosAngeles_data_list <- lapply(LosAngeles_list, read.csv)

LosAngeles_data <- list()

for (i in 1:7) {
  LosAngeles_data[[i]] <- LosAngeles_data_list[i]
}

i=1

LosAngeles_data_18 <- LosAngeles_data_list[i]

# 提取风速data
LosAngeles_wind_data <- LosAngeles_data_18[[1]]$WND
LosAngeles_split_data <- strsplit(LosAngeles_wind_data, ",")
LosAngeles_wind_df <- data.frame(matrix(unlist(LosAngeles_split_data), nrow = length(LosAngeles_split_data), byrow = TRUE))
colnames(LosAngeles_wind_df) <- c("Direction", "Direction Quality Code", "Type", "Speed", "Speed Quality Code")
print(LosAngeles_wind_df)

# 计算平均风速
LosAngeles_wind_df$Speed %>% length()
LosAngeles_wind_df$Speed[LosAngeles_wind_df$Speed == "9999"] <- 0
LosAngeles_wind_df$Speed <- as.numeric(LosAngeles_wind_df$Speed)
LosAngeles_wind_average_speed <- sapply(seq(1, nrow(LosAngeles_wind_df), by = 38), function(i) mean(LosAngeles_wind_df$Speed[i:(i+23)]))
LosAngeles_wind_average_speed

# 可视化
# 绘制第一个直方图
hist(wind_average_speed, main = "Wind Speed Distribution", xlab = "Average Wind Speed in 2018", ylab = "Frequency", col = rgb(0, 1, 1, alpha = 0.3), border = "white")

# 添加第二个直方图
hist(LosAngeles_wind_average_speed, col = rgb(0, 0, 1, alpha = 0.2), border = "white", add = TRUE)

# 添加图例
legend("topright", legend = c("Madison Data", "Los Angeles Data"), fill = c(rgb(0, 1, 1, alpha = 0.3), rgb(0, 0, 1, alpha = 0.2)))


visualize_wind_speed <- function(i) {
  # Madison
  data_18 <- data_list[i]
  # 提取风速data
  wind_data <- data_18[[1]]$WND
  split_data <- strsplit(wind_data, ",")
  wind_df <- data.frame(matrix(unlist(split_data), nrow = length(split_data), byrow = TRUE))
  colnames(wind_df) <- c("Direction", "Direction Quality Code", "Type", "Speed", "Speed Quality Code")
  
  # 计算平均风速
  wind_df$Speed %>% length()
  wind_df$Speed[wind_df$Speed == "9999"] <- 0
  wind_df$Speed <- as.numeric(wind_df$Speed)
  wind_average_speed <- sapply(seq(1, nrow(wind_df), by = 38), function(i) mean(wind_df$Speed[i:(i+23)]))
  
  # LosAngles
  LosAngeles_data_18 <- LosAngeles_data_list[i]
  
  # 提取风速data
  LosAngeles_wind_data <- LosAngeles_data_18[[1]]$WND
  LosAngeles_split_data <- strsplit(LosAngeles_wind_data, ",")
  LosAngeles_wind_df <- data.frame(matrix(unlist(LosAngeles_split_data), nrow = length(LosAngeles_split_data), byrow = TRUE))
  colnames(LosAngeles_wind_df) <- c("Direction", "Direction Quality Code", "Type", "Speed", "Speed Quality Code")
  
  # 计算平均风速
  LosAngeles_wind_df$Speed %>% length()
  LosAngeles_wind_df$Speed[LosAngeles_wind_df$Speed == "9999"] <- 0
  LosAngeles_wind_df$Speed <- as.numeric(LosAngeles_wind_df$Speed)
  LosAngeles_wind_average_speed <- sapply(seq(1, nrow(LosAngeles_wind_df), by = 38), function(i) mean(LosAngeles_wind_df$Speed[i:(i+23)]))
  # 计算方差
  LosAngeles_var <- var(LosAngeles_wind_average_speed%>%na.omit())
  Madison_var <- var(wind_average_speed%>%na.omit())
  # 可视化
  hist(wind_average_speed, main = "Wind Speed Distribution", xlab = paste("Average Wind Speed in ", i+2017), ylab = "Frequency", col = rgb(0, 1, 1, alpha = 0.3), border = "white")
  hist(LosAngeles_wind_average_speed, col = rgb(0, 0, 1, alpha = 0.2), border = "white", add = TRUE)
  # legend("topright", legend = c("Madison Data", "Los Angeles Data"), fill = c(rgb(0, 1, 1, alpha = 0.3), rgb(0, 0, 1, alpha = 0.2)))
  legend("topright", 
         legend = c(paste("Madison Data -Var:", round(Madison_var, 2)), 
                    paste("Los Angeles Data -Var:", round(LosAngeles_var, 2))),
         fill = c(rgb(0, 1, 1, alpha = 0.3), rgb(0, 0, 1, alpha = 0.2)),
         text.font = 2, # 使用 text.font 参数来设置字体 
         cex = 0.7)  
}

visualize_wind_speed(7)

LosAngeles_var <- var(LosAngeles_wind_average_speed%>%na.omit())
Madison_var <- var(wind_average_speed%>%na.omit())




