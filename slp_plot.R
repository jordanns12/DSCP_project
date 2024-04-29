library(tidyverse)
library(ggplot2)
rm(list = ls()) # initialization


setwd("/Users/lixiangyi/Documents/学习/大四（VISP）下/STAT 605/Project")
folder_path <- "/Users/lixiangyi/Documents/学习/大四（VISP）下/STAT 605/Project/Madison_data"

file_list <- list.files(path = folder_path, pattern = ".csv$", full.names = TRUE)
data_list <- lapply(file_list, read.csv)

LosAngeles_path <- "/Users/lixiangyi/Documents/学习/大四（VISP）下/STAT 605/Project/LosAngeles_data"
LosAngeles_list <- list.files(path = LosAngeles_path, pattern = ".csv$", full.names = TRUE)
LosAngeles_data_list <- lapply(LosAngeles_list, read.csv)

LosAngeles_data <- list()

for (i in 1:7) {
  LosAngeles_data[[i]] <- LosAngeles_data_list[i]
}

i=1

Abstract_slp <- function(df,i){ # 提取平均气压data
  data_18 <- df[i]
  # 提取气压data
  slp_data <- data_18[[1]]$SLP
  split_data <- strsplit(slp_data, ",")
  slp_df <- data.frame(matrix(unlist(split_data), nrow = length(split_data), byrow = TRUE))
  colnames(slp_df) <- c("SLP","quality status")
  
  # 计算平均气压
  slp_df$SLP %>% length()
  slp_df$SLP[slp_df$SLP == "9999"] <- 0
  slp_df$SLP <- as.numeric(slp_df$SLP)
  slp_average <- sapply(seq(1, nrow(slp_df), by = 38), function(i) mean(slp_df$SLP[i:(i+23)]))
  return(slp_average)
}

Abstract_slp(data_list,1) %>%na.omit() %>% mean()
Abstract_slp(LosAngeles_data_list,1) %>%na.omit() %>% mean()

visualize_wind_speed <- function(i) {
  # 提取气压data
  Madison_average_slp <- Abstract_slp(data_list,i) 
  LosAngeles_average_slp <- Abstract_slp(LosAngeles_data_list,i) 
  # 计算方差
  Madison_var <- var(Madison_average_slp%>%na.omit())
  LosAngeles_var <- var(LosAngeles_average_slp%>%na.omit())
  # 可视化
  hist(Madison_average_slp, main = "Sea Level Pressure(SLP) Distribution", xlab = paste("Average SLP in ", i+2017), ylab = "Frequency", col = rgb(0, 1, 1, alpha = 0.4), breaks = 10, border = "white")
  hist(LosAngeles_average_slp, col = rgb(0, 0, 1, alpha = 0.4), border = "white", breaks = 10, add = TRUE)
  # legend("topright", legend = c("Madison Data", "Los Angeles Data"), fill = c(rgb(0, 1, 1, alpha = 0.3), rgb(0, 0, 1, alpha = 0.2)))
  legend("topright", 
         legend = c(paste("Madison Data -Var:", round(Madison_var, 2)), 
                    paste("Los Angeles Data -Var:", round(LosAngeles_var, 2))),
         fill = c(rgb(0, 1, 1, alpha = 0.3), rgb(0, 0, 1, alpha = 0.2)),
         text.font = 2, # 使用 text.font 参数来设置字体 
         cex = 0.7)  
}

visualize_wind_speed(3)

Madison_year_slp <- list()
LosAngeles_year_slp <- list()
for (i in 1:7) {
  Madison_year_slp[i] <- Abstract_slp(data_list,i) %>%na.omit() %>% mean()
  LosAngeles_year_slp[i] <- Abstract_slp(LosAngeles_data_list,i) %>%na.omit() %>% mean()
}
LosAngeles_year_slp
Madison_year_slp <- c(34076.98, 33618.03, 29340.01, 29438.28, 31295.07, 30267.79, 30876.58)
LosAngeles_year_slp <- c(24492.89, 24116.96, 24237.06, 26833.65, 23422.17, 27393.23, 25795.61)

# 创建直方图
barplot(Madison_year_slp, names.arg = 2018:2024, col = "skyblue", main = "Madison Annual Average SLP (2018-2024)", xlab = "Year", ylab = "Average SLP")
barplot(LosAngeles_year_slp, names.arg = 2018:2024, col = "lightgreen", main = "Los Angeles Annual Average SLP (2018-2024)", xlab = "Year", ylab = "Average SLP")
# 创建一个包含 Madison 和 Los Angeles 年均 SLP 数据的矩阵
slp_matrix <- matrix(c(Madison_year_slp, LosAngeles_year_slp), nrow = 2, byrow = TRUE)

# 创建直方图
barplot(slp_matrix, beside = TRUE, col = c("skyblue", "lightgreen"), main = "Comparison of Annual Average SLP (2018-2024)", xlab = "Year", ylab = "Average SLP", names.arg = 2018:2024)
legend("topright", legend = c("Madison", "Los Angeles"), fill = c("skyblue", "lightgreen"), bty = "n")


# 创建直方图
barplot(slp_matrix, beside = TRUE, col = c(rgb(0, 0.9, 1, alpha = 0.5), rgb(0, 0, 1, alpha = 0.3)), border = NA, main = "Comparison of Annual Average SLP (2018-2024)", xlab = "Year", ylab = "Average SLP", names.arg = 2018:2024)
legend("topright", legend = c("Madison", "Los Angeles"), fill = c(rgb(0, 0.9, 1, alpha = 0.3), rgb(0, 0, 1, alpha = 0.2)), bty = "n")

