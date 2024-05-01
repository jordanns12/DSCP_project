library(tidyverse)
library(ggplot2)
rm(list = ls()) # initialization

# You should get the data from Madison and also from Los Angeles
# Data source link: https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00532/html

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

LosAngeles_data_18 <- LosAngeles_data_list[i]


################################### wind_speeds_visualize ##################################
visualize_wind_speed <- function(i) {
  # Madison
  data_18 <- data_list[i]
  wind_data <- data_18[[1]]$WND
  split_data <- strsplit(wind_data, ",")
  wind_df <- data.frame(matrix(unlist(split_data), nrow = length(split_data), byrow = TRUE))
  colnames(wind_df) <- c("Direction", "Direction Quality Code", "Type", "Speed", "Speed Quality Code")
  
  # mean
  wind_df$Speed %>% length()
  wind_df$Speed[wind_df$Speed == "9999"] <- 0
  wind_df$Speed <- as.numeric(wind_df$Speed)
  wind_average_speed <- sapply(seq(1, nrow(wind_df), by = 38), function(i) mean(wind_df$Speed[i:(i+23)]))
  
  # LosAngles
  LosAngeles_data_18 <- LosAngeles_data_list[i]
  
  LosAngeles_wind_data <- LosAngeles_data_18[[1]]$WND
  LosAngeles_split_data <- strsplit(LosAngeles_wind_data, ",")
  LosAngeles_wind_df <- data.frame(matrix(unlist(LosAngeles_split_data), nrow = length(LosAngeles_split_data), byrow = TRUE))
  colnames(LosAngeles_wind_df) <- c("Direction", "Direction Quality Code", "Type", "Speed", "Speed Quality Code")
  
  # mean
  LosAngeles_wind_df$Speed %>% length()
  LosAngeles_wind_df$Speed[LosAngeles_wind_df$Speed == "9999"] <- 0
  LosAngeles_wind_df$Speed <- as.numeric(LosAngeles_wind_df$Speed)
  LosAngeles_wind_average_speed <- sapply(seq(1, nrow(LosAngeles_wind_df), by = 38), function(i) mean(LosAngeles_wind_df$Speed[i:(i+23)]))
  # var
  LosAngeles_var <- var(LosAngeles_wind_average_speed%>%na.omit())
  Madison_var <- var(wind_average_speed%>%na.omit())
  # plot
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

visualize_wind_speed(5) # you can choose any number between 1-7(means 2018-2024)


###################################  atmospheric pressure ##################################

Abstract_slp <- function(df,i){ # 提取平均气压data
  data_18 <- df[i]
  # get data
  slp_data <- data_18[[1]]$SLP
  split_data <- strsplit(slp_data, ",")
  slp_df <- data.frame(matrix(unlist(split_data), nrow = length(split_data), byrow = TRUE))
  colnames(slp_df) <- c("SLP","quality status")
  # mean data
  slp_df$SLP %>% length()
  slp_df$SLP[slp_df$SLP == "9999"] <- 0
  slp_df$SLP <- as.numeric(slp_df$SLP)
  slp_average <- sapply(seq(1, nrow(slp_df), by = 38), function(i) mean(slp_df$SLP[i:(i+23)]))
  return(slp_average)
}

visualize_wind_speed <- function(i) {
  # getdata
  Madison_average_slp <- Abstract_slp(data_list,i) 
  LosAngeles_average_slp <- Abstract_slp(LosAngeles_data_list,i) 
  # var
  Madison_var <- var(Madison_average_slp%>%na.omit())
  LosAngeles_var <- var(LosAngeles_average_slp%>%na.omit())
  hist(Madison_average_slp, main = "Sea Level Pressure(SLP) Distribution", xlab = paste("Average SLP in ", i+2017), ylab = "Frequency", col = rgb(0, 1, 1, alpha = 0.4), breaks = 10, border = "white")
  hist(LosAngeles_average_slp, col = rgb(0, 0, 1, alpha = 0.4), border = "white", breaks = 10, add = TRUE)
  # legend("topright", legend = c("Madison Data", "Los Angeles Data"), fill = c(rgb(0, 1, 1, alpha = 0.3), rgb(0, 0, 1, alpha = 0.2)))
  legend("topright", 
         legend = c(paste("Madison Data -Var:", round(Madison_var, 2)), 
                    paste("Los Angeles Data -Var:", round(LosAngeles_var, 2))),
         fill = c(rgb(0, 1, 1, alpha = 0.3), rgb(0, 0, 1, alpha = 0.2)),
         text.font = 2, 
         cex = 0.7)  
}

Madison_year_slp <- list()
LosAngeles_year_slp <- list()
for (i in 1:7) {
  Madison_year_slp[i] <- Abstract_slp(data_list,i) %>%na.omit() %>% mean()
  LosAngeles_year_slp[i] <- Abstract_slp(LosAngeles_data_list,i) %>%na.omit() %>% mean()
}

Madison_year_slp
Madison_year_slp <- c(34076.98, 33618.03, 29340.01, 29438.28, 31295.07, 30267.79, 30876.58) 
# this data is from Madison_year_slp
LosAngeles_year_slp
LosAngeles_year_slp <- c(24492.89, 24116.96, 24237.06, 26833.65, 23422.17, 27393.23, 25795.61)
# this data is from LosAngeles_year_slp
# visualization
barplot(slp_matrix, beside = TRUE, col = c(rgb(0, 0.9, 1, alpha = 0.5), rgb(0, 0, 1, alpha = 0.3)), border = NA, main = "Comparison of Annual Average SLP (2018-2024)", xlab = "Year", ylab = "Average SLP", names.arg = 2018:2024)
legend("topright", legend = c("Madison", "Los Angeles"), fill = c(rgb(0, 0.9, 1, alpha = 0.3), rgb(0, 0, 1, alpha = 0.2)), bty = "n")
# atmospheric pressure done

################################## Dew temp ##################################

Abstract_temp <- function(df,i){
  data_18 <- df[i]
  temp_data <- data_18[[1]]$TMP
  split_data <- strsplit(temp_data, ",")
  temp_df <- data.frame(matrix(unlist(split_data), nrow = length(split_data), byrow = TRUE))
  colnames(temp_df) <- c("temp","quality status")
  
  temp_df$temp %>% length()
  temp_df$temp[temp_df$temp == "+9999"] <- 0
  temp_df$temp <- as.numeric(temp_df$temp)
  temp_average <- sapply(seq(1, nrow(temp_df), by = 38), function(i) mean(temp_df$temp[i:(i+23)]))
  temp_average <- temp_average/10
  return(temp_average)
}

Abstract_DEW_temp <- function(df,i){
  data_18 <- df[i]
  temp_data <- data_18[[1]]$DEW
  split_data <- strsplit(temp_data, ",")
  temp_df <- data.frame(matrix(unlist(split_data), nrow = length(split_data), byrow = TRUE))
  colnames(temp_df) <- c("temp","quality status")
  
  # mean T
  temp_df$temp %>% length()
  temp_df$temp[temp_df$temp == "+9999"] <- 0
  temp_df$temp <- as.numeric(temp_df$temp)
  temp_average <- sapply(seq(1, nrow(temp_df), by = 38), function(i) mean(temp_df$temp[i:(i+23)]))
  temp_average <- temp_average/10
  temp_average
  return(temp_average)
}

# Percentage of time sultry, oppressive or uncomfortable (i.e., dew point above 18°C)
dew_plot <- function(i){
  dew_Madison <- Abstract_DEW_temp(data_list,i)
  dew_LosAngeles <- Abstract_DEW_temp(LosAngeles_data_list,i)
  line_size = 1
  ggplot() +
    geom_line(aes(x = 1:length(dew_Madison), y = dew_Madison, color = "Madison"), size = line_size) +
    geom_line(aes(x = 1:length(dew_LosAngeles), y = dew_LosAngeles, color = "Los Angeles"), size = line_size) +
    labs(x = "Day", y = "Dew point Temperature", title = paste("Dew point Temperature Comparison in ",i+2017)) +
    scale_color_manual(values = c("Madison" = rgb(0, 1, 1, alpha = 0.7), "Los Angeles" = rgb(0, 0, 1, alpha = 0.4))) +
    geom_hline(yintercept = 17, linetype = "dashed", color =  "black", size = 0.6) +  # 添加 y=18 的直线
    annotate("text", x = 100, y = 18.1, label = "Hot, oppressive, or uncomfortable", size = 4, color =  "black") +  # 添加注释
    theme(
      plot.title = element_text(hjust = 0.5),  # 居中标题
      axis.title.x = element_text(hjust = 0.5),  # 居中 x 轴标签
      axis.title.y = element_text(hjust = 0.5),  # 居中 y 轴标签
      panel.background = element_rect(fill = "white")
    )
}

dew_plot(4) # you can choose any number between 1-7(means 2018-2024)


################################## Dew temp inference ##################################


Abstract_temp <- function(df,i){
  data_18 <- df[i]
  # 提取data
  temp_data <- data_18[[1]]$TMP
  split_data <- strsplit(temp_data, ",")
  temp_df <- data.frame(matrix(unlist(split_data), nrow = length(split_data), byrow = TRUE))
  colnames(temp_df) <- c("temp","quality status")
  
  # 计算平均温度
  temp_df$temp %>% length()
  temp_df$temp[temp_df$temp == "+9999"] <- 0
  temp_df$temp <- as.numeric(temp_df$temp)
  temp_average <- sapply(seq(1, nrow(temp_df), by = 38), function(i) mean(temp_df$temp[i:(i+23)]))
  temp_average <- temp_average/10
  return(temp_average)
}

#Abstract_temp(data_list,1)
Abstract_DEW_temp <- function(df,i){
  data_18 <- df[i]
  # 提取data
  temp_data <- data_18[[1]]$DEW
  split_data <- strsplit(temp_data, ",")
  temp_df <- data.frame(matrix(unlist(split_data), nrow = length(split_data), byrow = TRUE))
  colnames(temp_df) <- c("temp","quality status")
  
  # 计算平均温度
  temp_df$temp %>% length()
  temp_df$temp[temp_df$temp == "+9999"] <- 0
  temp_df$temp <- as.numeric(temp_df$temp)
  temp_average <- sapply(seq(1, nrow(temp_df), by = 38), function(i) mean(temp_df$temp[i:(i+23)]))
  temp_average <- temp_average/10
  temp_average
  return(temp_average)
}

getRH <- function(df,i){
  temp <- Abstract_temp(df,i) %>%na.omit()%>% as.data.frame()
  colnames(temp) <- c("tmp")
  dew_temp <- Abstract_DEW_temp(data_list,i) %>%na.omit() %>% as.data.frame()
  colnames(dew_temp) <- c("tmp")
  RH <- array()
  for (j in 1:length(temp$tmp)) {
    RH[j] <- 100 - 5*(temp$tmp[j]) + 5*(dew_temp$tmp[j])
  }
  RH <- RH%>%as.data.frame()
  colnames(RH) <- c("rh")
  return(RH)
}

dew_reference_humidity_plot <- function(i){ # i menas year
  
  rh_1 <- getRH(data_list,i)
  rh_2 <- getRH(LosAngeles_data_list,i)
  length_1 <- (rh_1$rh) %>% length()
  length_2 <- (rh_2$rh) %>% length()
  
  ggplot() +
    geom_line(data = rh_1, aes(x = 1:length_1, y = rh), color = rgb(0, 1, 1, alpha = 0.7), linetype = "solid") +
    geom_line(data = rh_2, aes(x = 1:length_2, y = rh), color = rgb(0, 0, 1, alpha = 0.4), linetype = "solid") +
    labs(x = "Day", y = "Humidity", title = "Humidity Variation Over Days") +
    scale_color_manual(values = c("Madison" = rgb(0, 1, 1, alpha = 0.7), "Los Angeles" = rgb(0, 0, 1, alpha = 0.4))) +
    theme_minimal()
}
dew_reference_humidity_plot(4)
