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
    print(temp$tmp[j])
  }
  RH <- RH%>%as.data.frame()
  colnames(RH) <- c("rh")
  return(RH)
}

rh_1 <- getRH(data_list,1)
rh_2 <- getRH(LosAngeles_data_list,1)
rh_1$rh
rh_2$rh

ggplot() +
  geom_line(data = rh_1, aes(x = 1:363, y = rh), color = "blue", linetype = "solid") +
  geom_line(data = rh_2, aes(x = 1:319, y = rh), color = "red", linetype = "solid") +
  labs(x = "Day", y = "Humidity", title = "Humidity Variation Over Days") +
  scale_color_manual(values = c("Madison" = "blue", "Los Angeles" = "red")) +
  theme_minimal()

ggplot() +
  geom_line(data = rh_1, aes(x = 1:363, y = rh, color = "Madison"), linetype = "solid") +
  geom_line(data = rh_2, aes(x = 1:319, y = rh, color = "Los Angeles"), linetype = "solid") +
  labs(x = "Day", y = "Humidity", title = "Humidity Variation Over Days", color = "City") +
  theme_minimal()

ggplot(rh, aes(x = 1:363, y = rh)) +
  geom_line() +
  labs(x = "Day of Year", y = "Relative Humidity", title = "Relative Humidity Over One Year")
