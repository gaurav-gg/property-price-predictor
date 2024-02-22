library(mlbench)
library(caret)
library(dplyr)
data <- read.csv('data_all.csv')
control <- rfeControl(functions = rfFuncs,method = 'cv',number = 10)
data <- select(data,-rlat,-rlon,-price,-area,-source)
data$bhk[is.na(data$bhk)] <- 0
data <- select(data,-X)
data <- select(data,-city)
smp_size <- floor(0.2*nrow(data))
train_ind <- sample(seq_len(nrow(data)),size = smp_size)
train <- data[train_ind,]
results <- rfe(data[,-5],data[,5],sizes=c(1:4,6:78),rfeControl = control)
