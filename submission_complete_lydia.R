### INSTALL PACKAGES AND LOAD ###

install.packages("zoo")
install.packages("glmmTMB")
library(tidyverse)
library(dplyr)
library(zoo)
library(glmmTMB)

### LOAD TRAIN DATASET ###

Train_data <- read_csv("G:/DATA ANALITYCS/Portfolio Project/predict-shots-in-the-premier-league/livit_dev_test_lydia/train.csv")

### SETUP TRAIN DATA ###
## Identify and develop predictive variables

Train_data$Date <- as.Date(Train_data$Date, format = "%m/%d/%y")

Train_data <- Train_data %>% 
  group_by(PlyID) %>% 
  arrange(PlyID, Date) %>% 
  mutate(MinutesPlayedAVG = round(rollapply(Minutes, width = 5, FUN = mean, partial = TRUE),3),)

Train_data <- Train_data %>% 
  group_by(PlyID) %>% 
  arrange(PlyID, Date) %>% 
  mutate_at(.vars = vars(MinutePlayedAVG),
            .funs = list(~lag(.)))

### VARIABLES ###

Train_data <- Train_data %>% 
  ungroup() %>% 
  mutate_at(.vars = vars(MinutesPlayedAVG), 
            .funs = list(~ round((. - mean(., na.rm = T))/sd(., na.rm = T),3)))

## Build Prediction Model
### Main Model
PredictModel1 <- glmmTMB(Shots ~ HmAw + WinProb + PlyPosition + MinutesPlayedAVG, data = Train_data, family = "nbinom2")
summary(PredictModel1)

### Backup Model
PredictModel2 <- glmmTMB(Shots ~ HmAw + WinProb + PlyPosition, data = Train_data, family = "nbinom2")
summary(PredictModel2)

### LOAD TEST DATASET ###

Test_data <- read_csv("G:/DATA ANALITYCS/Portfolio Project/predict-shots-in-the-premier-league/livit_dev_test_lydia/test.csv")

### SETUP TEST DATA ###

Test_data$Date <- as.Date(Test_data$Date, format = "%m/%d/%y")

Test_data <- Test_data %>% 
  group_by(PlyID) %>% 
  arrange(PlyID, Date) %>% 
  mutate(MinutesPlayedAVG = round(rollapply(Minutes, width = n(), FUN = mean, partial = TRUE),3),)

Test_data <- Test_data %>% 
  group_by(PlyID) %>% 
  arrange(PlyID, Date) %>% 
  mutate_at(.vars = vars(MinutesPlayedAVG),
            .funs = list(~lag(.)))

### VARIABLES ###

Test_data <- Test_data %>% 
  ungroup() %>% 
  mutate_at(.vars = vars(MinutesPlayedAVG), 
            .funs = list(~ round((. - mean(., na.rm = T))/sd(., na.rm = T),3)))

### TEST PREDICTED ###
## Run Prediction use Main Model

Test_data$Predicted <- round(predict(PredictModel1, newdata = Test_data, type = "response"),3)

## Use Backup Model for Run Backup Prediction

Test_data$Predicted[is.na(Test_data$Predicted)] <- round(predict(PredictModel2, 
                                                                 newdata = Test_data[is.na(Test_data$Predicted),], 
                                                                 type = "response"),3)

### Submission set ###

Test_data <- subset(Test_data, select = c(Id, Predicted))
Test_data <- data.frame(Test_data)

write_csv(Test_data, "G:/DATA ANALITYCS/Portfolio Project/predict-shots-in-the-premier-league/livit_dev_test_lydia/test_data.csv")

