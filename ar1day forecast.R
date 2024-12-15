library(ggplot2)
library(rsample)
library(rpart)
library(caret)
library(caTools)
library(dplyr)
library(TTR)
library(earth)
library(vip)



setwd("E:/Pub/direction change/new data")

nifty <- read.csv("NIFTY50.csv")
# Volume of 16/02/2024 is missing , so this data has been deleted and 

#############################################################

# Technical Indicators

# ATR
# NATR

# AD
# ADXR

# Bollinger Band-Up
# Money Flow Index
# Mid Point
# STD
# T3
# Williams R
# Stochastic Fast K
# Stochastic Slow D
# RSI

# High Low Close (HLC)



hlc <- cbind(nifty$High, nifty$High, nifty$Close)
vix <- read.csv("vix.csv")
vix_close <- vix$Close



# Accumulation Distribution Index / Williams AD

ad3 <- williamsAD(HLC = hlc)



# Average True Range

main_atr3 <- ATR(HLC = hlc, n = 3)

atr3 <- main_atr3[,2] 



# Normalized Average True Range (NATR) 

natr3 <- (atr3/nifty$Close)*100


# Average Directional Index (ADX)

main_adx <- as.data.frame(ADX(HLC = hlc, n=3))

adx <- main_adx$ADX



# Average Directional Index rating (ADXR)

adx_lag3 <- 0

for(i in 4:length(adx)){
  
  adx_lag3[i] <- adx[i -3] 
  
}



adxr3 <- (adx + adx_lag3)/2 



# Bollinger Band (up)

main_bb <- as.data.frame(BBands(HLC = hlc,n=3))

bb_up3 <- main_bb$up

# Mid Point / Pivot point



# Money Flow Index
# Volume figure for date 16/02/2024 is missing both in NSE and Yahoo Finance dataset



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded)



# On Balance Volume

obv <- OBV(nifty$Close, nifty$Shares.Traded)



#Triple Exponential Moving Average (T3)

t3 <- EMA(EMA(EMA(nifty$Close,n=3),3),n=3)

# Williams R

wr3 <- WPR(HLC = hlc, n= 3)

# RSI

rsi3 <- RSI(nifty$Close,n=3)



# Stochastic slow & fast 

stochKD3 <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 3, nSlowD = 3))

fastK3<- stochKD3$fastK

fastD3 <- stochKD3$fastD

slowD3 <- stochKD3$slowD


hyper_grid <- expand.grid(
  
  degree = 1:3,
  nprune = seq(2,100, length.out = 10) %>% floor()
)





# Target Variable - Forecasting horizon (1 day,3 days,5 days,7 days)

target1 <- 0


for(i in 1:(nrow(nifty)-1)){
  
  target1[i] <- log(nifty$Close[i+1] / nifty$Close[i])
  
  target1 <- na.omit(target1)
  
}

input <- cbind(ad3, adx, adxr3, atr3, natr3, bb_up3, mfi, obv,t3,wr3,
               fastD3, fastK3, slowD3, rsi3, vix_close)


stndz_input <- scale(input, center = T, scale = T)



ar_input <- cbind(target1 = target1,stndz_input)

ar_input1.3 <- ar_input[-c(1:41),] # to ensure all the variables have same length 

ar_input1.3 <- as.data.frame(ar_input1.3)



# Data split

row_index <- sample(nrow(ar_input1.3), size = 0.7*nrow(ar_input1.3),replace = F) 

ar_input_train1.3 <- as.data.frame(ar_input1.3[row_index,])

ar_input_test1.3 <- as.data.frame(ar_input1.3[-row_index,])




cv_mars_1.3 <- train(
  
  x = subset(ar_input_train1.3, select = -target1),   
  
  y =  ar_input_train1.3$target1,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)

summary(cv_mars_1.3)

ar_train_predic1.3 <- predict(cv_mars_1.3, ar_input_train1.3)

ar_input_train_rmse1.3 <- RMSE(ar_train_predic1.3, ar_input_train1.3$target1)

ar_input_train_rmse1.3



ar_test_predic1.3 <- predict(cv_mars_1.3, ar_input_test1.3)

ar_input_test_rmse1.3 <- RMSE(ar_train_predic1.3, ar_input_test1.3$target1)

ar_input_test_rmse1.3




############################################################

# 5 day data


ad5 <- williamsAD(HLC = hlc)



# Average True Range

main_atr5 <- ATR(HLC = hlc, n = 5)

atr5 <- main_atr5[,2] 



# Normalized Average True Range (NATR) 

natr5 <- (atr5/nifty$Close)*100


# Average Directional Index (ADX)

main_adx <- as.data.frame(ADX(HLC = hlc, n=5))

adx5 <- main_adx$ADX



# Average Directional Index rating (ADXR)

adx_lag5 <- 0

for(i in 6:length(adx5)){
  
  adx_lag5[i] <- adx5[i - 5] 
  
}



adxr5 <- (adx5 + adx_lag5)/2 



# Bollinger Band (up)

main_bb <- as.data.frame(BBands(HLC = hlc,n=5))

bb_up5 <- main_bb$up

# Mid Point / Pivot point



# Money Flow Index
# Volume figure for date 16/02/2024 is missing both in NSE and Yahoo Finance dataset



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded)



# On Balance Volume

obv <- OBV(nifty$Close, nifty$Shares.Traded)



#Triple Exponential Moving Average (T3)

t5 <- EMA(EMA(EMA(nifty$Close,n=5),5),n=5)

# Williams R

wr5 <- WPR(HLC = hlc, n= 5)

# RSI

rsi5 <- RSI(nifty$Close,n=5)



# Stochastic slow & fast 

stochKD5 <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 5, nSlowD = 5))

fastK5 <- stochKD5$fastK

fastD5 <- stochKD5$fastD

slowD5 <- stochKD5$slowD


input5 <- cbind(ad5, adx5, adxr5, atr5, natr5, bb_up5, mfi, obv,t5,wr5,
               fastD5, fastK5, slowD5, rsi5, vix_close)


stndz_input5 <- scale(input5, center = T, scale = T)



ar_input1.5 <- cbind(target1 = target1,stndz_input5)

ar_input1.5 <- ar_input[-c(1:41),] # to ensure all the variables have same length 

ar_input1.5 <- as.data.frame(ar_input1.5)



# Data split

row_index <- sample(nrow(ar_input1.5), size = 0.7*nrow(ar_input1.5),replace = F) 

ar_input_train1.5 <- as.data.frame(ar_input1.5[row_index,])

ar_input_test1.5 <- as.data.frame(ar_input1.5[-row_index,])




cv_mars_1.5 <- train(
  
  x = subset(ar_input_train1.5, select = -target1),   
  
  y =  ar_input_train1.5$target1,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


summary(cv_mars_1.5)

ar_train_predic1.5 <- predict(cv_mars_1.5, ar_input_train1.5)

ar_input_train_rmse1.5 <- RMSE(ar_train_predic1.5, ar_input_train1.5$target1)

ar_input_train_rmse1.5



ar_test_predic1.5 <- predict(cv_mars_1.5, ar_input_test1.5)

ar_input_test_rmse1.5 <- RMSE(ar_train_predic1.5, ar_input_test1.5$target1)

ar_input_test_rmse1.5




############################################

# 7 Day data

ad7 <- williamsAD(HLC = hlc)



# Average True Range

main_atr7 <- ATR(HLC = hlc, n = 7)

atr7 <- main_atr7[,2] 



# Normalized Average True Range (NATR) 

natr7 <- (atr7/nifty$Close)*100


# Average Directional Index (ADX)

main_adx <- as.data.frame(ADX(HLC = hlc, n=7))

adx7 <- main_adx$ADX



# Average Directional Index rating (ADXR)

adx_lag7 <- 0

for(i in 8:length(adx7)){
  
  adx_lag7[i] <- adx7[i - 7] 
  
}



adxr7 <- (adx7 + adx_lag7)/2 



# Bollinger Band (up)

main_bb <- as.data.frame(BBands(HLC = hlc,n=7))

bb_up7 <- main_bb$up

# Mid Point / Pivot point



# Money Flow Index
# Volume figure for date 16/02/2024 is missing both in NSE and Yahoo Finance dataset



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded)



# On Balance Volume

obv <- OBV(nifty$Close, nifty$Shares.Traded)



#Triple Exponential Moving Average (T3)

t7 <- EMA(EMA(EMA(nifty$Close,n=7),7),n=7)

# Williams R

wr7 <- WPR(HLC = hlc, n= 7)

# RSI

rsi7 <- RSI(nifty$Close,n=7)

# Stochastic slow & fast 

stochKD7 <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 7, nSlowD = 7))

fastK7 <- stochKD7$fastK

fastD7 <- stochKD7$fastD

slowD7 <- stochKD7$slowD

input7 <- cbind(ad7, adx7, adxr7, atr7, natr7, bb_up7, mfi, obv,t7,wr7,
                fastD7, fastK7, slowD7, rsi7, vix_close)


stndz_input7 <- scale(input7, center = T, scale = T)



ar_input1.7 <- cbind(target1 = target1,stndz_input7)

ar_input1.7 <- ar_input1.7[-c(1:41),] # to ensure all the variables have same length 

ar_input1.7 <- as.data.frame(ar_input1.7)



# Data split

row_index <- sample(nrow(ar_input1.7), size = 0.7*nrow(ar_input1.7),replace = F) 

ar_input_train1.7 <- as.data.frame(ar_input1.7[row_index,])

ar_input_test1.7 <- as.data.frame(ar_input1.7[-row_index,])




cv_mars_1.7 <- train(
  
  x = subset(ar_input_train1.7, select = -target1),   
  
  y =  ar_input_train1.7$target1,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)




summary(cv_mars_1.7)

ar_train_predic1.7 <- predict(cv_mars_1.7, ar_input_train1.7)

ar_input_train_rmse1.7 <- RMSE(ar_train_predic1.7, ar_input_train1.7$target1)

ar_input_train_rmse1.7



ar_test_predic1.7 <- predict(cv_mars_1.7, ar_input_test1.7)

ar_input_test_rmse1.7 <- RMSE(ar_train_predic1.7, ar_input_test1.7$target1)

ar_input_test_rmse1.7




###################################################################

# 

cv_mars_1.3$bestTune
cv_mars_1.5$bestTune
cv_mars_1.7$bestTune


windows(10,10)

mfrow=c(3,1)

vip(cv_mars_1.3, bar = "TRUE", num_features = 10)
vip(cv_mars_1.5, bar = FALSE, num_features = 10)
vip(cv_mars_1.7, bar = FALSE,num_features = 10)


