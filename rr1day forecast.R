# Regularized Regression
library(ggplot2)
library(rsample)
library(rpart)
library(caret)
library(caTools)
library(dplyr)
library(TTR)
library(recipes)
library(glmnet)
library(caret)
library(vip)







# Ridge
# LASSO
# Elastic Net



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

##################################################################
# Accumulation Distribution Index / Williams AD

ad <- williamsAD(HLC = hlc)



# Average True Range

main_atr <- ATR(HLC = hlc, n = 14)

atr <- main_atr[,2] 



# Normalized Average True Range (NATR) 

natr <- (atr/nifty$Close)*100


# Average Directional Index (ADX)

main_adx <- as.data.frame(ADX(HLC = hlc, n=14))

adx <- main_adx$ADX

# Average Directional Index rating (ADXR)

adx_lag14 <- 0

for(i in 15:length(adx)){
  
  adx_lag14[i] <- adx[i -14] 
  
}

adxr <- (adx + adx_lag14)/2 




# Bollinger Band (up)

main_bb <- as.data.frame(BBands(HLC = hlc,n=20))

bb_up <- main_bb$up

# Mid Point / Pivot point



# Money Flow Index
# Volume figure for date 16/02/2024 is missing both in NSE and Yahoo Finance dataset



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded)



# On Balance Volume

obv <- OBV(nifty$Close, nifty$Shares.Traded)



#Triple Exponential Moving Average (T3)

t3 <- EMA(EMA(EMA(nifty$Close,n=10),10),n=10)

# Williams R

wr <- WPR(HLC = hlc, n= 14)

# RSI

rsi <- RSI(nifty$Close,n=14)



# Stochastic slow & fast 

stochKD <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 3, nSlowD = 3))

fastK <- stochKD$fastK

fastD <- stochKD$fastD

slowD <- stochKD$slowD

target1 <- 0


for(i in 1:(nrow(nifty)-1)){
  
  target1[i] <- log(nifty$Close[i+1] / nifty$Close[i])
  
  target1 <- na.omit(target1)
  
}


input <- as.numeric(cbind(slowD,fastD, fastK, rsi, mfi, adx, adxr, 
                              natr, atr,ad, obv,bb_up, t, wr, vix_close = vix$Close))

input <- input[-c(1:41),]

std_input <- scale(as.numeric(input), center = T, scale = T)

std_input <- std_input[-1,]

final_input1 <- as.data.frame(cbind(target1, std_input))

x1 <- model.matrix(target1 ~ . , data = final_input1)[,-1]

y <- final_input1$target1


ridge_train1 <- cv.glmnet(
  
  x = x1.3,
  y = y,
  alpha = 0
)


lasso_train1 <- cv.glmnet(
  
  x = x1.3,
  
  y = y ,
  
  alpha = 1
  
)


set.seed(1234)



# Data split

row_index <- sample(nrow(final_input1.3_cv), size = 0.7* nrow(final_input1.3_cv),replace = F) 

input_train3 <- final_input1.3_cv[row_index,]

input_test3 <- final_input1.3_cv[-row_index,]



x1.3 <- model.matrix(target1 ~ . , data = input_train3)[,-1]

y <- input_train3$target1


cv_ridge_train1.3 <- cv.glmnet(
  
  x = x1.3,
  y = y,
  alpha = 0
)



###########################################################################

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


# Target Variable - Forecasting horizon (1 day,3 days,5 days,7 days)

target1 <- 0


for(i in 1:(nrow(nifty)-1)){
  
  target1[i] <- log(nifty$Close[i+1] / nifty$Close[i])
  
  target1 <- na.omit(target1)
  
}




input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, mfi, adx, adxr3, 
                              natr3, atr3,ad3, obv,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input1.3 <- as.data.frame(cbind(target1, std_input3))




set.seed(1234)

final_input1.3_cv <- final_input1.3[-c(1:16),]

# Data split

row_index <- sample(nrow(final_input1.3_cv), size = 0.7*nrow(final_input1.3_cv),replace = F) 

input_train3 <- final_input1.3_cv[row_index,]

input_test3 <- final_input1.3_cv[-row_index,]



x1.3 <- model.matrix(target1 ~ . , data = input_train3)[,-1]

y <- input_train3$target1


cv_ridge_train1.3 <- cv.glmnet(
  
  x = x1.3,
  y = y,
  alpha = 0
)



input_test3_ivs <- as.matrix(input_test3[,-1])


ridge_predict_train1.3 <- predict(cv_ridge_train1.3, x1.3)

ridge_rmse_train1.3 <- RMSE(ridge_predict_train1.3, y) 

ridge_rmse_train1.3

ridge_predict_test1.3 <- predict(cv_ridge_train1.3, input_test3_ivs)



ridge_rmse_test1.3 <- RMSE(ridge_predict_test1.3, input_test3$target1)

ridge_rmse_test1.3

mean()

cv_lasso_train1.3 <- cv.glmnet(
  
  x = x1.3,
  
  y = y ,
  
  alpha = 1
  
)

lasso_predict_train1.3 <- predict(cv_lasso_train1.3, x1.3)

lasso_rmse_train1.3 <- RMSE(lasso_predict_train1.3, y)

lasso_rmse_train1.3

lasso_predict_test1.3 <- predict(cv_lasso_train1.3, input_test3_ivs)


lasso_rmse1.3 <- RMSE(lasso_predict_test1.3, input_test3$target1)

lasso_rmse1.3


###########################################################


# 5 day computation of TIs

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


# Target Variable - Forecasting horizon (1 day,3 days,5 days,7 days)

target1 <- 0


for(i in 1:(nrow(nifty)-1)){
  
  target1[i] <- log(nifty$Close[i+1] / nifty$Close[i])
  
  target1 <- na.omit(target1)
  
}


input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi, adx5, adxr5, 
                              natr5, atr5,ad5, obv,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input1.5 <- as.data.frame(cbind(target1, std_input5))




# Data Split

final_input1.5_cv <- final_input1.5[-c(1:20),]

row_index <- sample(nrow(final_input1.5_cv), size = 0.7*nrow(final_input1.5_cv),replace = F) 

input_train5 <- final_input1.5_cv[row_index,]

input_test5 <- final_input1.5_cv[-row_index,]





x1.5 <- model.matrix(target1 ~ . , data = input_train5)[,-1]

y <- input_train5$target1




cv_ridge_train1.5 <- cv.glmnet(
  
  x = x1.5,
  
  y = y, 
  
  alpha = 0 
)

ridge_predict_train1.5 <- predict(cv_ridge_train1.5, x1.5)

ridge_rmse_train1.5 <- RMSE(ridge_predict_train1.5, y)

ridge_rmse_train1.5


input_test5_ivs <- as.matrix(input_test5[,-1])

ridge_predict_test1.5 <- predict(cv_ridge_train1.5, input_test5_ivs)

ridge_rmse1.5 <- RMSE(ridge_predict_test1.5, input_test5$target1)

ridge_rmse1.5

cv_lasso_train1.5 <- cv.glmnet(
  
  x = x1.5,
  
  y = y ,
  
  alpha = 1
  
)


lasso_predict_train1.5 <- predict(cv_lasso_train1.5, x1.5)

lasso_rmse_train1.5 <- RMSE(lasso_predict_train1.5, y)

lasso_rmse_train1.5

lasso_predict_test1.5<- predict(cv_lasso_train1.5, input_test5_ivs)

lasso_rmse_test1.5 <- RMSE(lasso_predict_test1.5, input_test5$target1)

lasso_rmse_test1.5




##########################################################################

# 7 day computation of TIs


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

target1<- 0


for(i in 1:(nrow(nifty)-1)){
  
  target1[i] <- log(nifty$Close[i+1] / nifty$Close[i])
  
  target1 <- na.omit(target1)
  
}



input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input1.7 <- as.data.frame(cbind(target1, std_input7))


# Data Split

final_input1.7_cv <- final_input1.7[-c(1:24),]


row_index <- sample(nrow(final_input1.7_cv), size = 0.7*nrow(final_input1.7_cv),replace = F) 

input_train7 <- final_input1.7_cv[row_index,]

input_test7 <- final_input1.7_cv[-row_index,]


x1.7 <- model.matrix(target1 ~ . , data = input_train7)[,-1]

y <- input_train7$target1




cv_ridge_train1.7 <- cv.glmnet(
  
  x = x1.7,
  
  y = y,

  alpha = 0  
  
)




ridge_predict_train1.7 <- predict(cv_ridge_train1.7, x1.7)

ridge_rmse_train1.7 <- RMSE(ridge_predict_train1.7, y)

ridge_rmse_train1.7


input_test7_ivs <- as.matrix(input_test7[,-1])

ridge_predict_test1.7 <- predict(cv_ridge_train1.7, input_test7_ivs)

ridge_rmse_test1.7 <- RMSE(ridge_predict_test1.7, input_test7$target1)

ridge_rmse_test1.7





cv_lasso_train1.7 <- cv.glmnet(
  
  x = x1.7,
  
  y = y ,
  
  alpha = 1
  
)



lasso_predict_train1.7 <- predict(cv_lasso_train1.7, x1.7)

lasso_rmse_train1.7 <- RMSE(lasso_predict_train1.7, y)

lasso_rmse_train1.7

lasso_predict_test1.7 <- predict(cv_lasso_train1.7, input_test7_ivs)

lasso_rmse_test1.7 <- RMSE(lasso_predict_test1.7, input_test7$target1)

lasso_rmse_test1.7





#######################################

windows(10,10)
par(mfrow=c(3,1))


plot(input_test3$target1, main = "RR: 1 day ahead forecast with 3 day data", 
     col = 3, type = "l")

lines(ridge_predict_test1.3, col = "blue")

plot(input_test3$target1, main = "RR: 1 day ahead forecast with 5 day data", 
     col = 3, type = "l")

lines(ridge_predict_test1.5, col = "blue")

plot(input_test3$target1, main = "RR: 1 day ahead forecast with 7 day data", 
     col = 3, type = "l")

lines(ridge_predict_test1.7, col = "blue")

###########################################################

# Model Summary

vip(cv_ridge_train1.3)
vip(cv_ridge_train1.5)
vip(cv_ridge_train1.7)



vip(cv_lasso_train1.3)
vip(cv_lasso_train1.5)
vip(cv_lasso_train1.7)
