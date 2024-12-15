
library(ggplot2)
library(rsample)
library(rpart)
library(caret)
library(caTools)
library(dplyr)
library(TTR)
library(earth)



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

setwd("E:/Pub/direction change/new data")

nifty <- read.csv("NIFTY50.csv")

hlc <- cbind(nifty$High, nifty$High, nifty$Close)
vix <- read.csv("vix.csv")
vix_close <- vix$Close

######################################

# Standard formulae

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

adxr14 <- (adx + adx_lag14)/2 




# Bollinger Band (up)

main_bb <- as.data.frame(BBands(HLC = hlc,n=20))

bb_up <- main_bb$up

# Mid Point / Pivot point



# Money Flow Index
# Volume figure for date 16/02/2024 is missing both in NSE and Yahoo Finance dataset



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded, n= 14)



# On Balance Volume

obv <- OBV(nifty$Close, nifty$Shares.Traded)



#Triple Exponential Moving Average (T3)

t3 <- EMA(EMA(EMA(nifty$Close,n=14),14),n=14)

# Williams R

wr <- WPR(HLC = hlc, n= 14)

# RSI

rsi <- RSI(nifty$Close,n=14)



# Stochastic slow & fast 

stochKD <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 3, nSlowD = 3))

fastK <- stochKD$fastK

fastD <- stochKD$fastD

slowD <- stochKD$slowD


# nifty Variable - Forecasting horizon (1 day,3 days,5 days,7 days)

nifty1 <- 0


for(i in 1:(nrow(nifty)-1)){
  
  nifty1[i] <- log(nifty$Close[i+1] / nifty$Close[i])
  
  nifty1 <- na.omit(nifty1)
  
}

nifty1.1 <- 0

for(i in 1:length(nifty1)){
  
  nifty1.1[i] <- if (nifty1[i] >= 0) 1 else 0
  
}




nifty3 <- 0


for(i in 1:(nrow(nifty)-3)){
  
  nifty3[i] <- log(nifty$Close[i+3] / nifty$Close[i])
  
  nifty3 <- na.omit(nifty3)
  
}

nifty3.1 <- 0

for(i in 1:length(nifty3)){
  
  nifty3.1[i] <- if (nifty3[i] >= 0) 1 else 0
  
}

nifty5 <- 0


for(i in 1:(nrow(nifty)-1)){
  
  nifty5[i] <- log(nifty$Close[i+1] / nifty$Close[i])
  
  nifty5 <- na.omit(nifty5)
  
}

nifty5.1 <- 0

for(i in 1:length(nifty5)){
  
  nifty5.1[i] <- if (nifty5[i] >= 0) 1 else 0
  
}

################################################################



# Logistic Regression 

input <- cbind(ad, adx, adxr14, atr, natr, bb_up, mfi, obv,t3,wr,
               fastD, fastK, slowD, rsi, vix_close)


stndz_input <- scale(input, center = T, scale = T)

file.create("standardized input.csv")

write.csv(stndz_input, "standardized input.csv")


input1 <- cbind(nifty1.1,stndz_input)

input1 <- input1[-c(1:41),] # to ensure all the variables have same length 

input1 <- as.data.frame(input1)


log_model1 <- glm(nifty1.1~., family = "binomial", data = input1)

summary(log_model1)


input3 <- as.data.frame(cbind(nifty3.1, input))

input3 <- input3[-c(1:41),]

log_model3 <- glm(nifty3.1~., family = "binomial", data = input3)

summary(log_model3)




input5 <- cbind(nifty5.1, input)

input5 <- as.data.frame(input5[-c(1:41),])



log_model5 <- glm(nifty5.1~., family = "binomial", data = input5)

summary(log_model5)


###########################################################

# 1 day ahead prediction with 3 day data

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

target1.3 <- 0

for(i in 1:length(target1)){
  
  target1.3[i] <- if(target1[i] >= 0) 1 else 0
}


input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, mfi, adx, adxr3, 
                natr3, atr3,ad3, obv,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input1.3 <- as.data.frame(cbind(target1.3, std_input3))




log_model1.3 <- glm(final_input1.3$target1.3~., family = "binomial", data = final_input1.3)

summary(log_model1.3)
coef(log_model1)
round(confint(log_model1),4)

set.seed(1234)

final_input1.3_cv <- final_input1.3[-c(1:16),]

# Data split

row_index <- sample(nrow(final_input1.3_cv), size = 0.7*nrow(final_input1.3_cv),replace = F) 

input_train3 <- final_input1.3_cv[row_index,]

input_test3 <- final_input1.3_cv[-row_index,]

cv_log_model1.3 <- train(as.factor(target1.3)~ .,
                       data = input_train3,
                       method = "glm",
                       family = "binomial",
                       trControl = trainControl(method = "cv", number = 10))


pred_log_model1.3 <- predict(cv_log_model1.3, input_train3) 

cm1.3_train <- confusionMatrix(as.factor(pred_log_model1.3), as.factor(input_train3$target1.3))

pred_log_model1.3_test <- predict(cv_log_model1.3, input_test3)

cm1.3_test <- confusionMatrix(as.factor(pred_log_model1.3_test), as.factor(input_test3$target1.3))


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



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded, n = 5)



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

target5 <- 0


for(i in 1:(nrow(nifty)-1)){
  
  target5[i] <- log(nifty$Close[i+1] / nifty$Close[i])
  
  target5 <- na.omit(target5)
  
}

target1.5 <- 0

for(i in 1:length(target5)){
  
  target1.5[i] <- if (target5[i] >= 0) 1 else 0
  
}

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi, adx5, adxr5, 
                              natr5, atr5,ad5, obv,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input1.5 <- as.data.frame(cbind(target1.5, std_input5))


log_model1.5 <- glm(final_input1.5$target1.5~., family = "binomial", data = final_input1.5)

summary(log_model1.5)


# Data Split

final_input1.5_cv <- final_input1.5[-c(1:20),]

row_index <- sample(nrow(final_input1.5_cv), size = 0.7*nrow(final_input1.5_cv),replace = F) 

input_train5 <- final_input1.5_cv[row_index,]

input_test5 <- final_input1.5_cv[-row_index,]




cv_log_model1.5 <- train(as.factor(target1.5)~ .,
                         data = input_train5,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model1.5 <- predict(cv_log_model1.5, input_train5) 

cm1.5_train <- confusionMatrix(as.factor(pred_log_model1.5), as.factor(input_train5$target1.5))

pred_log_model1.5_test <- predict(cv_log_model1.5, input_test5)

cm1.5_test <- confusionMatrix(as.factor(pred_log_model1.5_test), as.factor(input_test5$target1.5))






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



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded, n =7)



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

target7 <- 0


for(i in 1:(nrow(nifty)-1)){
  
  target7[i] <- log(nifty$Close[i+1] / nifty$Close[i])
  
  target7 <- na.omit(target7)
  
}

target1.7 <- 0

for(i in 1:length(target7)){
  
  target1.7[i] <- if (target7[i] >= 0) 1 else 0
  
}

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input1.7 <- as.data.frame(cbind(target1.7, std_input7))

log_model1.7 <- glm(final_input1.7$target1.7~., family = "binomial", data = final_input1.7)

summary(log_model1.7)

# Data Split

final_input1.7_cv <- final_input1.7[-c(1:24),]


row_index <- sample(nrow(final_input1.7_cv), size = 0.7*nrow(final_input1.7_cv),replace = F) 

input_train7 <- final_input1.7_cv[row_index,]

input_test7 <- final_input1.7_cv[-row_index,]

cv_log_model1.7 <- train(as.factor(target1.7)~ .,
                         data = input_train7,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model1.7 <- predict(cv_log_model1.7, input_train7) 

cm1.7_train <- confusionMatrix(as.factor(pred_log_model1.7), as.factor(input_train7$target1.7))

pred_log_model1.7_test <- predict(cv_log_model1.7, input_test7)

cm1.7_test <- confusionMatrix(as.factor(pred_log_model1.7_test), as.factor(input_test7$target1.7))


#######################################



##################################################

# 3 day ahead prediction

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



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded, n = 3)



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

target3 <- 0


for(i in 1:(nrow(nifty)-3)){
  
  target3[i] <- log(nifty$Close[i+3] / nifty$Close[i])
  
  target3 <- na.omit(target3)
  
}

target3.3 <- 0

for(i in 1:length(target3)){
  
  target3.3[i] <- if (target3[i] >= 0) 1 else 0
  
}

input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, mfi, adx, adxr3, 
                              natr3, atr3,ad3, obv,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input3 <- as.data.frame(cbind(target3.3, std_input3))


log_model3.3 <- glm(final_input3$target3.3~., family = "binomial", data = final_input3)
summary(log_model3.3)



# Data split

final_input3.3_cv <- final_input3[-c(1:16),]

row_index <- sample(nrow(final_input3.3_cv), size = 0.7*nrow(final_input3.3_cv),replace = F) 

input_train3 <- final_input3.3_cv[row_index,]

input_test3 <- final_input3.3_cv[-row_index,]


set.seed(1234)



cv_log_model3.3 <- train(as.factor(target3.3)~ .,
                         data = input_train3,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model3.3 <- predict(cv_log_model3.3, input_train3) 

cm3.3_train <- confusionMatrix(as.factor(pred_log_model3.3), as.factor(input_train3$target3.3))


pred_log_model3.3_test <- predict(cv_log_model3.3, input_test3)

cm3.3_test <- confusionMatrix(as.factor(pred_log_model3.3_test), as.factor(input_test3$target3.3))



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



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded, n = 5)



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

target5 <- 0


for(i in 1:(nrow(nifty)-3)){
  
  target5[i] <- log(nifty$Close[i+3] / nifty$Close[i])
  
  target5 <- na.omit(target5)
  
}

target3.5 <- 0

for(i in 1:length(target5)){
  
  target3.5[i] <- if (target5[i] >= 0) 1 else 0
  
}

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi, adx5, adxr5, 
                              natr5, atr5,ad5, obv,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input3.5 <- as.data.frame(cbind(target3.5, std_input5))

log_model3.5 <- glm(final_input3.5$target3.5~., family = "binomial", data = final_input3.5)


summary(log_model3.5)



# Data Split 

final_input3.5_cv <- final_input3.5[-c(1:20),]

row_index <- sample(nrow(final_input3.5_cv), size = 0.7*nrow(final_input3.5_cv),replace = F) 

input_train5 <- final_input3.5_cv[row_index,]

input_test5 <- final_input3.5_cv[-row_index,]



cv_log_model3.5 <- train(as.factor(target3.5)~ .,
                         data = input_train5,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model3.5 <- predict(cv_log_model3.5, input_train5) 

cm3.5_train <- confusionMatrix(as.factor(pred_log_model3.5), as.factor(input_train5$target3.5))


pred_log_model3.5_test <- predict(cv_log_model3.5, input_test5)

cm3.5test <- confusionMatrix(as.factor(pred_log_model3.5_test), as.factor(input_test5$target3.5))




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



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded, n = 7)



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

target7 <- 0


for(i in 1:(nrow(nifty)-3)){
  
  target7[i] <- log(nifty$Close[i+3] / nifty$Close[i])
  
  target7 <- na.omit(target7)
  
}

target3.7 <- 0

for(i in 1:length(target7)){
  
  target3.7[i] <- if (target7[i] >= 0) 1 else 0
  
}

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input7 <- as.data.frame(cbind(target3.7, std_input7))


log_model3.7 <- glm(final_input7$target3.7~., family = "binomial", data = final_input7)

summary(log_model7)


# Data Split

final_input3.7_cv <- final_input7[-c(1:24),]

row_index <- sample(nrow(final_input3.7_cv), size = 0.7*nrow(final_input3.7_cv),replace = F) 

input_train7 <- final_input3.7_cv[row_index,]

input_test7 <- final_input3.7_cv[-row_index,]


cv_log_model3.7 <- train(as.factor(target3.7)~ .,
                         data = input_train7,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model3.7 <- predict(cv_log_model3.7, input_train7) 

cm3.7_train <- confusionMatrix(as.factor(pred_log_model3.7), as.factor(input_train7$target3.7))


pred_log_model3.7_test <- predict(cv_log_model3.7, input_test7)

cm3.7test <- confusionMatrix(as.factor(pred_log_model3.7_test), as.factor(input_test7$target3.7))




##############################################################

# 5 day ahead prediction


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



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded , n = 3)



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

target3 <- 0


for(i in 1:(nrow(nifty)-5)){
  
  target3[i] <- log(nifty$Close[i+5] / nifty$Close[i])
  
  target3 <- na.omit(target3)
  
}

target5.3 <- 0

for(i in 1:length(target3)){
  
  target5.3[i] <- if (target3[i] >= 0) 1 else 0
  
}

input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, mfi, adx, adxr3, 
                              natr3, atr3,ad3, obv,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input3 <- as.data.frame(cbind(target5.3, std_input3))


log_model5.3 <- glm(final_input3$target5.3~., family = "binomial", data = final_input3)

summary(log_model5.3)



# Data split

final_input5.3_cv <- final_input3[-c(1:16),]

row_index <- sample(nrow(final_input5.3_cv), size = 0.7*nrow(final_input5.3_cv),replace = F) 

input_train3 <- final_input5.3_cv[row_index,]

input_test3 <- final_input5.3_cv[-row_index,]


set.seed(1234)



cv_log_model5.3 <- train(as.factor(target5.3)~ .,
                         data = input_train3,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model5.3 <- predict(cv_log_model5.3, input_train3) 

cm5.3_train <- confusionMatrix(as.factor(pred_log_model5.3), as.factor(input_train3$target5.3))


pred_log_model5.3_test <- predict(cv_log_model5.3, input_test3)

cm5.3_test <- confusionMatrix(as.factor(pred_log_model5.3_test), as.factor(input_test3$target5.3))



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



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded, n = 5)



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

target5 <- 0


for(i in 1:(nrow(nifty)-5)){
  
  target5[i] <- log(nifty$Close[i+5] / nifty$Close[i])
  
  target5 <- na.omit(target5)
  
}

target5.5 <- 0

for(i in 1:length(target5)){
  
  target5.5[i] <- if (target5[i] >= 0) 1 else 0
  
}

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi, adx5, adxr5, 
                              natr5, atr5,ad5, obv,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input5.5 <- as.data.frame(cbind(target5.5, std_input5))

log_model5.5 <- glm(final_input5.5$target5.5~., family = "binomial", data = final_input5.5)


summary(log_model5.5)



# Data Split 

final_input5.5_cv <- final_input5.5[-c(1:20),]

row_index <- sample(nrow(final_input5.5_cv), size = 0.7*nrow(final_input5.5_cv),replace = F) 

input_train5 <- final_input5.5_cv[row_index,]

input_test5 <- final_input5.5_cv[-row_index,]



cv_log_model5.5 <- train(as.factor(target5.5)~ .,
                         data = input_train5,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model5.5 <- predict(cv_log_model5.5, input_train5) 

cm5.5_train <- confusionMatrix(as.factor(pred_log_model5.5), as.factor(input_train5$target5.5))


pred_log_model5.5_test <- predict(cv_log_model5.5, input_test5)

cm5.5_test <- confusionMatrix(as.factor(pred_log_model5.5_test), as.factor(input_test5$target5.5))




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



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded, n =7)



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

target7 <- 0


for(i in 1:(nrow(nifty)-7)){
  
  target7[i] <- log(nifty$Close[i+7] / nifty$Close[i])
  
  target7 <- na.omit(target7)
  
}

target5.7 <- 0

for(i in 1:length(target7)){
  
  target5.7[i] <- if (target7[i] >= 0) 1 else 0
  
}

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input7 <- as.data.frame(cbind(target5.7, std_input7))


log_model5.7 <- glm(final_input7$target5.7~., family = "binomial", data = final_input7)

summary(log_model5.7)


# Data Split

final_input5.7_cv <- final_input7[-c(1:24),]

row_index <- sample(nrow(final_input5.7_cv), size = 0.7*nrow(final_input5.7_cv),replace = F) 

input_train7 <- final_input5.7_cv[row_index,]

input_test7 <- final_input5.7_cv[-row_index,]


cv_log_model5.7 <- train(as.factor(target5.7)~ .,
                         data = input_train7,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model5.7 <- predict(cv_log_model5.7, input_train7) 

cm5.7_train <- confusionMatrix(as.factor(pred_log_model5.7), as.factor(input_train7$target5.7))


pred_log_model5.7_test <- predict(cv_log_model5.7, input_test7)

cm5.7_test <- confusionMatrix(as.factor(pred_log_model5.7_test), as.factor(input_test7$target5.7))

##########################################################################
# Buy Signal  prediction 

# with standard formulae

# 1 day ahead prediction


file.create("std nifty return.csv")

nifty1 <- 0

for(i in 1:length(log_model1$fitted.values)){
  
  nifty1[i] <- if(log_model1$fitted.values[i] >= 0.5) 1 else 0
}


# 3 day ahead prediction


nifty3 <- 0

for(i in 1:length(log_model3$fitted.values)){
  
  nifty3[i] <- if(log_model3$fitted.values[i] >= 0.5) 1 else 0
}


# 5 day ahead prediction

nifty5 <- 0

for(i in 1:length(log_model5$fitted.values)){
  
  nifty5[i] <- if(log_model5$fitted.values[i] >= 0.5) 1 else 0
}


nifty_buy_std <- cbind(nifty1, nifty3, nifty5)

write.csv(nifty_buy_std, " std nifty return.csv")


###################################

# i day ahead prediction


nifty_buy1.3 <- 0

for(i in 1:length(log_model1.3$fitted.values)){
  
  nifty_buy1.3[i] <- if(log_model1.3$fitted.values[i] >= 0.5) 1 else 0
}


nifty_buy1.5 <- 0

for(i in 1:length(log_model1.5$fitted.values)){
  
  nifty_buy1.5[i] <- if(log_model1.5$fitted.values[i] >= 0.5) 1 else 0
}

nifty_buy1.7 <- 0

for(i in 1:length(log_model1.7$fitted.values)){
  
  nifty_buy1.7[i] <- if(log_model1.7$fitted.values[i] >= 0.5) 1 else 0
}


###################################################################

# 3 day ahead prediction

nifty_buy3.3 <- 0

for(i in 1:length(log_model3.3$fitted.values)){
  
  nifty_buy3.3[i] <- if(log_model3.3$fitted.values[i] >= 0.5) 1 else 0
}

nifty_buy3.5 <- 0

for(i in 1:length(log_model3.5$fitted.values)){
  
  nifty_buy3.5[i] <- if(log_model3.5$fitted.values[i] >= 0.5) 1 else 0
}

nifty_buy3.7 <- 0

for(i in 1:length(log_model3.7$fitted.values)){
  
  nifty_buy3.7[i] <- if(log_model3.7$fitted.values[i] >= 0.5) 1 else 0
}




############################################

# 5 day ahead prediction

nifty_buy5.3 <- 0

for(i in 1:length(log_model5.3$fitted.values)){
  
  nifty_buy5.3[i] <- if(log_model5.3$fitted.values[i] >= 0.5) 1 else 0
}

nifty_buy5.5 <- 0

for(i in 1:length(log_model5.5$fitted.values)){
  
  nifty_buy5.5[i] <- if(log_model5.5$fitted.values[i] >= 0.5) 1 else 0
}


nifty_buy5.7 <- 0

for(i in 1:length(log_model5.7$fitted.values)){
  
  nifty_buy5.7[i] <- if(log_model5.7$fitted.values[i] >= 0.5) 1 else 0
}



nifty_buy <- as.data.frame(cbind(nifty_buy1.3, nifty_buy1.5, nifty_buy1.7,
                                   nifty_buy3.3, nifty_buy3.5,nifty_buy3.7,
                                   nifty_buy5.3, nifty_buy5.5, nifty_buy5.7, nifty$Close))




write.csv(nifty_buy, "nifty return.csv")


#################################################################