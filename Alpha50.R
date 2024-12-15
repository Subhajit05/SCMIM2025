

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



####################################################################

windows(10,10)

par(mfrow = c(2,2))


plot(factor(nifty$Year) ,  nifty$Close, col = "red", ylab = "Nifty50",
     xlab = "Year",   main = "Nifty50 (01/03/19 - 28/03/24)")



plot(factor(inverse$Year) ,  inverse$Close, col = "red", ylab = "Nifty50 TR 1X",
     xlab = "Year", main = "Nifty50 TR1X (01/03/19 - 28/03/24)")



plot(factor(alpha50$Year), alpha50$Close, col = "red", ylab = "Nifty50 Alpha",
     xlab = "Year", main = "Nifty Alpha50 (01/03/19 - 28/03/24)")



###################################################

# Alpha 50 Analysis

windows(10,10)
par(mfrow = c(2,1))

plot(log(nifty$Close), main = "Nifty 50 - Nifty Alpha 50", ylab = "Log of close values",
     col="red", type = "l", ylim = c(8.8,10.8))

lines(log(alpha50$Close), col="blue", type = "l")


plot(log(nifty$Close), main = "Nifty 50 - Nifty50 TR 1X Inverse", 
     col="red", type = "l", ylim = c(5.2,10), ylab = "Log of close values")
lines(log(inverse$Close),  col="blue", type = "l")


###################################################################

setwd("E:/Pub/direction change/new data")

nifty <- read.csv("NIFTY50.csv")
# Volume of 16/02/2024 is missing , so this data has been deleted and 

alpha50 <- read.csv("NIFTY ALPHA 50.csv")

hlc <- cbind(alpha50$High, alpha50$Close, alpha50$Low)
vix <- read.csv("vix.csv")
vix_close <- vix$Close


alpha1 <- 0


for(i in 1:(nrow(alpha50)-1)){
  
  alpha1[i] <- log(alpha50$Close[i+1] / alpha50$Close[i])
  
  alpha1 <- na.omit(alpha1)
  
}


alpha1.1 <- 0

for(i in 1:length(alpha1)){
  
  alpha1.1 [i] <- if(alpha1[i] >= 0) 1 else 0
  
}


  
  
  
alpha3 <- 0

for(i in 1:(nrow(alpha50)-3)){
  
  alpha3[i] <- log(alpha50$Close[i+3] / alpha50$Close[i])
  
  alpha3 <- na.omit(alpha3)
}
  
  
alpha3.1 <- 0

for (i in 1:length(alpha3)){
  
  alpha3.1[i] <- if(alpha3[i] >= 0) 1 else 0
}


alpha5 <- 0
  
  for(i in 1:(nrow(alpha50)-5)){
    
    alpha5[i] <- log(alpha50$Close[i+5] / alpha50$Close[i])
    
    alpha5 <- na.omit(alpha5)
    
}

alpha5.1 <- 0

for(i in 1:length(alpha5)){
  
  alpha5.1[i] <- if(alpha5[i] >= 0)1 else 0
  
}

################################################################

# Standard formulae of technical indicators

# Accumulation Distribution

ad <- williamsAD(HLC = hlc)




# Average True Range
  
  main_atr <- ATR(HLC = hlc, n = 14)

atr <- main_atr[,2]


natr <- (atr/alpha50$Close)*100

# Average Directional Indexs

main_adx <- as.data.frame(ADX(HLC = hlc, n= 14))

adx <- main_adx$ADX


lag_adx <- 0

for(i in 1:length(adx)){
  
  lag_adx <- adx[i - 14]
}

adxr <- (adx + lag_adx)/2

# Bollinger Bands

main_bb <- as.data.frame(BBands(HLC = hlc, n =20))
                         
bb_up <- main_bb$up


# RSI

rsi <- RSI(alpha50$Close, n = 14)

# t3

t3 <- EMA(EMA(EMA(alpha50$Close, n = 14), n = 14), n= 14)


# William's R

wr <- WPR(HLC = hlc , n = 14)


# OBV

obv <- OBV(alpha50$Close, alpha50$Volume)


# MFI

mfi <- MFI(HLC = hlc, volume = alpha50$Volume, n = 14)



# Stochastics


main_stoch <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 3 , nSlowD = 3))

fastK<- main_stoch$fastK

fastD <- main_stoch$fastD

slowD <- main_stoch$slowD


main_input <- as.data.frame(cbind(ad, atr, natr, adx, adxr ,bb_up ,obv ,rsi,t3, wr,
                            mfi,fastK, fastD, slowD ,vix_close))

std_input <- scale(main_input, center = T, scale = T)

std_input <- std_input[-1,]


final_input1 <- as.data.frame(cbind(alpha1.1, std_input))


final_input1 <- final_input1[-c(1:39),]



std_log_alpha1 <- glm(alpha1.1 ~ ., family =  "binomial", data = final_input1)


summary(std_log_alpha1)





final_input3 <-  as.data.frame(cbind(alpha3.1 , std_input))

final_input3 <- final_input3[-c(1:39),]


std_log_alpha3 <- glm(alpha3.1 ~ ., family =  "binomial", data = final_input3)

summary(std_log_alpha3)






final_input5 <- as.data.frame(cbind(alpha5.1, std_input))

final_input5 <- final_input5[-c(1:39),]


std_log_alpha5<- glm(alpha5.1~ . , family = "binomial", data = final_input5)
summary(std_log_alpha5)



################################################

# 1  Day forecasting 3 day data



ad3 <- williamsAD(HLC = hlc)



# Average True Range

main_atr3 <- ATR(HLC = hlc, n = 3)

atr3 <- main_atr3[,2] 



# Normalized Average True Range (NATR) 

natr3 <- (atr3/alpha50$Close)*100


# Average Directional Index (ADX)

main_adx <- as.data.frame(ADX(HLC = hlc, n=3))

adx3 <- main_adx$ADX



# Average Directional Index rating (ADXR)

adx_lag3 <- 0

for(i in 4:length(adx3)){
  
  adx_lag3[i] <- adx[i -3] 
  
}



adxr3 <- (adx3 + adx_lag3)/2 



# Bollinger Band (up)

main_bb <- as.data.frame(BBands(HLC = hlc,n=3))

bb_up3 <- main_bb$up

# Mid Point / Pivot point



# Money Flow Index
# Volume figure for date 16/02/2024 is missing both in NSE and Yahoo Finance dataset



mfi3 <- MFI(HLC = hlc, volume = alpha50$Volume, n= 3)



# On Balance Volume

obv3 <- OBV(alpha50$Close, alpha50$Volume)



#Triple Exponential Moving Average (T3)

t3 <- EMA(EMA(EMA(alpha50$Close,n=3),3),n=3)

# Williams R

wr3 <- WPR(HLC = hlc, n= 3)

# RSI

rsi3 <- RSI(alpha50$Close,n=3)



# Stochastic slow & fast 

stochKD3 <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 3, nSlowD = 3))

fastK3<- stochKD3$fastK

fastD3 <- stochKD3$fastD

slowD3 <- stochKD3$slowD


# Target Variable - Forecasting horizon (1 day,3 days,5 days,7 days)


alpha1.3 <- 0

for(i in 1:length(alpha1)){
  
  alpha1.3[i] <- if (alpha1[i] >= 0) 1 else 0
  
}


input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, adx3, adxr3, obv3,  
                             mfi3, natr3, atr3,ad3,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input1.3 <- as.data.frame(cbind(alpha1.3, std_input3))


log_model_1.3 <- glm(alpha1.3 ~ ., family = "binomial", data = final_input1.3)

summary(log_model_1.3)



# Data split

final_input1.3_cv <- final_input1.3[-c(1:16),]
row_index <- sample(nrow(final_input1.3_cv), size = 0.7*nrow(final_input1.3_cv),replace = F) 

input_train3 <- final_input1.3_cv[row_index,]

input_test3 <- final_input1.3_cv[-row_index,]

cv_log_model1.3 <- train(as.factor(alpha1.3)~ .,
                         data = input_train3,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model1.3 <- predict(cv_log_model1.3, input_train3) 

cm1.3_train <- confusionMatrix(as.factor(pred_log_model1.3), as.factor(input_train3$alpha1.3))

cm1.3_train

pred_log_model1.3_test <- predict(cv_log_model1.3, input_test3)

cm1.3_test <- confusionMatrix(as.factor(pred_log_model1.3_test), as.factor(input_test3$alpha1.3))

cm1.3_test

########################################################################

# 5 day computation

ad5 <- williamsAD(HLC = hlc)



# Average True Range

main_atr5 <- ATR(HLC = hlc, n = 5)

atr5 <- main_atr5[,2] 



# Normalized Average True Range (NATR) 

natr5 <- (atr5/alpha50$Close)*100


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



mfi5 <- MFI(HLC = hlc, volume = alpha50$Volume, n = 5)



# On Balance Volume

obv5 <- OBV(alpha50$Close, alpha50$Volume)



#Triple Exponential Moving Average (T3)

t5 <- EMA(EMA(EMA(nifty$Close,n=5),5),n=5)

# Williams R

wr5 <- WPR(HLC = hlc, n= 5)

# RSI

rsi5 <- RSI(alpha50$Close,n=5)



# Stochastic slow & fast 

stochKD5 <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 5, nSlowD = 5))

fastK5 <- stochKD5$fastK

fastD5 <- stochKD5$fastD

slowD5 <- stochKD5$slowD


# Target Variable - Forecasting horizon (1 day,3 days,5 days,7 days)

alpha5 <- 0


for(i in 1:(nrow(alpha50)-1)){
  
  alpha5[i] <- log(alpha50$Close[i+1] / alpha50$Close[i])
  
  alpha5 <- na.omit(alpha5)
  
}

alpha1.5 <- 0

for(i in 1:length(alpha5)){
  
  alpha1.5[i] <- if (alpha5[i] >= 0) 1 else 0
  
}

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5,adx5, adxr5, mfi5, obv5,
                              natr5, atr5,ad5, bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input1.5 <- as.data.frame(cbind(alpha1.5, std_input5))


log_model_1.5 <- glm(alpha1.5 ~ ., family = "binomial", data = final_input1.5)

summary(log_model_1.5)



final_input1.5_cv <- as.data.frame(cbind(alpha1.5, std_input5))

final_input1.5_cv <- final_input1.5_cv[-c(1:20),]


row_index <- sample(nrow(final_input1.5_cv), size = 0.7*nrow(final_input1.5_cv),replace = F) 

input_train5 <- final_input1.5_cv[row_index,]

input_test5 <- final_input1.5_cv[-row_index,]

cv_log_model1.5 <- train(as.factor(alpha1.5)~ .,
                         data = input_train5,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model1.5 <- predict(cv_log_model1.5, input_train5) 

cm1.5_train <- confusionMatrix(as.factor(pred_log_model1.5), as.factor(input_train5$alpha1.5))

cm1.5_train

pred_log_model1.5_test <- predict(cv_log_model1.5, input_test5)

cm1.5_test <- confusionMatrix(as.factor(pred_log_model1.5_test), as.factor(input_test5$alpha1.5))

cm1.5_test

#############################################################################

# 7 Day computation

ad7 <- williamsAD(HLC = hlc)



# Average True Range

main_atr7 <- ATR(HLC = hlc, n = 7)

atr7 <- main_atr7[,2] 



# Normalized Average True Range (NATR) 

natr7 <- (atr7/alpha50$Close)*100


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



mfi7 <- MFI(HLC = hlc, volume = alpha50$Volume, n = 7)



# On Balance Volume

obv7 <- OBV(alpha50$Volume, alpha50$Volume)



#Triple Exponential Moving Average (T3)

t7 <- EMA(EMA(EMA(alpha50$Close,n=7),7),n=7)

# Williams R

wr7 <- WPR(HLC = hlc, n= 7)

# RSI

rsi7 <- RSI(alpha50$Close,n=7)

# Stochastic slow & fast 

stochKD7 <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 7, nSlowD = 7))

fastK7 <- stochKD7$fastK

fastD7 <- stochKD7$fastD

slowD7 <- stochKD7$slowD

alpha7 <- 0


for(i in 1:(nrow(alpha50)-1)){
  
  alpha7[i] <- log(alpha50$Close[i+1] / alpha50$Close[i])
  
  alpha7 <- na.omit(alpha7)
  
}

alpha1.7 <- 0

for(i in 1:length(alpha7)){
  
  alpha1.7[i] <- if (alpha7[i] >= 0) 1 else 0
  
}

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, adx7, adxr7, 
                              natr7, atr7,ad7,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input1.7 <- as.data.frame(cbind(alpha1.7, std_input7))



log_model_1.7 <- glm(alpha1.7 ~ ., family = "binomial", data = final_input1.7)

summary(log_model_1.7)



# Data Split

final_input1.7_cv <- final_input1.7[-c(1:24),]


row_index <- sample(nrow(final_input1.7_cv), size = 0.7*nrow(final_input1.7_cv),replace = F) 

input_train7 <- final_input1.7_cv[row_index,]

input_test7 <- final_input1.7_cv[-row_index,]

cv_log_model1.7 <- train(as.factor(alpha1.7)~ .,
                         data = input_train7,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model1.7 <- predict(cv_log_model1.7, input_train7) 

cm1.7_train <- confusionMatrix(as.factor(pred_log_model1.7), as.factor(input_train7$alpha1.7))

cm1.7_train


pred_log_model1.7_test <- predict(cv_log_model1.7, input_test7)

cm1.7_test <- confusionMatrix(as.factor(pred_log_model1.7_test), as.factor(input_test7$alpha1.7))

cm1.7_test

############################################################################################

# 3 day forecasting 

alpha3 <- 0


for(i in 1:(nrow(alpha50)-3)){
  
  alpha3[i] <- log(alpha50$Close[i+3] / alpha50$Close[i])
  
  alpha3 <- na.omit(alpha3)
  
}

alpha3.3 <- 0

for(i in 1:length(alpha3)){
  
  alpha3.3[i] <- if (alpha3[i] >= 0) 1 else 0
  
}


input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3,  adx3, adxr3, mfi3, obv3,
                              natr3, atr3,ad3,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input3.3 <- as.data.frame(cbind(alpha3.3, std_input3))



log_model_3.3 <- glm(alpha3.3 ~ ., family = "binomial", data = final_input3.3)

summary(log_model_3.3)




# Data split

final_input3.3_cv <- final_input3.3[-c(1:16),]



row_index <- sample(nrow(final_input3.3_cv), size = 0.7*nrow(final_input3.3_cv),replace = F) 

input_train3 <- final_input3.3_cv[row_index,]

input_test3 <- final_input3.3_cv[-row_index,]

cv_log_model3.3 <- train(as.factor(alpha3.3)~ .,
                         data = input_train3,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model3.3 <- predict(cv_log_model3.3, input_train3) 

cm1.3_train <- confusionMatrix(as.factor(pred_log_model3.3), as.factor(input_train3$alpha3.3))

cm1.3_train

pred_log_model3.3_test <- predict(cv_log_model3.3, input_test3)

cm3.3_test <- confusionMatrix(as.factor(pred_log_model3.3_test), as.factor(input_test3$alpha3.3))

cm3.3_test
###################################################

# 5 day computation

alpha5 <- 0


for(i in 1:(nrow(alpha50)-3)){
  
  alpha5[i] <- log(alpha50$Close[i+3] / alpha50$Close[i])
  
  alpha5 <- na.omit(alpha5)
  
}

alpha3.5 <- 0

for(i in 1:length(alpha5)){
  
  alpha3.5[i] <- if (alpha5[i] >= 0) 1 else 0
  
}



input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, adx5, adxr5, mfi5,
                              natr5, atr5,ad5, obv5,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input3.5 <- as.data.frame(cbind(alpha3.5, std_input5))


log_model_3.5 <- glm(alpha3.5 ~ ., family = "binomial", data = final_input3.5)

summary(log_model_3.5)




# Data Split

final_input3.5_cv <- as.data.frame(cbind(alpha3.5, std_input5))


final_input3.5_cv <- final_input3.5_cv[-c(1:20),]

row_index <- sample(nrow(final_input3.5_cv), size = 0.7*nrow(final_input3.5_cv),replace = F) 

input_train5 <- final_input3.5_cv[row_index,]

input_test5 <- final_input3.5_cv[-row_index,]

cv_log_model3.5 <- train(as.factor(alpha3.5)~ .,
                         data = input_train5,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model3.5 <- predict(cv_log_model3.5, input_train5) 

cm3.5_train <- confusionMatrix(as.factor(pred_log_model3.5), as.factor(input_train5$alpha3.5))

cm3.5_train

pred_log_model3.5_test <- predict(cv_log_model3.5, input_test3)

cm3.5_test <- confusionMatrix(as.factor(pred_log_model3.5_test), as.factor(input_test3$alpha3.5))

cm3.5_test

################################################################################

#3 day ahead prediction with 7 day data

alpha7 <- 0


for(i in 1:(nrow(alpha50)-3)){
  
  alpha7[i] <- log(alpha50$Close[i+3] / alpha50$Close[i])
  
  alpha7 <- na.omit(alpha7)
  
}

alpha3.7 <- 0

for(i in 1:length(alpha7)){
  
  alpha3.7[i] <- if (alpha7[i] >= 0) 1 else 0
  
}

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, adx7, adxr7, 
                              natr7, atr7,ad7,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input3.7 <- as.data.frame(cbind(alpha3.7, std_input7))

log_model_3.7 <- glm(alpha3.7 ~ ., family = "binomial", data = final_input3.7)

summary(log_model_3.7)



# Data Split

final_input1.7_cv <- final_input1.7[-c(1:24),]

row_index <- sample(nrow(final_input1.7_cv), size = 0.7*nrow(final_input1.7_cv),replace = F) 

input_train7 <- final_input1.7_cv[row_index,]

input_test7 <- final_input1.7_cv[-row_index,]

cv_log_model3.7 <- train(as.factor(alpha3.7)~ .,
                         data = input_train7,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model3.7 <- predict(cv_log_model3.7, input_train7) 

cm3.7_train <- confusionMatrix(as.factor(pred_log_model3.7), as.factor(input_train7$alpha3.7))

cm3.7_train


pred_log_model3.7_test <- predict(cv_log_model3.7, input_test7)

cm3.7_test <- confusionMatrix(as.factor(pred_log_model3.7_test), as.factor(input_test7$alpha3.7))

cm3.7_test

##########################################################################

# 5 day forecast 

# 3 day data


alpha5 <- 0


for(i in 1:(nrow(alpha50)-5)){
  
  alpha5[i] <- log(alpha50$Close[i+5] / alpha50$Close[i])
  
  alpha5 <- na.omit(alpha5)
  
}

alpha5.3 <- 0

for(i in 1:length(alpha5)){
  
  alpha5.3[i] <- if (alpha5[i] >= 0) 1 else 0
  
}


input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, adx3, adxr3, mfi3, obv3,
                              natr3, atr3,ad3,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input5.3 <- as.data.frame(cbind(alpha5.3, std_input3))

log_model_5.3 <- glm(alpha5.3 ~ ., family = "binomial", data = final_input5.3)

summary(log_model_5.3)




# Data split

final_input5.3_cv <- final_input5.3[-c(1:16),]

row_index <- sample(nrow(final_input5.3_cv), size = 0.7*nrow(final_input5.3_cv),replace = F) 

input_train3 <- final_input5.3_cv[row_index,]

input_test3 <- final_input5.3_cv[-row_index,]


cv_log_model5.3 <- train(as.factor(alpha5.3)~ .,
                         data = input_train3,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model5.3 <- predict(cv_log_model5.3, input_train3) 

cm5.3_train <- confusionMatrix(as.factor(pred_log_model5.3), as.factor(input_train3$alpha5.3))

cm5.3_train

pred_log_model5.3_test <- predict(cv_log_model5.3, input_test3)

cm5.3_test <- confusionMatrix(as.factor(pred_log_model5.3_test), as.factor(input_test3$alpha5.3))

cm5.3_test

##################################################################################

# 5 day data

alpha5 <- 0


for(i in 1:(nrow(alpha50)-5)){
  
  alpha5[i] <- log(alpha50$Close[i+5] / alpha50$Close[i])
  
  alpha5 <- na.omit(alpha5)
  
}

alpha5.5 <- 0

for(i in 1:length(alpha5)){
  
  alpha5.5[i] <- if (alpha5[i] >= 0) 1 else 0
  
}

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, adx5, adxr5, mfi5, obv5,
                              natr5, atr5,ad5,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input5.5<- as.data.frame(cbind(alpha5.5, std_input5))

log_model_5.5 <- glm(alpha5.5~., family = "binomial", data = final_input5.5)

summary(log_model_5.5)


# data split

final_input5.5_cv <- final_input5.5_cv[-c(1:20),]


row_index <- sample(nrow(final_input5.5_cv), size = 0.7*nrow(final_input5.5_cv),replace = F) 

input_train5 <- final_input5.5_cv[row_index,]

input_test5 <- final_input5.5_cv[-row_index,]




cv_log_model5.5 <- train(as.factor(alpha5.5)~ .,
                         data = input_train5,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model5.5 <- predict(cv_log_model5.5, input_train5) 

cm5.5_train <- confusionMatrix(as.factor(pred_log_model5.5), as.factor(input_train5$alpha5.5))

cm5.5_train

pred_log_model5.5_test <- predict(cv_log_model5.5, input_test5)

cm5.5_test <- confusionMatrix(as.factor(pred_log_model5.5_test), as.factor(input_test5$alpha5.5))

cm5.5_test

############################################################################

# 7 day computation

alpha5.7 <- 0

for(i in 1:length(alpha5)){
  
  alpha5.7[i] <- if(alpha5[i] >= 0) 1 else 0
  
}

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, adx7, adxr7, mfi7, obv7, 
                              natr7, atr7,ad7,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input5.7 <- as.data.frame(cbind(alpha5.7, std_input7))

log_model5.7 <- glm(alpha5.7 ~. , family = "binomial", data = final_input5.7)

summary(log_model5.7)



# Data Split

final_input5.7_cv <- final_input5.7[-c(1:24),]


row_index <- sample(nrow(final_input5.7_cv), size = 0.7*nrow(final_input5.7_cv),replace = F) 

input_train7 <- final_input5.7_cv[row_index,]

input_test7 <- final_input5.7_cv[-row_index,]

cv_log_model5.7 <- train(as.factor(alpha5.7)~ .,
                         data = input_train7,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model5.7 <- predict(cv_log_model5.7, input_train7) 

cm5.7_train <- confusionMatrix(as.factor(pred_log_model5.7), as.factor(input_train7$alpha5.7))

cm5.7_train

pred_log_model5.7_test <- predict(cv_log_model5.7, input_test7)

cm5.7_test <- confusionMatrix(as.factor(pred_log_model5.7_test), as.factor(input_test7$alpha5.7))

cm5.7_test

########################################################

nifty1 <- 0

for(i in 1:(nrow(nifty)-1)){
  
  nifty1[i] <- log(nifty$Close[i+1]/nifty$Close[i])
  
  nifty1 <- na.omit(nifty1)
  
}

nifty1.1 <- 0

for (i in 1:length(nifty1)){
  
  nifty1.1[i] <- if(nifty1[i] >= 0) 1 else 0
}

alpha1 <- 0

for(i in 1:(nrow(alpha50)-1)){
  
  alpha1[i] <- log(alpha50$Close[i+1]/alpha50$Close[i])
  
  alpha1 <- na.omit(alpha1)
  
}

alpha1.1 <- 0

for (i in 1:length(alpha1)){
  
  alpha1.1[i] <- if(alpha1[i] >= 0) 1 else 0
}

alpha1.1 <- alpha1.1[-c(1:3)]

cm1.1 <- confusionMatrix(as.factor(nifty1.1), as.factor(alpha1.1))
cm1.1

###############################################

nifty3 <- 0

for(i in 1:(nrow(nifty)-3)){
  
  nifty3[i] <- log(nifty$Close[i+3]/nifty$Close[i])
  
  nifty3 <- na.omit(nifty3)
  
}

nifty3.1 <- 0

for (i in 1:length(nifty3)){
  
  nifty3.1[i] <- if(nifty3[i] >= 0) 1 else 0
}

alpha3 <- 0

for(i in 1:(nrow(alpha50) - 3)){
  
  alpha3[i] <- log(alpha50$Close[i+3]/alpha50$Close[i])
  
  alpha3 <- na.omit(alpha3)
  
}

alpha3.1 <- 0

for (i in 1:length(alpha3)){
  
  alpha3.1[i] <- if(alpha3[i] >= 0) 1 else 0
}

alpha3.1 <- alpha3.1[-c(1:3)]

cm3.1 <- confusionMatrix(as.factor(nifty3.1), as.factor(alpha3.1))
cm3.1

################################################

nifty5 <- 0

for(i in 1:(nrow(nifty)-5)){
  
  nifty5[i] <- log(nifty$Close[i+5]/nifty$Close[i])
  
  nifty5 <- na.omit(nifty5)
  
}

nifty5.1 <- 0

for (i in 1:length(nifty5)){
  
  nifty5.1[i] <- if(nifty5[i] >= 0) 1 else 0
}

alpha5 <- 0

for(i in 1:(nrow(alpha50) - 5)){
  
  alpha5[i] <- log(alpha50$Close[i+5]/alpha50$Close[i])
  
  alpha5 <- na.omit(alpha5)
  
}

alpha5.1 <- 0

for (i in 1:length(alpha5)){
  
  alpha5.1[i] <- if(alpha5[i] >= 0) 1 else 0
}

alpha5.1 <- alpha5.1[-c(1:3)]

cm5.1 <- confusionMatrix(as.factor(nifty5.1), as.factor(alpha5.1))
cm5.1

###################################################################

# Adaptive regression

library(ggplot2)
library(rsample)
library(rpart)
library(caret)
library(caTools)
library(dplyr)
library(TTR)
library(earth)
library(vip)

###############################################

# adaptive regression with 3 day data

setwd("E:/Pub/direction change/new data")

nifty <- read.csv("NIFTY50.csv")
alpha50 <- read.csv("Nifty Alpha 50.csv")
hlc <- cbind(nifty$High, nifty$High, nifty$Close)
vix <- read.csv("vix.csv")
vix_close <- vix$Close


ad <- williamsAD(HLC= hlc)

main_atr <- ATR(HLC = hlc)

atr <- main_atr[,2] 



# Normalized Average True Range (NATR) 

natr <- (atr/nifty$Close)*100


# Average Directional Index (ADX)

main_adx <- as.data.frame(ADX(HLC = hlc))

adx <- main_adx$ADX



# Average Directional Index rating (ADXR)

adx_lag3 <- 0

for(i in 4:length(adx)){
  
  adx_lag3[i] <- adx[i -3] 
  
}



adxr3 <- (adx + adx_lag3)/2 



# Bollinger Band (up)

main_bb <- as.data.frame(BBands(HLC = hlc))

bb_up <- main_bb$up

# Mid Point / Pivot point



# Money Flow Index
# Volume figure for date 16/02/2024 is missing both in NSE and Yahoo Finance dataset



mfi <- MFI(HLC = hlc, volume = nifty$Shares.Traded)



# On Balance Volume

obv <- OBV(nifty$Close, nifty$Shares.Traded)



#Triple Exponential Moving Average (T3)

t3 <- EMA(EMA(EMA(nifty$Close)))

# Williams R

wr <- WPR(HLC = hlc)

# RSI

rsi <- RSI(nifty$Close)



# Stochastic slow & fast 

stochKD <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 3, nSlowD = 3))

fastK3 <- stochKD$fastK

fastD3 <- stochKD$fastD

slowD3 <- stochKD$slowD


hyper_grid <- expand.grid(
  
  degree = 1:3,
  nprune = seq(2,100, length.out = 10) %>% floor()
)





# Target Variable - Forecasting horizon (1 day,3 days,5 days,7 days)

alpha1 <- 0


for(i in 1:(nrow(alpha50)-1)){
  
  alpha1[i] <- log(alpha50$Close[i+1] / alpha50$Close[i])
  
  alpha1 <- na.omit(alpha1)
  
}

input <- cbind(ad, adx, adxr3, atr, natr, bb_up, mfi, obv,t3,wr,
               fastD3, fastK3, slowD3, rsi, vix_close)


stndz_input <- scale(input, center = T, scale = T)



ar_input <- cbind(alpha1,stndz_input)

ar_input1 <- ar_input[-c(1:30),] # to ensure all the variables have same length 

ar_input1 <- as.data.frame(ar_input1)



# Data split

row_index <- sample(nrow(ar_input1), size = 0.7*nrow(ar_input1),replace = F) 

ar_input_train1 <- as.data.frame(ar_input1[row_index,])

ar_input_test1 <- as.data.frame(ar_input1[-row_index,])




cv_mars_1.3 <- train(
  
  x = subset(ar_input_train1, select = -alpha1),   
  
  y =  ar_input_train1$alpha1,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_1.3, ar_input_train1)

ar_rmse_train1 <- RMSE(ar_pred_train, ar_input_train1$alpha1)

ar_rmse_train1


ar_pred_test <- predict(cv_mars_1.3, ar_input_test1)

ar_rmse_test1 <- RMSE(ar_pred_test, ar_input_test1$alpha1)

ar_rmse_test1

cv_mars_1.3$bestTune

vip(cv_mars_1.3, bar = FALSE)

#############################################################

# AR with 5 day data

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



mfi5 <- MFI(HLC = hlc, volume = nifty$Shares.Traded, n= 5)



# On Balance Volume

obv5 <- OBV(nifty$Close, nifty$Shares.Traded)



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

alpha1.5 <- 0


for(i in 1:(nrow(alpha50)-1)){
  
  alpha1.5[i] <- log(alpha50$Close[i+1] / alpha50$Close[i])
  
  alpha1.5 <- na.omit(alpha1.5)
  
}


input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi, adx5, adxr5, 
                              natr5, atr5,ad5, obv,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input1.5_cv <- as.data.frame(cbind(alpha1.5, std_input5))

final_input1.5_cv <- final_input1.5_cv[-c(1:20),]


row_index <- sample(nrow(final_input1.5_cv), size = 0.7*nrow(final_input1.5_cv),replace = F) 

input_train5 <- final_input1.5_cv[row_index,]

input_test5 <- final_input1.5_cv[-row_index,]


hyper_grid <- expand.grid(
  
  degree = 1:3,
  nprune = seq(2,100, length.out = 10) %>% floor()
)





cv_mars_1.5 <- train(
  
  x = subset(input_train5, select = -alpha1.5),   
  
  y =  input_train5$alpha1.5,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


vip(cv_mars_1.5)

cv_mars_1.5$bestTune

ar_pred_train1.5 <- predict(cv_mars_1.5, input_train5)

ar_rmse_train1.5 <- RMSE(ar_pred_train1.5, input_train5$alpha1.5)

ar_rmse_train1.5


ar_pred_test1.5 <- predict(cv_mars_1.5, input_test5)

ar_rmse_test1.5 <- RMSE(ar_pred_test1.5, input_test5$alpha1.5)

ar_rmse_test1.5

cv_mars_1$bestTune

########################################################################

# 7 day data 

alpha1.7 <- 0


for(i in 1:(nrow(alpha50)-1)){
  
  alpha1.7[i] <- log(alpha50$Close[i+1] / alpha50$Close[i])
  
  alpha1.7 <- na.omit(alpha1.7)
  
}

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input1.7 <- as.data.frame(cbind(alpha1.7, std_input7))



# Data Split

final_input1.7_cv <- final_input1.7[-c(1:24),]


row_index <- sample(nrow(final_input1.7_cv), size = 0.7*nrow(final_input1.7_cv),replace = F) 

input_train7 <- final_input1.7_cv[row_index,]

input_test7 <- final_input1.7_cv[-row_index,]

hyper_grid <- expand.grid(
  
  degree = 1:3,
  nprune = seq(2,100, length.out = 10) %>% floor()
)


cv_mars_1.7 <- train(
  
  
  x = subset(input_train7, select = -alpha1.7),   
  
  y =  input_train7$alpha1.7,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid
  
)

ar_pred_train1.7 <- predict(cv_mars_1.7, input_train7)

ar_rmse_train1.7 <- RMSE(ar_pred_train1.7, input_train7$alpha1.7)

ar_rmse_train1.7


ar_pred_test1.7 <- predict(cv_mars_1.7, input_test7)

ar_rmse_test1.7 <- RMSE(ar_pred_test1.7, input_test7$alpha1.7)

ar_rmse_test1.7

cv_mars_1.7$bestTune

vip(cv_mars_1.7)

#######################################################################
# 3 day ahead prediction 
# 3 day data


alpha3 <- 0


for(i in 1:(nrow(alpha50)-3)){
  
  alpha3[i] <- log(alpha50$Close[i+3] / alpha50$Close[i])
  
  alpha3 <- na.omit(alpha3)
  
}


input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, mfi, adx, adxr3, 
                              natr3, atr3,ad3, obv,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input3.3 <- as.data.frame(cbind(alpha3, std_input3))



final_input3.3_cv <- final_input1.3[-c(1:16),]

# Data split

row_index <- sample(nrow(final_input3.3_cv), size = 0.7*nrow(final_input3.3_cv),replace = F) 

input_train3 <- final_input3.3_cv[row_index,]

input_test3 <- final_input3.3_cv[-row_index,]

cv_mars_3.3 <- train(
  
  x = subset(input_train3, select = -alpha3),   
  
  y =  input_train3$alpha3,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid
  
)


ar_pred_train3.3 <- predict(cv_mars_3.3, input_train3)

ar_rmse_train3.3 <- RMSE(ar_pred_train3.3, input_train3$alpha3)

ar_rmse_train3.3


ar_pred_test3.3 <- predict(cv_mars_3.3, input_test3)

ar_rmse_test3.3 <- RMSE(ar_pred_test3.3, input_test3$alpha3)

ar_rmse_test3.3

cv_mars_3.3$bestTune

vip(cv_mars_3.3)

####################################################################################

# 3 day prediction with 5 day data

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi, adx5, adxr5, 
                              natr5, atr5,ad5, obv,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input3.5_cv <- as.data.frame(cbind(alpha3, std_input5))

final_input3.5_cv <- final_input3.5_cv[-c(1:20),]


row_index <- sample(nrow(final_input3.5_cv), size = 0.7*nrow(final_input3.5_cv),replace = F) 

input_train5 <- final_input3.5_cv[row_index,]

input_test5 <- final_input3.5_cv[-row_index,]

cv_mars_3.5 <- train(
  
  x = subset(input_train5, select = -alpha3),   
  
  y =  input_train5$alpha3,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid
  
)


ar_pred_train3.5 <- predict(cv_mars_3.5, input_train5)

ar_rmse_train3.5 <- RMSE(ar_pred_train3.5, input_train5$alpha3)

ar_rmse_train3.5


ar_pred_test3.5 <- predict(cv_mars_3.5, input_test5)

ar_rmse_test3.5 <- RMSE(ar_pred_test3.5, input_test5$alpha3)

ar_rmse_test3.5

cv_mars_3.5$bestTune

vip(cv_mars_3.5)

##################################################################################

# 3 day prediction with 7 day data

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input3.7 <- as.data.frame(cbind(alpha3, std_input7))



# Data Split

final_input3.7_cv <- final_input3.7[-c(1:24),]


row_index <- sample(nrow(final_input3.7_cv), size = 0.7*nrow(final_input3.7_cv),replace = F) 

input_train7 <- final_input3.7_cv[row_index,]

input_test7 <- final_input3.7_cv[-row_index,]

cv_mars_3.7 <- train(
  
  x = subset(input_train7, select = -alpha3),   
  
  y =  input_train7$alpha3,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid
  
  )

ar_pred_train3.7 <- predict(cv_mars_3.7, input_train7)

ar_rmse_train3.7 <- RMSE(ar_pred_train3.7, input_train7$alpha3)

ar_rmse_train3.7


ar_pred_test3.7 <- predict(cv_mars_3.7, input_test7)

ar_rmse_test3.7 <- RMSE(ar_pred_test3.7, input_test7$alpha3)

ar_rmse_test3.7

cv_mars_3.7$bestTune

vip(cv_mars_3.7)
#################################################################

# 5 day prediction with 3 day data

alpha5 <- 0

for(i in 1:(nrow(alpha50)-5)){
  
  alpha5[i] <- log(alpha50$Close[i+5]/alpha50$Close[i])
  
  alpha5 <- na.omit(alpha5)
}

input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, mfi5, adx, adxr3, 
                              natr3, atr3,ad3, obv5,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input5.3 <- as.data.frame(cbind(alpha5, std_input3))



final_input5.3_cv <- final_input5.3[-c(1:16),]

# Data split

row_index <- sample(nrow(final_input5.3_cv), size = 0.7*nrow(final_input5.3_cv),replace = F) 

input_train3 <- final_input5.3_cv[row_index,]

input_test3 <- final_input5.3_cv[-row_index,]

cv_mars_5.3 <- train(
  
  x = subset(input_train3, select = -alpha5),   
  
  y =  input_train3$alpha5,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train5.3 <- predict(cv_mars_5.3, input_train3)

ar_rmse_train5.3 <- RMSE(ar_pred_train5.3, input_train3$alpha5)

ar_rmse_train5.3


ar_pred_test5.3 <- predict(cv_mars_5.3, input_test3)

ar_rmse_test5.3 <- RMSE(ar_pred_test5.3, input_test3$alpha5)

ar_rmse_test5.3

cv_mars_5.3$bestTune

vip(cv_mars_5.3)

##################################################################################

# 5 day prediction with 5 day data





input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi5, adx5, adxr5, 
                              natr5, atr5,ad5, obv5,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]







final_input5.5_cv <- as.data.frame(cbind(alpha5, std_input5))

final_input5.5_cv <- final_input5.5_cv[-c(1:20),]


row_index <- sample(nrow(final_input5.5_cv), size = 0.7 * nrow(final_input5.5_cv),replace = F) 

input_train5 <- final_input5.5_cv[row_index,]

input_test5 <- final_input5.5_cv[-row_index,]



cv_mars_5.5 <- train(
  
  x = subset(input_train5, select = -alpha5),   
  
  y =  input_train5$alpha5,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid
  
)


ar_pred_train5.5 <- predict(cv_mars_5.5, input_train5)

ar_rmse_train5.5 <- RMSE(ar_pred_train5.5, input_train5$alpha5)

ar_rmse_train5.5


ar_pred_test5.5 <- predict(cv_mars_5.5, input_test5)

ar_rmse_test5.5 <- RMSE(ar_pred_test5.5, input_test5$alpha5)

ar_rmse_test5.5

cv_mars_5.5$bestTune

vip(cv_mars_5.5)

###################################################################################

# 5 day prediction with 7 day data

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input5.7 <- as.data.frame(cbind(alpha5, std_input7))


log_model_3.7 <- glm(alpha3.7 ~ ., family = "binomial", data = final_input3.7)

summary(log_model_3.7)


# Data Split

final_input5.7_cv <- final_input5.7[-c(1:24),]


row_index <- sample(nrow(final_input5.7_cv), size = 0.7*nrow(final_input5.7_cv),replace = F) 

input_train7 <- final_input5.7_cv[row_index,]

input_test7 <- final_input5.7_cv[-row_index,]

cv_mars_5.7 <- train(
  
  x = subset(input_train7, select = -alpha5),   
  
  y =  input_train7$alpha5,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid
  
)

ar_pred_train5.7 <- predict(cv_mars_5.7, input_train7)

ar_rmse_train5.7 <- RMSE(ar_pred_train5.7, input_train7$alpha5)

ar_rmse_train5.7


ar_pred_test5.7 <- predict(cv_mars_5.7, input_test7)

ar_rmse_test5.7 <- RMSE(ar_pred_test5.7, input_test7$alpha5)

ar_rmse_test5.7

cv_mars_5.7$bestTune

vip(cv_mars_5.7)

#################################################################################

# Regularised Regression


# 1 day prediction with 3 day data

alpha1 <- 0


for(i in 1:(nrow(alpha50)-1)){
  
  alpha1[i] <- log(alpha50$Close[i+1] / alpha50$Close[i])
  
  alpha1 <- na.omit(alpha1)
  
}


input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, mfi, adx, adxr3, 
                              natr3, atr3,ad3, obv,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input1.3 <- as.data.frame(cbind(alpha1, std_input3))



final_input1.3_cv <- final_input1.3[-c(1:16),]

# Data split

row_index <- sample(nrow(final_input1.3_cv), size = 0.7*nrow(final_input1.3_cv),replace = F) 

input_train3 <- final_input1.3_cv[row_index,]

input_test3 <- final_input1.3_cv[-row_index,]


x1.3 <- model.matrix(alpha1 ~ . , data = input_train3)[,-1]

y <- input_train3$alpha1


ridge_train1.3 <- cv.glmnet(
  
  x = x1.3,
  y = y,
  alpha = 0
)


lasso_train1.3 <- cv.glmnet(
  
  x = x1.3,
  
  y = y ,
  
  alpha = 1
  
)


input_test3_ivs <- as.matrix(input_test3[,-1])


ridge_predict_train1.3 <- predict(ridge_train1.3, x1.3)

ridge_rmse_train1.3 <- RMSE(ridge_predict_train1.3, y) 

ridge_rmse_train1.3

ridge_predict_test1.3 <- predict(ridge_train1.3, input_test3_ivs)

ridge_rmse_test1.3 <- RMSE(ridge_predict_test1.3, input_test3$alpha1)

ridge_rmse_test1.3

lasso_predict_train1.3 <- predict(lasso_train1.3, x1.3)

lasso_rmse_train1.3 <- RMSE(lasso_predict_train1.3, y)

lasso_rmse_train1.3

lasso_predict_test1.3 <- predict(lasso_train1.3, input_test3_ivs)

lasso_rmse_test1.3 <- RMSE(lasso_predict_test1.3,input_test3$alpha1)

lasso_rmse_test1.3


vip(ridge_train1.3)
vip(lasso_train1.3, bar=FALSE)




#####################################################################

# 1 day prediction with 5 day data

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi, adx5, adxr5, 
                              natr5, atr5,ad5, obv,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input1.5_cv <- as.data.frame(cbind(alpha1, std_input5))

final_input1.5_cv <- final_input1.5_cv[-c(1:20),]


row_index <- sample(nrow(final_input1.5_cv), size = 0.7*nrow(final_input1.5_cv),replace = F) 

input_train5 <- final_input1.5_cv[row_index,]

input_test5 <- final_input1.5_cv[-row_index,]



x1.5 <- model.matrix(alpha1 ~ . , data = input_train5)[,-1]

y <- input_train5$alpha1


ridge_train1.5 <- cv.glmnet(
  
  x = x1.5,
  y = y,
  alpha = 0
)


lasso_train1.5 <- cv.glmnet(
  
  x = x1.5,
  
  y = y ,
  
  alpha = 1
  
)


input_test5_ivs <- as.matrix(input_test5[,-1])


ridge_predict_train1.5 <- predict(ridge_train1.5, x1.5)

ridge_rmse_train1.5 <- RMSE(ridge_predict_train1.5, y) 

ridge_rmse_train1.5

ridge_predict_test1.5 <- predict(ridge_train1.5, input_test5_ivs)

ridge_rmse_test1.5 <- RMSE(ridge_predict_test1.5, input_test5$alpha1)

ridge_rmse_test1.5

lasso_predict_train1.5 <- predict(lasso_train1.5, x1.5)

lasso_rmse_train1.5 <- RMSE(lasso_predict_train1.5, y)

lasso_rmse_train1.5

lasso_predict_test1.5 <- predict(lasso_train1.5, input_test5_ivs)

lasso_rmse_test1.5 <- RMSE(lasso_predict_test1.5,input_test5$alpha1)

lasso_rmse_test1.5


vip(ridge_train1.5)
vip(lasso_train1.5, bar=FALSE)
#######################################################################
# 1 day prediction with 7 day data

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input1.7 <- as.data.frame(cbind(alpha1, std_input7))



# Data Split

final_input1.7_cv <- final_input1.7[-c(1:24),]


row_index <- sample(nrow(final_input1.7_cv), size = 0.7*nrow(final_input1.7_cv),replace = F) 

input_train7 <- final_input1.7_cv[row_index,]

input_test7 <- final_input1.7_cv[-row_index,]


x1.7 <- model.matrix(alpha1 ~ . , data = input_train7)[,-1]

y <- input_train7$alpha1


ridge_train1.7 <- cv.glmnet(
  
  x = x1.7,
  y = y,
  alpha = 0
)


lasso_train1.7 <- cv.glmnet(
  
  x = x1.7,
  
  y = y ,
  
  alpha = 1
  
)


input_test7_ivs <- as.matrix(input_test7[,-1])


ridge_predict_train1.7 <- predict(ridge_train1.7, x1.7)

ridge_rmse_train1.7 <- RMSE(ridge_predict_train1.7, y) 

ridge_rmse_train1.7

ridge_predict_test1.7 <- predict(ridge_train1.7, input_test7_ivs)

ridge_rmse_test1.7 <- RMSE(ridge_predict_test1.7, input_test7$alpha1)

ridge_rmse_test1.7

lasso_predict_train1.7 <- predict(lasso_train1.7, x1.7)

lasso_rmse_train1.7 <- RMSE(lasso_predict_train1.7, y)

lasso_rmse_train1.7

lasso_predict_test1.7 <- predict(lasso_train1.7, input_test7_ivs)

lasso_rmse_test1.7 <- RMSE(lasso_predict_test1.7,input_test7$alpha1)

lasso_rmse_test1.7


vip(ridge_train1.7)
vip(lasso_train1.7, bar=FALSE)


################################################################################

# 3 day prediction with 3 day  data

alpha3 <- 0


for(i in 1:(nrow(alpha50)-3)){
  
  alpha3[i] <- log(alpha50$Close[i+3] / alpha50$Close[i])
  
  alpha3 <- na.omit(alpha1)
  
}


input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, mfi, adx, adxr3, 
                              natr3, atr3,ad3, obv,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input3.3 <- as.data.frame(cbind(alpha3, std_input3))



final_input3.3_cv <- final_input3.3[-c(1:16),]

# Data split

row_index <- sample(nrow(final_input3.3_cv), size = 0.7*nrow(final_input3.3_cv),replace = F) 

input_train3 <- final_input3.3_cv[row_index,]

input_test3 <- final_input3.3_cv[-row_index,]


x3.3 <- model.matrix(alpha3 ~ . , data = input_train3)[,-1]

y <- input_train3$alpha3


ridge_train3.3 <- cv.glmnet(
  
  x = x3.3,
  y = y,
  alpha = 0
)


lasso_train3.3 <- cv.glmnet(
  
  x = x3.3,
  
  y = y ,
  
  alpha = 1
  
)


input_test3_ivs <- as.matrix(input_test3[,-1])


ridge_predict_train3.3 <- predict(ridge_train3.3, x3.3)

ridge_rmse_train3.3 <- RMSE(ridge_predict_train3.3, y) 

ridge_rmse_train3.3

ridge_predict_test3.3 <- predict(ridge_train3.3, input_test3_ivs)

ridge_rmse_test3.3 <- RMSE(ridge_predict_test3.3, input_test3$alpha3)

ridge_rmse_test3.3

lasso_predict_train3.3 <- predict(lasso_train3.3, x3.3)

lasso_rmse_train3.3 <- RMSE(lasso_predict_train3.3, y)

lasso_rmse_train3.3

lasso_predict_test3.3 <- predict(lasso_train3.3, input_test3_ivs)

lasso_rmse_test3.3 <- RMSE(lasso_predict_test3.3,input_test3$alpha3)

lasso_rmse_test3.3


vip(ridge_train3.3)
vip(lasso_train3.3, bar=FALSE)

############################################################################

# 3 day prediction with 5 day data

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi5, adx5, adxr5, 
                              natr5, atr5,ad5, obv5,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input3.5_cv <- as.data.frame(cbind(alpha3, std_input5))

final_input3.5_cv <- final_input3.5_cv[-c(1:20),]







# Data Split 

row_index <- sample(nrow(final_input3.5_cv), size = 0.7*nrow(final_input3.5_cv),replace = F) 

input_train5 <- final_input3.5_cv[row_index,]

input_test5 <- final_input3.5_cv[-row_index,]


x3.5 <- model.matrix(alpha3 ~ . , data = input_train5)[,-1]

y <- input_train5$alpha3


ridge_train3.5 <- cv.glmnet(
  
  x = x3.5,
  y = y,
  alpha = 0
)


lasso_train3.5 <- cv.glmnet(
  
  x = x3.5,
  
  y = y ,
  
  alpha = 1
  
)


input_test5_ivs <- as.matrix(input_test5[,-1])


ridge_predict_train3.5 <- predict(ridge_train3.5, x3.5)

ridge_rmse_train3.5 <- RMSE(ridge_predict_train3.5, y) 

ridge_rmse_train3.5

ridge_predict_test3.5 <- predict(ridge_train3.5, input_test5_ivs)

ridge_rmse_test3.5 <- RMSE(ridge_predict_test3.5, input_test5$alpha3)

ridge_rmse_test3.5

lasso_predict_train3.5 <- predict(lasso_train3.5, x3.5)

lasso_rmse_train3.5 <- RMSE(lasso_predict_train3.5, y)

lasso_rmse_train3.5

lasso_predict_test3.5 <- predict(lasso_train3.5, input_test5_ivs)

lasso_rmse_test3.5 <- RMSE(lasso_predict_test3.5,input_test5$alpha3)

lasso_rmse_test3.5


vip(ridge_train3.5)
vip(lasso_train3.5, bar=FALSE)

########################################################

# 3 day prediction with 7 day data


input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input3.7 <- as.data.frame(cbind(alpha3, std_input7))



# Data Split

final_input3.7_cv <- final_input3.7[-c(1:24),]


row_index <- sample(nrow(final_input3.7_cv), size = 0.7*nrow(final_input3.7_cv),replace = F) 

input_train7 <- final_input3.7_cv[row_index,]

input_test7 <- final_input3.7_cv[-row_index,]


x3.7 <- model.matrix(alpha3 ~ . , data = input_train7)[,-1]

y <- input_train7$alpha3


ridge_train3.7 <- cv.glmnet(
  
  x = x3.7,
  y = y,
  alpha = 0
)


lasso_train3.7 <- cv.glmnet(
  
  x = x3.7,
  
  y = y ,
  
  alpha = 1
  
)


input_test7_ivs <- as.matrix(input_test7[,-1])


ridge_predict_train3.7 <- predict(ridge_train3.7, x3.7)

ridge_rmse_train3.7 <- RMSE(ridge_predict_train3.7, y) 

ridge_rmse_train3.7

ridge_predict_test3.7 <- predict(ridge_train3.7, input_test7_ivs)

ridge_rmse_test3.7 <- RMSE(ridge_predict_test3.7, input_test7$alpha3)

ridge_rmse_test3.7

lasso_predict_train3.7 <- predict(lasso_train3.7, x3.7)

lasso_rmse_train3.7 <- RMSE(lasso_predict_train3.7, y)

lasso_rmse_train3.7

lasso_predict_test3.7 <- predict(lasso_train3.7, input_test7_ivs)

lasso_rmse_test3.7 <- RMSE(lasso_predict_test3.7,input_test7$alpha3)

lasso_rmse_test3.7


vip(ridge_train3.7)
vip(lasso_train3.7, bar=FALSE)

####################################################################################

# 5 day prediction 3 day data


alpha5 <- 0


for(i in 1:(nrow(alpha50)-5)){
  
  alpha5[i] <- log(alpha50$Close[i+5] / alpha50$Close[i])
  
  alpha5 <- na.omit(alpha5)
  
}


input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, mfi, adx, adxr3, 
                              natr3, atr3,ad3, obv,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input5.3 <- as.data.frame(cbind(alpha5, std_input3))



final_input5.3_cv <- final_input5.3[-c(1:16),]

# Data split

row_index <- sample(nrow(final_input5.3_cv), size = 0.7*nrow(final_input5.3_cv),replace = F) 

input_train3 <- final_input5.3_cv[row_index,]

input_test3 <- final_input5.3_cv[-row_index,]


x5.3 <- model.matrix(alpha5 ~ . , data = input_train3)[,-1]

y <- input_train3$alpha5


ridge_train5.3 <- cv.glmnet(
  
  x = x5.3,
  y = y,
  alpha = 0
)


lasso_train5.3 <- cv.glmnet(
  
  x = x5.3,
  
  y = y ,
  
  alpha = 1
  
)


input_test3_ivs <- as.matrix(input_test3[,-1])


ridge_predict_train5.3 <- predict(ridge_train5.3, x5.3)

ridge_rmse_train5.3 <- RMSE(ridge_predict_train5.3, y) 

ridge_rmse_train5.3

ridge_predict_test5.3 <- predict(ridge_train5.3, input_test3_ivs)

ridge_rmse_test5.3 <- RMSE(ridge_predict_test5.3, input_test3$alpha5)

ridge_rmse_test5.3

lasso_predict_train5.3 <- predict(lasso_train5.3, x5.3)

lasso_rmse_train5.3 <- RMSE(lasso_predict_train5.3, y)

lasso_rmse_train5.3

lasso_predict_test5.3 <- predict(lasso_train5.3, input_test3_ivs)

lasso_rmse_test5.3 <- RMSE(lasso_predict_test5.3,input_test3$alpha5)

lasso_rmse_test5.3


vip(ridge_train5.3)
vip(lasso_train5.3, bar=FALSE)






###########################################

# 5 day prediction 5 day data

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, mfi, adx5, adxr5, 
                              natr5, atr5,ad5, obv,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input5.5_cv <- as.data.frame(cbind(alpha5, std_input5))

final_input5.5_cv <- final_input5.5_cv[-c(1:20),]


row_index <- sample(nrow(final_input5.5_cv), size = 0.7*nrow(final_input5.5_cv),replace = F) 

input_train5 <- final_input5.5_cv[row_index,]

input_test5 <- final_input5.5_cv[-row_index,]



x5.5 <- model.matrix(alpha5 ~ . , data = input_train5)[,-1]

y <- input_train5$alpha5


ridge_train5.5 <- cv.glmnet(
  
  x = x5.5,
  y = y,
  alpha = 0
)


lasso_train5.5 <- cv.glmnet(
  
  x = x5.5,
  
  y = y ,
  
  alpha = 1
  
)


input_test5_ivs <- as.matrix(input_test5[,-1])


ridge_predict_train5.5 <- predict(ridge_train5.5, x5.5)

ridge_rmse_train5.5 <- RMSE(ridge_predict_train5.5, y) 

ridge_rmse_train5.5

ridge_predict_test5.5 <- predict(ridge_train5.5, input_test5_ivs)

ridge_rmse_test5.5 <- RMSE(ridge_predict_test5.5, input_test5$alpha5)

ridge_rmse_test5.5

lasso_predict_train5.5 <- predict(lasso_train5.5, x5.5)

lasso_rmse_train5.5 <- RMSE(lasso_predict_train5.5, y)

lasso_rmse_train5.5

lasso_predict_test5.5 <- predict(lasso_train5.5, input_test5_ivs)

lasso_rmse_test5.5 <- RMSE(lasso_predict_test5.5,input_test5$alpha5)

lasso_rmse_test5.5


vip(ridge_train5.5)
vip(lasso_train5.5, bar=FALSE)










#############################

# 5 day prediction 7 day data

input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi, adx7, adxr7, 
                              natr7, atr7,ad7, obv,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input5.7 <- as.data.frame(cbind(alpha5, std_input7))



# Data Split

final_input5.7_cv <- final_input5.7[-c(1:24),]


row_index <- sample(nrow(final_input5.7_cv), size = 0.7*nrow(final_input5.7_cv),replace = F) 

input_train7 <- final_input5.7_cv[row_index,]

input_test7 <- final_input5.7_cv[-row_index,]

x5.7 <- model.matrix(alpha5 ~ . , data = input_train7)[,-1]

y <- input_train7$alpha5


ridge_train5.7 <- cv.glmnet(
  
  x = x5.7,
  y = y,
  alpha = 0
)


lasso_train5.7 <- cv.glmnet(
  
  x = x5.7,
  
  y = y ,
  
  alpha = 1
  
)


input_test7_ivs <- as.matrix(input_test7[,-1])


ridge_predict_train5.7 <- predict(ridge_train5.7, x5.5)

ridge_rmse_train5.7 <- RMSE(ridge_predict_train5.7, y) 

ridge_rmse_train5.7

ridge_predict_test5.7 <- predict(ridge_train5.7, input_test7_ivs)

ridge_rmse_test5.7 <- RMSE(ridge_predict_test5.7, input_test7$alpha5)

ridge_rmse_test5.7

lasso_predict_train5.7 <- predict(lasso_train5.7, x5.7)

lasso_rmse_train5.7 <- RMSE(lasso_predict_train5.7, y)

lasso_rmse_train5.7

lasso_predict_test5.7 <- predict(lasso_train5.7, input_test7_ivs)

lasso_rmse_test5.7 <- RMSE(lasso_predict_test5.7,input_test7$alpha5)

lasso_rmse_test5.7


vip(ridge_train5.7)
vip(lasso_train5.7, bar=FALSE)

#================================================================
  
# Buy Signal Prediction

# standard formulae


file.create("std alpha return.csv")

alpha_ret1 <- 0

for(i in 1:length(std_log_alpha1$fitted.values)){
  
  alpha_ret1[i] <- if(std_log_alpha1$fitted.values[i] >= 0.5)1 else 0
  
}


#  3 day prediction

alpha_ret3 <- 0

for(i in 1:length(std_log_alpha3$fitted.values)){
  
  alpha_ret3[i] <- if(std_log_alpha3$fitted.values[i] >= 0.5)1 else 0
  
}


# 5 day prediction

alpha_ret5 <- 0

for(i in 1:length(std_log_alpha5$fitted.values)){
  
  alpha_ret5[i] <- if(std_log_alpha5$fitted.values[i] >= 0.5)1 else 0
  
}


alpha_return <- cbind(alpha_ret1, alpha_ret3, alpha_ret5) 

write.csv(alpha_return, "std alpha return.csv")







################################################


alpha_buy1.3 <- 0

for(i in 1:length(log_model_1.3$fitted.values)){
  
  alpha_buy1.3[i] <- if(log_model_1.3$fitted.values[i] >= 0.5) 1 else 0
}


alpha_buy1.5 <- 0

for(i in 1:length(log_model_1.5$fitted.values)){
  
  alpha_buy1.5[i] <- if(log_model_1.5$fitted.values[i] >= 0.5) 1 else 0
}

alpha_buy1.7 <- 0

for(i in 1:length(log_model_1.7$fitted.values)){
  
  alpha_buy1.7[i] <- if(log_model_1.7$fitted.values[i] >= 0.5) 1 else 0
}



#########################################################






alpha_buy3.3 <- 0

for(i in 1:length(log_model_3.3$fitted.values)){
  
  alpha_buy3.3[i] <- if(log_model_3.3$fitted.values[i] >= 0.5) 1 else 0
}

alpha_buy3.5 <- 0

for(i in 1:length(log_model_3.5$fitted.values)){
  
  alpha_buy3.5[i] <- if(log_model_3.5$fitted.values[i] >= 0.5) 1 else 0
}

alpha_buy3.7 <- 0

for(i in 1:length(log_model_3.7$fitted.values)){
  
  alpha_buy3.7[i] <- if(log_model_3.7$fitted.values[i] >= 0.5) 1 else 0
}

############################################



alpha_buy5.3 <- 0

for(i in 1:length(log_model_5.3$fitted.values)){
  
  alpha_buy5.3[i] <- if(log_model_5.3$fitted.values[i] >= 0.5) 1 else 0
}

alpha_buy5.5 <- 0

for(i in 1:length(log_model_5.5$fitted.values)){
  
  alpha_buy5.5[i] <- if(log_model_5.5$fitted.values[i] >= 0.5) 1 else 0
}


alpha_buy5.7 <- 0

for(i in 1:length(log_model5.7$fitted.values)){
  
  alpha_buy5.7[i] <- if(log_model5.7$fitted.values[i] >= 0.5) 1 else 0
}



alpha_buy <- as.data.frame(cbind(alpha_buy1.3, alpha_buy1.5, alpha_buy1.7,
                                 alpha_buy3.3, alpha_buy3.5,alpha_buy3.7,
                                 alpha_buy5.3, alpha_buy5.5, alpha_buy5.7, alpha50$Close ))


file.create("alpha return.csv")

write.csv(alpha_buy, "alpha return.csv")

alpha_return <- read.csv("alpha return.csv")

#################################################

# Return estimation post factoring in Nifty Movement



alpha5 <- 0


for(i in 1:(nrow(alpha50)-5)){
  
  alpha5[i] <- log(alpha50$Close[i+5] / alpha50$Close[i])
  
  alpha5 <- na.omit(alpha5)
  
}

alpha5.5 <- 0

for(i in 1:length(alpha5)){
  
  alpha5.5[i] <- if (alpha5[i] >= 0) 1 else 0
  
}


nifty5 <- 0

for(i in 1:(nrow(nifty)-5)){
  
  nifty5[i] <- log(nifty$Close[i+5]/nifty$Close[i])
  
  
}



input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, adx5, adxr5, mfi5, obv5,
                       nifty5,natr5, atr5,ad5,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]





final_input5.5<- as.data.frame(cbind(alpha5.5, std_input5))

log_model_5.5 <- glm(alpha5.5~., family = "binomial", data = final_input5.5)

alpha_nifty_buy5.5 <- 0

for(i in 1:length(log_model_5.5$fitted.values)){
  
  alpha_nifty_buy5.5[i] <- if(log_model_5.5$fitted.values[i]>=0.5) 1 else 0
}

file.create("alpha nifty buy.csv")
write.csv(alpha_nifty_buy5.5, "alpha nifty buy.csv")
