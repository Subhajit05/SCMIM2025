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






setwd("E:/Pub/direction change/new data")

nifty <- read.csv("NIFTY50.csv")
# Volume of 16/02/2024 is missing , so this data has been deleted and 


quality <- read.csv("QUALITY 30.csv")

hlc <- cbind(quality$High, quality$Low, quality$Close)
vix <- read.csv("vix.csv")
vix_close <- vix$Close

hyper_grid <- expand.grid(
  
  degree = 1:3,
  nprune = seq(2,100, length.out = 10) %>% floor()
  
  
)

#################################################


# Application standard formulae


quality1 <- 0


for(i in 1:(nrow(quality)-1)){
  
  quality1[i] <- log(quality$Close[i+1] / quality$Close[i])
  
  quality1 <- na.omit(quality1)
  
}


quality1.1 <- 0

for(i in 1:length(quality1)){
  
  quality1.1 [i] <- if(quality1[i] >= 0) 1 else 0
  
}


quality3 <- 0


for(i in 1:(nrow(quality)-3)){
  
  quality3[i] <- log(quality$Close[i+3] / quality$Close[i])
  
  quality3 <- na.omit(quality3)
  
}


quality3.1 <- 0

for(i in 1:length(quality3)){
  
  quality3.1 [i] <- if(quality3[i] >= 0) 1 else 0
  
}

quality5 <- 0


for(i in 1:(nrow(quality)-5)){
  
  quality5[i] <- log(quality$Close[i+5] / quality$Close[i])
  
  quality5 <- na.omit(quality5)
  
}


quality5.1 <- 0

for(i in 1:length(quality5)){
  
  quality5.1 [i] <- if(quality5[i] >= 0) 1 else 0
  
}



################################################################

# Standard formulae of technical indicators

# Accumulation Distribution

ad <- williamsAD(HLC = hlc)


# Average True Range

main_atr <- ATR(HLC = hlc, n = 14)

atr <- main_atr[,2]


natr <- (atr/quality$Close)*100

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

rsi <- RSI(quality$Close, n = 14)

# t3

t3 <- EMA(EMA(EMA(quality$Close, n = 14), n = 14), n= 14)


# William's R

wr <- WPR(HLC = hlc , n = 14)


# OBV 

obv <- OBV(quality$Close, quality$Volume)


# MFI

mfi <- MFI(HLC = hlc, volume = quality$Volume, n= 14)


# Stochastics


main_stoch <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 3 , nSlowD = 3))

fastK<- main_stoch$fastK

fastD <- main_stoch$fastD

slowD <- main_stoch$slowD


main_input <- as.data.frame(cbind(ad, atr, natr, adx, adxr ,bb_up , rsi,t3, wr,
                                obv, mfi, fastK, fastD, slowD ,vix_close))

std_input <- scale(main_input, center = T, scale = T)

std_input <- std_input[-1,]


final_input1 <- as.data.frame(cbind(quality1.1, std_input))


final_input1 <- final_input1[-c(1:39),]



std_log_quality1 <- glm(quality1.1 ~ ., family = "binomial", data = final_input1)

summary(std_log_quality1)


final_input3 <- as.data.frame(cbind(quality3.1, std_input))

final_input3 <- final_input3[-c(1:39),]

std_log_quality3 <- glm(quality3.1 ~ ., family = "binomial", data = final_input3)

summary(std_log_quality3)


final_input5 <- as.data.frame(cbind(quality5.1, std_input))

final_input5 <- final_input5[-c(1:39),]

std_log_quality5 <- glm(quality5.1 ~ ., family = "binomial", data = final_input5)

summary(std_log_quality5)










##########################################

# TI (3 day data)


ad3 <- williamsAD(HLC = hlc)



# Average True Range

main_atr3 <- ATR(HLC = hlc, n = 3)

atr3 <- main_atr3[,2] 



# Normalized Average True Range (NATR) 

natr3 <- (atr3/quality$Close)*100


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



mfi3 <- MFI(HLC = hlc, volume = quality$Volume, n = 3)



# On Balance Volume

obv3 <- OBV(price = quality$Close,volume = quality$Volume)



#Triple Exponential Moving Average (T3)

t3 <- EMA(EMA(EMA(quality$Close,n=3),3),n=3)

# Williams R

wr3 <- WPR(HLC = hlc, n= 3)

# RSI

rsi3 <- RSI(quality$Close,n=3)



# Stochastic slow & fast 

stochKD3 <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 3, nSlowD = 3))

fastK3<- stochKD3$fastK

fastD3 <- stochKD3$fastD

slowD3 <- stochKD3$slowD


main_input <- as.data.frame(cbind(ad3, atr3, natr3, adx, adxr3 ,bb_up3 , rsi3,t3, wr3,
                              mfi3, obv3, fastK3, fastD3, slowD3 ,vix_close))

std_input <- scale(main_input, center = T, scale = T)

std_input <- std_input[-1,]


final_input1 <- as.data.frame(cbind(quality1.1, std_input))


final_input1 <- final_input1[-c(1:39),]



log_model1.3 <- glm(quality1.1 ~ ., family = "binomial", data = final_input1)

summary(std_log_quality1.3)

#########################################

# TI 5 day data


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



mfi5 <- MFI(HLC = hlc, volume = quality$Volume, n = 5)



# On Balance Volume

 obv5 <- OBV(quality$Close, quality$Volume)



#Triple Exponential Moving Average (T3)

t5 <- EMA(EMA(EMA(quality$Close,n=5),5),n=5)

# Williams R

wr5 <- WPR(HLC = hlc, n= 5)

# RSI

rsi5 <- RSI(quality$Close,n=5)


# OBV

obv5 <- OBV(quality$Close, quality$Volume)




# Stochastic slow & fast 

stochKD5 <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 5, nSlowD = 5))

fastK5 <- stochKD5$fastK

fastD5 <- stochKD5$fastD

slowD5 <- stochKD5$slowD

####################################

# TI 7 day data

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



mfi7 <- MFI(HLC = hlc, volume = quality$Volume, n=7)



# On Balance Volume

obv7 <- OBV(quality$Close, quality$Volume)



#Triple Exponential Moving Average (T3)

t7 <- EMA(EMA(EMA(quality$Close,n=7),7),n=7)

# Williams R

wr7 <- WPR(HLC = hlc, n= 7)

# RSI

rsi7 <- RSI(quality$Close,n=7)

# Stochastic slow & fast 

stochKD7 <- as.data.frame(stoch(HLC = hlc, nFastK = 14, nFastD = 7, nSlowD = 7))

fastK7 <- stochKD7$fastK

fastD7 <- stochKD7$fastD

slowD7 <- stochKD7$slowD

##################################################
# 1 day ahead prediction 3 day data


quality1 <- 0

for(i in 1:(nrow(quality)-1)){
  
  quality1[i] <- log(quality$Close[i+1]/quality$Close[i])
  
  quality1 <- na.omit(quality1)}

quality1.3 <- 0

for(i in 1:length(quality1)){
  
  quality1.3[i] <- if(quality1[i] >= 0) 1 else 0
}


input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, adx, adxr3, mfi3, obv3,
                              natr3, atr3,ad3,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input1.3 <- as.data.frame(cbind(quality1.3, std_input3))


log_model1.3 <- glm(quality1.3 ~., family = "binomial", data = final_input1.3)

summary(log_model1.3)

# Data split
final_input1.3_cv <- final_input1.3[-c(1:16),]

row_index <- sample(nrow(final_input1.3_cv), size = 0.7*nrow(final_input1.3_cv),replace = F) 

input_train3 <- final_input1.3_cv[row_index,]

input_test3 <- final_input1.3_cv[-row_index,]

cv_log_model1.3 <- train(as.factor(quality1.3)~ .,
                         data = input_train3,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model1.3 <- predict(cv_log_model1.3, input_train3) 

cm1.3_train <- confusionMatrix(as.factor(pred_log_model1.3), as.factor(input_train3$quality1.3))

cm1.3_train


pred_log_model1.3_test <- predict(cv_log_model1.3, input_test3)

cm1.3_test <- confusionMatrix(as.factor(pred_log_model1.3_test), as.factor(input_test3$quality1.3))

cm1.3_test

# Ridge

x1.3 <- model.matrix(quality1 ~ . , data = input_train3)[,-1]

y <- input_train3$quality1


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

ridge_rmse_test1.3 <- RMSE(ridge_predict_test1.3, input_test3$quality1)

ridge_rmse_test1.3

lasso_predict_train1.3 <- predict(lasso_train1.3, x1.3)

lasso_rmse_train1.3 <- RMSE(lasso_predict_train1.3, y)

lasso_rmse_train1.3

lasso_predict_test1.3 <- predict(lasso_train1.3, input_test3_ivs)

lasso_rmse_test1.3 <- RMSE(lasso_predict_test1.3,input_test3$quality1)

lasso_rmse_test1.3


vip(ridge_train1.3)
vip(lasso_train1.3, bar=FALSE)


##########################
# Spline 

cv_mars_1.3 <- train(
  
  x = subset(input_train3, select = -quality1),   
  
  y =  input_train3$quality1,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_1.3, input_train3)

ar_rmse_train1 <- RMSE(ar_pred_train, input_train3$quality1)

ar_rmse_train1


ar_pred_test <- predict(cv_mars_1.3, input_test3)

ar_rmse_test1 <- RMSE(ar_pred_test, input_test3$quality1)

ar_rmse_test1

cv_mars_1.3$bestTune

vip(cv_mars_1.3, bar = FALSE)






###############################


# 1 day ahead prediction 5 day data


quality1.5 <- 0

for(i in 1:length(quality1)){
  
  quality1.5[i] <- if(quality1[i] >= 0) 1 else 0
}

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, adx5, adxr5, mfi5, obv5,
                              natr5, atr5,ad5,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input1.5 <- as.data.frame(cbind(quality1.5, std_input5))

log_model1.5 <- glm(quality1.5 ~. , family = "binomial", data = final_input1.5)


summary(log_model1.5)


# Data Split

final_input1.5_cv <- final_input1.5[-c(1:20),]


row_index <- sample(nrow(final_input1.5_cv), size = 0.7*nrow(final_input1.5_cv),replace = F) 

input_train5 <- final_input1.5_cv[row_index,]

input_test5 <- final_input1.5_cv[-row_index,]


cv_log_model1.5 <- train(as.factor(quality1.5)~ .,
                         data = input_train5,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model1.5 <- predict(cv_log_model1.5, input_train5) 

cm1.5_train <- confusionMatrix(as.factor(pred_log_model1.5), as.factor(input_train5$quality1.5))

cm1.5_train

pred_log_model1.5_test <- predict(cv_log_model1.5, input_test5)

cm1.5_test <- confusionMatrix(as.factor(pred_log_model1.5_test), as.factor(input_test5$quality1.5))

cm1.5_test



# Ridge


x1.5 <- model.matrix(quality1 ~ . , data = input_train5)[,-1]

y <- input_train5$quality1


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

ridge_rmse_test1.5 <- RMSE(ridge_predict_test1.5, input_test5$quality1)

ridge_rmse_test1.5

lasso_predict_train1.5 <- predict(lasso_train1.5, x1.5)

lasso_rmse_train1.5 <- RMSE(lasso_predict_train1.5, y)

lasso_rmse_train1.5

lasso_predict_test1.5 <- predict(lasso_train1.5, input_test5_ivs)

lasso_rmse_test1.5 <- RMSE(lasso_predict_test1.5,input_test5$quality1)

lasso_rmse_test1.5


vip(ridge_train1.5)
vip(lasso_train1.5, bar=FALSE)

#####################
# Spline


cv_mars_1.5 <- train(
  
  x = subset(input_train5, select = -quality1),   
  
  y =  input_train5$quality1,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_1.5, input_train5)

ar_rmse_train1.5 <- RMSE(ar_pred_train, input_train5$quality1)

ar_rmse_train1.5


ar_pred_test <- predict(cv_mars_1.5, input_test5)

ar_rmse_test1.5 <- RMSE(ar_pred_test, input_test5$quality1)

ar_rmse_test1.5

cv_mars_1.5$bestTune

vip(cv_mars_1.5, bar = FALSE)



#########################################

# 1 day ahead prediction 7 day data


quality1.7 <- 0

for(i in 1:length(quality1)){
  
  quality1.7[i] <- if(quality1[i] >= 0 ) 1 else 0
}


input7 <- as.data.frame(cbind(ad7,slowD7,fastD7, fastK7, rsi7,  
                              adx7, adxr7, mfi7, obv7,
                              natr7, atr7, bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input1.7 <- as.data.frame(cbind(quality1.7, std_input7))


log_model1.7 <- glm(quality1.7 ~., family = "binomial", data = final_input1.7)

summary(log_model1.7)


# Data Split

final_input1.7_cv <- final_input1.7[-c(1:24),]


row_index <- sample(nrow(final_input1.7_cv), size = 0.7*nrow(final_input1.7_cv),replace = F) 

input_train7 <- final_input1.7_cv[row_index,]

input_test7 <- final_input1.7_cv[-row_index,]


input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7, mfi7, adx7, adxr7, 
                              natr7, atr7,ad7, obv7,bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input1.7 <- as.data.frame(cbind(quality1.7, std_input7))



# Data Split

final_input1.7_cv <- final_input1.7[-c(1:24),]


row_index <- sample(nrow(final_input1.7_cv), size = 0.7*nrow(final_input1.7_cv),replace = F) 

input_train7 <- final_input1.7_cv[row_index,]

input_test7 <- final_input1.7_cv[-row_index,]

cv_log_model1.7 <- train(as.factor(quality1.7)~ .,
                         data = input_train7,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model1.7 <- predict(cv_log_model1.7, input_train7) 

cm1.7_train <- confusionMatrix(as.factor(pred_log_model1.7), as.factor(input_train7$quality1.7))

cm1.7_train


pred_log_model1.7_test <- predict(cv_log_model1.7, input_test7)

cm1.7_test <- confusionMatrix(as.factor(pred_log_model1.7_test), as.factor(input_test7$quality1.7))

cm1.7_test



# Ridge

x1.7 <- model.matrix(quality1 ~ . , data = input_train7)[,-1]

y <- input_train7$quality1


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

ridge_rmse_test1.7 <- RMSE(ridge_predict_test1.7, input_test7$quality1)

ridge_rmse_test1.7

lasso_predict_train1.7 <- predict(lasso_train1.7, x1.7)

lasso_rmse_train1.7 <- RMSE(lasso_predict_train1.7, y)

lasso_rmse_train1.7

lasso_predict_test1.7 <- predict(lasso_train1.7, input_test7_ivs)

lasso_rmse_test1.7 <- RMSE(lasso_predict_test1.7,input_test7$quality1)

lasso_rmse_test1.7


vip(ridge_train1.7)
vip(lasso_train1.7, bar=FALSE)
#################################################################
# Spline

cv_mars_1.7 <- train(
  
  x = subset(input_train7, select = -quality1),   
  
  y =  input_train7$quality1,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_1.7, input_train7)

ar_rmse_train1.7 <- RMSE(ar_pred_train, input_train7$quality1)

ar_rmse_train1.7


ar_pred_test <- predict(cv_mars_1.7, input_test7)

ar_rmse_test1.7 <- RMSE(ar_pred_test, input_test7$quality1)

ar_rmse_test1.7

cv_mars_1.7$bestTune

vip(cv_mars_1.7, bar = FALSE)

###############################################################

# 3 day prediction with 3 day data


quality3.3 <- 0

for(i in 1:length(quality3)){
  
  quality3.3[i] <- if(quality3[i] >= 0) 1 else 0
}

input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3,  adx, adxr3, mfi3, obv3,
                              natr3, atr3,ad3, bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input3.3 <- as.data.frame(cbind(quality3.3, std_input3))

log_model3.3 <- glm(quality3.3 ~., family = "binomial" , data = final_input3.3)

summary(log_model3.3)


# Data split

final_input3.3_cv <- final_input3.3[-c(1:16),]


row_index <- sample(nrow(final_input3.3_cv), size = 0.7*nrow(final_input3.3_cv),replace = F) 

input_train3 <- final_input3.3_cv[row_index,]

input_test3 <- final_input3.3_cv[-row_index,]


cv_log_model3.3 <- train(as.factor(quality3.3)~ .,
                         data = input_train3,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model3.3 <- predict(cv_log_model3.3, input_train3) 

cm3.3_train <- confusionMatrix(as.factor(pred_log_model3.3), as.factor(input_train3$quality3.3))

cm3.3_train

pred_log_model3.3_test <- predict(cv_log_model3.3, input_test3)

cm3.3_test <- confusionMatrix(as.factor(pred_log_model3.3_test), as.factor(input_test3$quality3.3))

cm3.3_test



# Ridge



x3.3 <- model.matrix(quality3 ~ . , data = input_train3)[,-1]

y <- input_train3$quality3


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

ridge_rmse_test3.3 <- RMSE(ridge_predict_test3.3, input_test3$quality3)

ridge_rmse_test3.3

lasso_predict_train3.3 <- predict(lasso_train3.3, x3.3)

lasso_rmse_train3.3 <- RMSE(lasso_predict_train3.3, y)

lasso_rmse_train3.3

lasso_predict_test3.3 <- predict(lasso_train3.3, input_test3_ivs)

lasso_rmse_test3.3 <- RMSE(lasso_predict_test3.3,input_test3$quality3)

lasso_rmse_test3.3


vip(ridge_train3.3)
vip(lasso_train3.3, bar=FALSE)

####################
# Spline

cv_mars_3.3 <- train(
  
  x = subset(input_train3, select = -quality3),   
  
  y =  input_train3$quality3,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_3.3, input_train3)

ar_rmse_train3.3 <- RMSE(ar_pred_train, input_train3$quality3)

ar_rmse_train3.3


ar_pred_test <- predict(cv_mars_3.3, input_test3)

ar_rmse_test3.3 <- RMSE(ar_pred_test, input_test3$quality3)

ar_rmse_test3.3

cv_mars_3.3$bestTune

vip(cv_mars_3.3, bar = FALSE)




##############################################################

# 3 day prediction with 5 day data

quality3.5 <- 0

for(i in 1:length(quality3)){
  
  quality3.5[i] <- if(quality3[i] >= 0 )1 else 0
  
}


input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5, adx5, adxr5, mfi5, obv5,
                              natr5, atr5,ad5,bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input3.5<- as.data.frame(cbind(quality3.5, std_input5))

log_model3.5 <- glm(quality3.5 ~., family = "binomial" , data = final_input3.5)

summary(log_model3.5)




# data split

final_input3.5_cv <- final_input3.5[-c(1:20),]

row_index <- sample(nrow(final_input3.5_cv), size = 0.7*nrow(final_input3.5_cv),replace = F) 

input_train5 <- final_input3.5_cv[row_index,]

input_test5 <- final_input3.5_cv[-row_index,]


cv_log_model3.5 <- train(as.factor(quality3.5)~ .,
                         data = input_train5,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model3.5 <- predict(cv_log_model3.5, input_train5) 

cm1.5_train <- confusionMatrix(as.factor(pred_log_model3.5), as.factor(input_train5$quality3.5))

cm1.5_train

pred_log_model3.5_test <- predict(cv_log_model3.5, input_test5)

cm3.5_test <- confusionMatrix(as.factor(pred_log_model3.5_test), as.factor(input_test5$quality3.5))

cm3.5_test


# Ridge

x3.5 <- model.matrix(quality3 ~ . , data = input_train5)[,-1]

y <- input_train5$quality3


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

ridge_rmse_test3.5 <- RMSE(ridge_predict_test3.5, input_test5$quality3)

ridge_rmse_test3.5

lasso_predict_train3.5 <- predict(lasso_train3.5, x3.5)

lasso_rmse_train3.5 <- RMSE(lasso_predict_train3.5, y)

lasso_rmse_train3.5

lasso_predict_test3.5 <- predict(lasso_train3.5, input_test5_ivs)

lasso_rmse_test3.5 <- RMSE(lasso_predict_test3.5,input_test5$quality3)

lasso_rmse_test3.5


vip(ridge_train3.5)
vip(lasso_train3.5, bar=FALSE)

###########################
# Spline


cv_mars_3.5 <- train(
  
  x = subset(input_train5, select = -quality3),   
  
  y =  input_train5$quality3,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_3.5, input_train5)

ar_rmse_train3.5 <- RMSE(ar_pred_train, input_train5$quality3)

ar_rmse_train3.5


ar_pred_test <- predict(cv_mars_3.5, input_test5)

ar_rmse_test3.5 <- RMSE(ar_pred_test, input_test5$quality3)

ar_rmse_test3.5

cv_mars_3.5$bestTune

vip(cv_mars_3.5, bar = FALSE)






#############################################

# 3 day prediction with 7 day data


quality3.7 <- 0

for(i in 1:length(quality3)){
  
  quality3.7[i] <- if(quality3[i]>=0)1 else 0
}




input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7,adx7, adxr7, mfi7, obv7,
                              natr7, atr7,ad7, bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input3.7 <- as.data.frame(cbind(quality3.7, std_input7))


log_model3.7 <- glm(quality3.7 ~ . , family = "binomial", data = final_input3.7)

summary(log_model3.7)

# Data Split

final_input3.7_cv <- final_input3.7[-c(1:24),]


row_index <- sample(nrow(final_input3.7_cv), size = 0.7*nrow(final_input3.7_cv),replace = F) 

input_train7 <- final_input3.7_cv[row_index,]

input_test7 <- final_input3.7_cv[-row_index,]

cv_log_model3.7 <- train(as.factor(quality3.7)~ .,
                         data = input_train7,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))

pred_log_model3.7 <- predict(cv_log_model3.7, input_train7) 

cm3.7_train <- confusionMatrix(as.factor(pred_log_model3.7), as.factor(input_train7$quality3.7))

cm3.7_train


pred_log_model3.7_test <- predict(cv_log_model3.7, input_test7)

cm3.7_test <- confusionMatrix(as.factor(pred_log_model3.7_test), as.factor(input_test7$quality3.7))

cm3.7_test


# Ridge

x3.7 <- model.matrix(quality3 ~ . , data = input_train7)[,-1]

y <- input_train7$quality3


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

ridge_rmse_test3.7 <- RMSE(ridge_predict_test3.7, input_test7$quality3)

ridge_rmse_test3.7

lasso_predict_train3.7 <- predict(lasso_train3.7, x3.7)

lasso_rmse_train3.7 <- RMSE(lasso_predict_train3.7, y)

lasso_rmse_train3.7

lasso_predict_test3.7 <- predict(lasso_train3.7, input_test7_ivs)

lasso_rmse_test3.7 <- RMSE(lasso_predict_test3.7,input_test7$quality3)

lasso_rmse_test3.7


vip(ridge_train3.7)
vip(lasso_train3.7, bar=FALSE)




############################################

# spline

cv_mars_3.7 <- train(
  
  x = subset(input_train7, select = -quality3),   
  
  y =  input_train7$quality3,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_3.7, input_train7)

ar_rmse_train3.7 <- RMSE(ar_pred_train, input_train7$quality3)

ar_rmse_train3.7


ar_pred_test <- predict(cv_mars_3.7, input_test7)

ar_rmse_test3.7 <- RMSE(ar_pred_test, input_test7$quality3)

ar_rmse_test3.7

cv_mars_3.7$bestTune

vip(cv_mars_3.7, bar = FALSE)

#################################################

# 5 day prediction with 3 day data

quality5.3 <- 0

for(i in 1:length(quality5)){
  
  quality5.3[i] <- if(quality5[i] >= 0) 1 else 0
}



input3 <- as.data.frame(cbind(slowD3,fastD3, fastK3, rsi3, adx, adxr3, mfi3, obv3,
                              natr3, atr3,ad3,bb_up3, t3, wr3, vix_close = vix$Close))

std_input3 <- scale(input3, center = T, scale = T)

std_input3 <- std_input3[-1,]

final_input5.3 <- as.data.frame(cbind(quality5.3, std_input3))

log_model5.3 <- glm(quality5.3 ~. , family = "binomial", data = final_input5.3)


summary(log_model5.3)


# Data split

final_input5.3_cv <- final_input5.3[-c(1:16),]


row_index <- sample(nrow(final_input5.3_cv), size = 0.7*nrow(final_input5.3_cv),replace = F) 

input_train3 <- final_input5.3_cv[row_index,]

input_test3 <- final_input5.3_cv[-row_index,]

cv_log_model5.3 <- train(as.factor(quality5.3)~ .,
                         data = input_train3,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model5.3 <- predict(cv_log_model5.3, input_train3) 

cm5.3_train <- confusionMatrix(as.factor(pred_log_model5.3), as.factor(input_train3$quality5.3))

cm5.3_train

pred_log_model5.3_test <- predict(cv_log_model5.3, input_test3)

cm5.3_test <- confusionMatrix(as.factor(pred_log_model5.3_test), as.factor(input_test3$quality5.3))

cm5.3_test


# Ridge



x5.3 <- model.matrix(quality5 ~ . , data = input_train3)[,-1]

y <- input_train3$quality5


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

ridge_rmse_test5.3 <- RMSE(ridge_predict_test5.3, input_test3$quality5)

ridge_rmse_test5.3

lasso_predict_train5.3 <- predict(lasso_train5.3, x5.3)

lasso_rmse_train5.3 <- RMSE(lasso_predict_train5.3, y)

lasso_rmse_train5.3

lasso_predict_test5.3 <- predict(lasso_train5.3, input_test3_ivs)

lasso_rmse_test5.3 <- RMSE(lasso_predict_test5.3,input_test3$quality5)

lasso_rmse_test5.3


vip(ridge_train5.3)
vip(lasso_train5.3, bar=FALSE)


################################

# Spline

cv_mars_5.3 <- train(
  
  x = subset(input_train3, select = -quality5),   
  
  y =  input_train3$quality5,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_5.3, input_train3)

ar_rmse_train5.3 <- RMSE(ar_pred_train, input_train3$quality5)

ar_rmse_train5.3


ar_pred_test <- predict(cv_mars_5.3, input_test3)

ar_rmse_test5.3 <- RMSE(ar_pred_test, input_test3$quality5)

ar_rmse_test5.3

cv_mars_5.3$bestTune

vip(cv_mars_5.3, bar = FALSE)






############################################

# 5 day prediction with 5 day data

quality5.5 <- 0

for(i in 1:length(quality5)){
  
  quality5.5[i] <- if(quality5[i] >=0 )1 else 0
}

input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5,  adx5, adxr5, mfi5, obv5,
                              natr5, atr5,ad5, bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input5.5 <- as.data.frame(cbind(quality5.5, std_input5))

log_model5.5 <- glm(quality5.5 ~ . , family = "binomial" , data = final_input5.5)

summary(log_model5.5)




# data split

final_input5.5_cv <- final_input5.5[-c(1:20),]


row_index <- sample(nrow(final_input5.5_cv), size = 0.7*nrow(final_input5.5_cv),replace = F) 

input_train5 <- final_input5.5_cv[row_index,]

input_test5 <- final_input5.5_cv[-row_index,]


cv_log_model5.5 <- train(as.factor(quality5.5)~ .,
                         data = input_train5,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))


pred_log_model5.5 <- predict(cv_log_model5.5, input_train5) 

cm5.5_train <- confusionMatrix(as.factor(pred_log_model5.5), as.factor(input_train5$quality5.5))

cm5.5_train

pred_log_model5.5_test <- predict(cv_log_model5.5, input_test5)

cm5.5_test <- confusionMatrix(as.factor(pred_log_model5.5_test), as.factor(input_test5$quality5.5))

cm5.5_test



# Ridge

x5.5 <- model.matrix(quality5 ~ . , data = input_train5)[,-1]

y <- input_train5$quality5


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

ridge_rmse_test5.5 <- RMSE(ridge_predict_test5.5, input_test5$quality5)

ridge_rmse_test5.5

lasso_predict_train5.5 <- predict(lasso_train5.5, x5.5)

lasso_rmse_train5.5 <- RMSE(lasso_predict_train5.5, y)

lasso_rmse_train5.5

lasso_predict_test5 <- predict(lasso_train5.5, input_test5_ivs)

lasso_rmse_test5.5 <- RMSE(lasso_predict_test5,input_test5$quality5)

lasso_rmse_test5.5


vip(ridge_train5.5)
vip(lasso_train5.5, bar=FALSE)

#######################################################################

#Spline

cv_mars_5.5 <- train(
  
  x = subset(input_train5, select = -quality5),   
  
  y =  input_train5$quality5,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_5.5, input_train5)

ar_rmse_train5.5 <- RMSE(ar_pred_train, input_train5$quality5)

ar_rmse_train5.5


ar_pred_test <- predict(cv_mars_5.5, input_test5)

ar_rmse_test5.5 <- RMSE(ar_pred_test, input_test5$quality5)

ar_rmse_test5.5

cv_mars_5.5$bestTune

vip(cv_mars_5.5, bar = FALSE)

##############################################################

# 5 day prediction with 7 day data

quality5.7 <- 0

for(i in 1:length(quality5)){
  
  quality5.7[i] <- if(quality5[i] >= 0 ) 1 else 0
  
}


input7 <- as.data.frame(cbind(slowD7,fastD7, fastK7, rsi7,  adx7, adxr7, mfi7, obv7,
                              natr7, atr7,ad7, bb_up7, t7, wr7, vix_close = vix$Close))

std_input7 <- scale(input7, center = T, scale = T)

std_input7 <- std_input7[-1,]

final_input5.7 <- as.data.frame(cbind(quality5.7, std_input7))


log_model5.7 <- glm(quality5.7 ~ . , family = "binomial", data = final_input5.7)

summary(log_model5.7)

# Data Split

final_input5.7_cv <- final_input5.7[-c(1:24),]


row_index <- sample(nrow(final_input5.7_cv), size = 0.7*nrow(final_input5.7_cv),replace = F) 

input_train7 <- final_input5.7_cv[row_index,]

input_test7 <- final_input5.7_cv[-row_index,]


cv_log_model5.7 <- train(as.factor(quality5.7)~ .,
                         data = input_train7,
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 10))

pred_log_model5.7 <- predict(cv_log_model5.7, input_train7) 

cm5.7_train <- confusionMatrix(as.factor(pred_log_model5.7), as.factor(input_train7$quality5.7))

cm5.7_train


pred_log_model5.7_test <- predict(cv_log_model5.7, input_test7)

cm5.7_test <- confusionMatrix(as.factor(pred_log_model5.7_test), as.factor(input_test7$quality5.7))

cm5.7_test



# Ridge



x5.7 <- model.matrix(quality5 ~ . , data = input_train7)[,-1]

y <- input_train7$quality5


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


ridge_predict_train5.7 <- predict(ridge_train5.7, x5.7)

ridge_rmse_train5.7 <- RMSE(ridge_predict_train5.7, y) 

ridge_rmse_train5.7

ridge_predict_test5.7 <- predict(ridge_train5.7, input_test7_ivs)

ridge_rmse_test5.7 <- RMSE(ridge_predict_test5.7, input_test7$quality5)

ridge_rmse_test5.7

lasso_predict_train5.7 <- predict(lasso_train5.7, x5.7)

lasso_rmse_train5.7 <- RMSE(lasso_predict_train5.7, y)

lasso_rmse_train5.7

lasso_predict_test5.7 <- predict(lasso_train5.7, input_test7_ivs)

lasso_rmse_test5.7 <- RMSE(lasso_predict_test5.7,input_test7$quality5)

lasso_rmse_test5.7


vip(ridge_train5.7)
vip(lasso_train5.7, bar=FALSE)

#################################################

# spline


cv_mars_5.7 <- train(
  
  x = subset(input_train7, select = -quality5),   
  
  y =  input_train7$quality5,
  
  method = "earth",
  
  metric = "RMSE",
  
  trControl = trainControl(method = "cv", number = 10),
  
  tuneGrid = hyper_grid)


ar_pred_train <- predict(cv_mars_5.7, input_train7)

ar_rmse_train5.7 <- RMSE(ar_pred_train, input_train7$quality5)

ar_rmse_train5.7


ar_pred_test <- predict(cv_mars_5.7, input_test7)

ar_rmse_test5.7 <- RMSE(ar_pred_test, input_test7$quality5)

ar_rmse_test5.7

cv_mars_5.7$bestTune

vip(cv_mars_5.7, bar = FALSE)

##################################################################

# Buy Signal  prediction

########################################
# with standard formulae

# 1 day ahead prediction

quality1 <- 0

for(i in 1:length(std_log_quality1$fitted.values)){
  
  quality1[i] <- if(std_log_quality1$fitted.values[i] >= 0.5) 1 else 0
}

# 3 day ahead prediction

quality3 <- 0

for(i in 1:length(std_log_quality3$fitted.values)){
  
  quality3[i] <- if(std_log_quality3$fitted.values[i] >= 0.5) 1 else 0
}


# 5 day ahead prediction

quality5 <- 0

for(i in 1:length(std_log_quality5$fitted.values)){
  
  quality5[i] <- if(std_log_quality5$fitted.values[i] >= 0.5) 1 else 0
}


file.create(" std quality return.csv")


std_return_quality <- cbind(quality1, quality3, quality5)


write.csv(std_return_quality, "std quality return.csv")

#############################################

# 1 day ahead prediction 

quality_buy1.3 <- 0

for(i in 1:length(log_model1.3$fitted.values)){
  
  quality_buy1.3[i] <- if(log_model1.3$fitted.values[i] >= 0.5) 1 else 0
}


quality_buy1.5 <- 0

for(i in 1:length(log_model1.5$fitted.values)){
  
  quality_buy1.5[i] <- if(log_model1.5$fitted.values[i] >= 0.5) 1 else 0
}

quality_buy1.7 <- 0

for(i in 1:length(log_model1.7$fitted.values)){
  
  quality_buy1.7[i] <- if(log_model1.7$fitted.values[i] >= 0.5) 1 else 0
}



#########################################################

# 3 day ahead prediction




quality_buy3.3 <- 0

for(i in 1:length(log_model3.3$fitted.values)){
  
  quality_buy3.3[i] <- if(log_model3.3$fitted.values[i] >= 0.5) 1 else 0
}

quality_buy3.5 <- 0

for(i in 1:length(log_model3.5$fitted.values)){
  
  quality_buy3.5[i] <- if(log_model3.5$fitted.values[i] >= 0.5) 1 else 0
}

quality_buy3.7 <- 0

for(i in 1:length(log_model3.7$fitted.values)){
  
  quality_buy3.7[i] <- if(log_model3.7$fitted.values[i] >= 0.5) 1 else 0
}

############################################

# 5 day ahead prediction

quality_buy5.3 <- 0

for(i in 1:length(log_model5.3$fitted.values)){
  
  quality_buy5.3[i] <- if(log_model5.3$fitted.values[i] >= 0.5) 1 else 0
}

quality_buy5.5 <- 0

for(i in 1:length(log_model5.5$fitted.values)){
  
  quality_buy5.5[i] <- if(log_model5.5$fitted.values[i] >= 0.5) 1 else 0
}


quality_buy5.7 <- 0

for(i in 1:length(log_model5.7$fitted.values)){
  
  quality_buy5.7[i] <- if(log_model5.7$fitted.values[i] >= 0.5) 1 else 0
}



quality_buy <- as.data.frame(cbind(quality_buy1.3, quality_buy1.5, quality_buy1.7,
                                 quality_buy3.3, quality_buy3.5,quality_buy3.7,
                                 quality_buy5.3, quality_buy5.5, quality_buy5.7, quality$Close))


file.create("quality return.csv")

write.csv(quality_buy, "quality return.csv")


one_day_return <-0

for(i in 1:length(quality_buy$quality_buy1.3)){
  
  
}
################################################################

# Prediction 5 day return after post factoring Nifty



nifty5 <- 0


for(i in 1:(nrow(nifty)-5)){
  
  nifty5[i] <- log(nifty$Close[i+5]/nifty$Close[i])
}


quality5.5 <- 0

for(i in 1:length(quality5)){
  
  quality5.5[i] <- if(quality5[i] >=0 )1 else 0
}









input5 <- as.data.frame(cbind(slowD5,fastD5, fastK5, rsi5,  adx5, adxr5, mfi5, obv5,
                          nifty5, natr5, atr5,ad5, bb_up5, t5, wr5, vix_close = vix$Close))

std_input5 <- scale(input5, center = T, scale = T)

std_input5 <- std_input5[-1,]

final_input5.5 <- as.data.frame(cbind(quality5.5, std_input5))

log_model5.5 <- glm(quality5.5 ~ . , family = "binomial" , data = final_input5.5)



nifty_quality_buy5.5 <- 0

for(i in 1:length(log_model5.5$fitted.values)){
  
  nifty_quality_buy5.5[i] <- if(log_model5.5$fitted.values[i] >= 0.5) 1 else 0
}


file.create("quality nifty buy.csv")

write.csv(nifty_quality_buy5.5, "quality nifty buy.csv")