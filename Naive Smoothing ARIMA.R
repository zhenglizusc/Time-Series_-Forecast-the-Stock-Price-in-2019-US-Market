#Upload libraries
library(forecast)
library(ggplot2)


# Import data:
y=read.csv("2000_11_29_2018_11_29.csv")

# Create a time series object y.ts and starting quarter 
y.ts=ts(y$Adj.Close,start=c(2001,1),frequency =12 )
y.ts

# Create training and testing sets using function window
train1.start=c(2001,1)
train1.end=c(2011,12)

test1.start=c(2012,1)
test1.end=c(2018,11)


train2.start=c(2001,1)
train2.end=c(2017,11)

test2.start=c(2017,12)
test2.end=c(2018,11)


train1.ts=window(y.ts,start = train1.start,end=train1.end)
train2.ts=window(y.ts,start = train2.start,end=train2.end)
test1.ts=window(y.ts,start=test1.start,end=test1.end)
test2.ts=window(y.ts,start=test2.start,end=test2.end)

nTrain1=length(train1.ts)
nTrain2=length(train2.ts)
nTest1=length(test1.ts)
nTest2=length(test2.ts)


# plot data
autoplot(y.ts)+geom_point()
dev.off()


# M1 = SEASONAL NAIVE MODEL
M11=snaive(train1.ts,h=nTest1,level=95,lambda="auto")
M12=snaive(train2.ts,h=nTest2,level=95,lambda="auto")
M11
M12

# Accuracy metrics
round(accuracy(M11,test1.ts)[,c("RMSE","MAPE")],2) 
round(accuracy(M12,test2.ts)[,c("RMSE","MAPE")],2) 

# Residual diagnistics
checkresiduals(M11)
dev.off()
checkresiduals(M12)
# Residuals do not resemble white noise. There is depends/patterns that seasonal naive model 
# was unable to capture. Also there seems to be outliers and it would be interesting to see what happened 
# during quarters of extreme outlying points. 


# M2 = REGRESSION MODEL
## Provided in excel


# M3 = SMOOTHING MODEL
M31=ets(train1.ts,lambda="auto")
M32=ets(train2.ts,lambda="auto")
M31
M32

# generate forecast
M3F1=forecast(M31,h=nTest1,level=95)
M3F2=forecast(M32,h=nTest2,level=95)
M3F1
M3F2

# Accuracy metrics
round(accuracy(M3F1,test1.ts)[,c("RMSE","MAPE")],2) 
round(accuracy(M3F2,test2.ts)[,c("RMSE","MAPE")],2) 
# residual diagnostics
checkresiduals(M31)
checkresiduals(M32)
# residuals look like white noise, stationary uncorrelated(no dependence) sequency of numbers, i.e.
# our models seems to be adequate


# M4 = ARIMA MODEL
M41=auto.arima(train1.ts,lambda="auto")
M42=auto.arima(train2.ts,lambda="auto")
M41
M42

M4$fitted


# Generate forecast
M4F1=forecast(M41,h=nTest1,level=95)
M4F2=forecast(M42,h=nTest2,level=95)
M4F1
M4F2

# Accuracy metrics
round(accuracy(M4F1,test1.ts)[,c("RMSE","MAPE")],2) 
round(accuracy(M4F2,test2.ts)[,c("RMSE","MAPE")],2)

# residual diagnostics
checkresiduals(M41)
checkresiduals(M42)
# residuals look like white noise, stationary uncorrelated(no dependence) sequency of numbers, i.e.
# our models seems to be adequate

########################################## 
# M = Champion model to forecast future values
# The champion model is naive
train_all.start=c(1992,1)
train_all.end=c(2018,9)
train_all.ts=window(y.ts,start = train_all.start,end=train_all.end)
M.naive=snaive(train_all.ts,h=12,level=95,lambda="auto")
M.naive
M.naive$fitted



