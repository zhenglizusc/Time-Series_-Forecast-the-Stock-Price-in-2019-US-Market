library(forecast)

# Use scan function if you are importing a column of numbers
# use read.csv if a table
M2Regression1.residuals=scan("M2Regression1.txt")
tsdisplay(M2Regression1.residuals)
# ACF -> ther is dependence, PACF -> include lag 1


M2Regression2.residuals=scan("M2Regression2.txt")
tsdisplay(M2Regression2.residuals)
# ACF -> no significant lags, residuals are white noise! Model is good.