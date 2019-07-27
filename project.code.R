### ====================== STAT 443 Term Project ====================== ###

# Term project for STAT 443: Time Series and Forecasting, University of British Columbia
# Created by Carson Ho (25431157), Alan Lau (53984167), Chris Lin (15787154)
# Last edited on March 8, 2019

# Project Title: How Do Economic Indicators Impact the General Public's Interest in US Jobless Claims?
# Dependent variable: Google Search Volume Index (SVI)
# Explanatory variables: Initial Claims, St Louis Fed Financial Stress Index, Standard & Poor's 500, Russell 2000

# Notes (March 8, 2019): I made some changes since our last meeting.

# Regarding the use of Initial Claims (ICSA) as an explanatory variable;
# I decided to use percentage change in ICSA instead of just "raw" ICSA values, so as to make it stationary (just so the prof and the TAs can't take marks off our report)
# As you know, log-differences and percentage changes are essentially the same; at values close to zero they are close approximations of each other.
# However, the farther the values are from zero, the approximations become a lot less accurate.
# For this reason, I opted for percentage changes in ICSA instead of log-differences, because ICSA values are pretty big (in the 100000s).
# That said, for the 3 stock indexes we are using in our analysis we will stick to log-differencing.

# One more thing. At our last meeting we talked about how we should just use "raw" Google Search Volume Index (SVI) values.
# This is because the SVI is an index with a range of 0 to 100; applying any transformation to it may distort its values.
# But just to be safe, I tried out several models, using the differenced version, the log-differenced version, and the percentage change version as dependent variables respectively.
# Turns out, the log-differencing and percentage change versions don't work, because the SVI includes some zeros.
# This means applying log-differencing or percentage change versions result in some undefined values.
# As for simple first differencing, the model can be fitted, but we get an out-of-sample RMSE that is larger than the "average of past observations" method.

# All in all, for the report I suggest we go with the following set up:
# Dependent variable: "raw" Google Search Volume Index (SVI) values
# Explanatory variables:
# Percentage changes in Initial Claims (ICSA)
# Log-differenced St Louis Fed Financial Stress Index (STLFSI)
# Log-differenced Standard & Poor's 500 (S&P 500)
# Log-differenced Russell 2000 (RUT)

# This results in an ARIMAX(2,0,1) model that gives the smallest out-of-sample RMSE, when compared to the plain ARIMA model and the 2 baseline models.

# PS: For your convenience, I saved all the plots as R objects. There is a list at the very bottom of this document.

# ====================== Beginning of Script ====================== #

# Clean workspace and set working directory
rm(list=ls())
setwd("E:/2018-19 W2/STAT 443/Term Project"); getwd()

# Load required packages
pacman::p_load(readxl, aTSA, xts, forecast, ggplot2, GGally, xtable)

# Import data
#Data<-read_excel("Data.xlsx", sheet="Data", col_types=c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))

# Define time series data and set up matrix of explanatory variables
SVI<-ts(Data$`Google SVI`); ICSA<-ts(Data$ICSA); STLFSI<-ts(Data$STLFSI); SP500<-ts(Data$`S&P 500`); RUT<-ts(Data$`Russell 2000`)
Data2<-Data; Data2$Week<-NULL; Data2$`Google SVI`<-NULL; Matrix<-data.matrix(Data2); rm(Data2)

# Define training and holdout sets (Training set: 1st to 210th observations | Holdout set: 211th to 260th observations)
SVI_train<-SVI[1:210]; SVI_holdout<-SVI[211:260]
Matrix_train<-Matrix[1:210, 1:4]; Matrix_holdout<-Matrix[211:260, 1:4]

# ====================== Testing for stationarity in variables ====================== #

# Google Search Volume Index (SVI)
stationary.test(SVI, method="adf")# Stationary except if we don't allow for drift and trend at lags 2, 3 and 4

# Percentage Change in Initial Claims (ICSA)
stationary.test(ICSA, method="adf")# Stationary

# Log-differenced St Louis Fed Financial Stress Index (STLFSI)
stationary.test(STLFSI, method="adf")# Stationary

# Log-differenced Standard & Poor's 500 (S&P 500)
stationary.test(SP500, method="adf")# Stationary

# Log-differenced Russell 2000 (RUT)
stationary.test(RUT, method="adf")# Stationary

# ====================== Correlation plots ====================== #

# Simple plot of correlations between variables
Data2<-Data; Data2$Week<-NULL; Figure_1<-ggpairs(Data2, title="Correlations between variables"); rm(Data2)

# Autocorrelation (ACF) & partial autocorrelation (PACF) plots for all variables
Figure_2<-ggtsdisplay(SVI, lag.max=20, main="Google Search Volume Index (SVI)", xlab="Week")
Figure_3<-ggtsdisplay(ICSA, lag.max=20, main="Percentage Change in Initial Claims (ICSA)", xlab="Week")
Figure_4<-ggtsdisplay(STLFSI, lag.max=20, main="Log-Differenced St Louis Fed Financial Stress Index (STLFSI)", xlab="Week")
Figure_5<-ggtsdisplay(SP500, lag.max=20, main="Log-Differenced Standard & Poor's 500 (S&P 500)", xlab="Week")
Figure_6<-ggtsdisplay(RUT, lag.max=20, main="Log-Differenced Russell 2000 (RUT)", xlab="Week")

# ====================== Out-of-sample RMSEs of baseline models ====================== #

# Persistence (random walk) model
fc<-NULL; zt<-SVI_holdout; fcerror<-NULL; MSE<-NULL; fc[1]<-SVI_train[210]
for(i in 2:50){
  fc[i]<-SVI_holdout[i-1]
}
fcerror<-zt-fc; MSE<-sum(fcerror^2); RMSE_rwf<-sqrt(MSE/50); rm(fc); rm(zt); rm(fcerror); rm(MSE); rm(i)
RMSE_rwf #11.98249

# Average of all previous observations
fc<-NULL; zt<-SVI_holdout; fcerror<-NULL; MSE<-NULL; cumsum<-sum(SVI_train); fc[1]<-cumsum/210
for(i in 2:50){
  cumsum<-cumsum+SVI_holdout[i-1]
  fc[i]<-cumsum/(210+i-1)
}
fcerror<-zt-fc; MSE<-sum(fcerror^2); RMSE_avgpo<-sqrt(MSE/50); rm(fc); rm(zt); rm(fcerror); rm(MSE); rm(cumsum); rm(i)
RMSE_avgpo #10.62995

# simple exponential smoothing (level only)
simple_exp <- HoltWinters(SVI_holdout,beta=F,gamma=F)
simple_exp
# in sample RMSE
names(simple_exp)
simple_exp$SSE
sqrt(simple_exp$SSE / (length(SVI_holdout)-1))
#RMSE = 8.905084

# linear (Holt) exponential smoothing (level+trend)
holt <- HoltWinters(SVI_holdout,gamma=F)
holt
#coefficient
a <- 27.82096
b <- 1.59778
#v and b
alpha <- 0.5118522
beta <- 0.4537782
#in sample RMSE
names(holt)
holt$SSE
sqrt(holt$SSE / (length(SVI_holdout)-1)) 
#RMSE = 12.28643

# Plotting forecasts with the "forecast" package
Figure_7<-autoplot(naive(ts(SVI[1:210]), h=50), main="Forecasts from Persistence (Random Walk) Model on Google SVI", xlab="Week", ylab="")
Figure_7

Figure_8<-autoplot(meanf(ts(SVI[1:210]), h=50), main="Forecasts from Average of Past Observations Model on Google SVI", xlab="Week", ylab="")
Figure_8

Figure_9<-autoplot(ts(SVI[1:210])) + autolayer(meanf(ts(SVI[1:210]), h=50), series="Avg. of past", PI=FALSE) + 
  autolayer(naive(ts(SVI[1:210]), h=50), series="Persistence", PI=FALSE) + 
  ggtitle("Comparison of Baseline Forecasting Models on Google SVI") + xlab("Week") + ylab("") + guides(colour=guide_legend(title="Forecast"))
Figure_9

# ====================== Fitting ARIMA/ARIMAX models ====================== #
# ARIMA(1,0,1) model (without explanatory variables) # out-of-sample RMSE lower than baseline models
Model1<-auto.arima(SVI_train, approximation=FALSE, test="adf"); print(Model1); checkresiduals(Model1); Figure_10<-recordPlot()
Model1_fc<-forecast(Model1, h=50); fcerror<-SVI_holdout-Model1_fc$mean; MSE<-sum(fcerror^2); RMSE_Model1<-sqrt(MSE/50); rm(fcerror); rm(MSE)
Figure_11<-autoplot(Model1_fc, main="Forecasts from ARIMA(1,0,1) on Google SVI", xlab="Week", ylab="")
RMSE_Model1

# ARIMAX(2,0,1) model (with explanatory variables) # lowest out-of-sample RMSE
Model2<-auto.arima(SVI_train, approximation=FALSE, xreg=Matrix_train, test="adf"); print(Model2); checkresiduals(Model2); Figure_12<-recordPlot()
Model2_fc<-forecast(Model2, h=50, xreg=Matrix_holdout); fcerror<-SVI_holdout-Model2_fc$mean; MSE<-sum(fcerror^2); RMSE_Model2<-sqrt(MSE/50); rm(fcerror); rm(MSE)
Figure_13<-autoplot(Model2_fc, main="Forecasts from ARIMAX(2,0,1) on Google SVI", xlab="Week", ylab="")
RMSE_Model2
# ====================== End of Script ====================== #
# For your convenience, I have saved all the plots as R objects.  They are all listed here, so you don't have to find it from the ocean of code above.

print(Figure_1)# Figure 1: Correlations bewteen variables
print(Figure_2)# Figure 2: Google Search Volume Index (SVI)
print(Figure_3)# Figure 3: Percentage Change in Initial Claims (ICSA)
print(Figure_4)# Figure 4: Log-Differenced St Louis Fed Financial Stress Index (STLFSI)
print(Figure_5)# Figure 5: Log-Differenced Standard & Poor's 500 (S&P 500)
print(Figure_6)# Figure 6: Log-Differenced Russell 2000 (RUT)
print(Figure_7)# Figure 7: Forecasts from Persistence (Random Walk) Model on Google SVI
print(Figure_8)# Figure 8: Forecasts from Average of Past Observations Model on Google SVI
print(Figure_9)# Figure 9: Comparison of Baseline Forecasting Models on Google SVI
print(Figure_10)# Figure 10: Residuals from ARIMA(1,0,1) model
print(Figure_11)# Figure 11: Forecasts from ARIMA(1,0,1) on Google SVI
print(Figure_12)# Figure 12: Residuals from ARIMA(2,0,1) model
print(Figure_13)# Figure 13: Forecasts from ARIMAX(2,0,1) on Google SVI

### ====================== END OF DOCUMENT ====================== ###