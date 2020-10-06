# EXERCISE 1.4
d_3stock <- read.table("d-3stock.txt")


AX=d_3stock$V2 #american express returns
date=d_3stock$V1
lAX<-diff(log(AX)) #log returns
library("fBasics")
basicStats(lAX) 
# skewness is -0.061669, excess kurtosis is 2.513587
hist(lAX)

qqnorm(lAX)
#joint test:
normalTest(lAX, method = "jb") #normality for log returns is rejected, p value very small (jarque-bera test)

s=skewness(lAX)
#test statistic for skewness (H0: skew=0, i.e. normality on the skewness side):
t1=skewness(lAX)/sqrt(6/length(lAX)) #-1.26
#p value:
pv1=2*(1-pnorm(t1))
pv1
# p value is 1.793621, which is bigger than 0.05 (we want to do a 5% significance level test)

# excess kurtosis test (H0: exc.kurt.=0, or kurt-3=0):
t2=kurtosis(lAX, method = "excess")/sqrt(24/length(lAX)) #25.75149
# p value:
pv2=2*(1-pnorm(t2))
pv2 #it is 0, so we reject H0 (not normal)
################################################################################
# Exercise 1.5


library("fBasics")
excaus=read.table("d-caus.txt",header=T)
exjpus=read.table("d-jpus.txt",header=T)
exuseu=read.table("d-useu.txt",header=T)
exusuk=read.table("d-usuk.txt",header=T)

# Compute the daily log return of each exchange rate.

retca=excaus[,4] # returns
retjp=exjpus[,4]
reteu=exuseu[,4]
retuk=exusuk[,4]

tsca=ts(retca,  frequency = 252, start = c(2000, 1))
tsjp=ts(retjp,  frequency = 252, start = c(2000, 1))
tseu=ts(reteu,  frequency = 252, start = c(2000, 1))
tsuk=ts(retuk,  frequency = 252, start = c(2000, 1))





logretca=diff(log(tsca))
logretjp=diff(log(tsjp))
logreteu=diff(log(tseu))
logretuk=diff(log(tsuk))


# Compute the sample mean, standard deviation, skewness, excess kurtosis,minimum, and maximum of the log returns of each exchange rate. 
basicStats(logretca)
basicStats(logretjp)
basicStats(logreteu)
basicStats(logretuk)


# Discuss the empirical characteristics of the log returns of exchange rates. 
par(mfcol=c(2,2))
plot(logretca, type="l")
plot(logretjp, type="l")
plot(logreteu, type="l")
plot(logretuk, type="l")
par(mfrow=c(1,1))
# log returns mostly vary between -0.02 and 0.02.

Box.test(logretca, lag= 500, type = "Ljung-Box")
Box.test(logretjp, lag= 500, type = "Ljung-Box")
Box.test(logreteu, lag= 500, type = "Ljung-Box")
Box.test(logretuk, lag= 500, type = "Ljung-Box")
# for high values of lag, the log returns are IID

# Obtain a density plot of the daily long returns of dollar-euro exchange rate.
hist(logreteu, prob= T)
################################################################################
# EXERCISE 2.13

crsp=read.table("m-ew6299.txt", header = F)
crsp1=ts(crsp, frequency = 12, start = c(1962, 1))
plot(crsp1)

# QUESTION A

mod1=ar(crsp1, method="yule-walker") 
mod1$order #we get the order of the AR model selected according to AIC
mod2=arima(crsp1, order = c(1,0,0))
mod2

plot(mod2$residuals) #a couple of big jumps

ac1=acf(crsp, lag = 200) # acf on the original data, not on the time series
acr1=acf(arima(crsp, order = c(1,0,0))$residuals, lag = 200) #autocorrelation of the residuals
plot(acr1) # some autocorrelations are significantly different from 0, which is not the best case

Box.test(acr1$acf, lag=10, type = "Ljung-Box") # we test if the residuals
Box.test(acr1$acf, lag=50, type = "Ljung-Box") # are independent, and we cant't always accept
Box.test(acr1$acf, lag=165, type = "Ljung-Box")# (p values are smaller than 0.05 for very big lags,
# bigger than 164)

library("lmtest")
coeftest(mod2) # we are testing the null hypothesis that the coefficients estimated
# by the function arima are equal to 0, and we always reject it

# The fitted model based on an AR(1) is not so good,
# because the result of the Ljung-Box test can't confirm that the residuals are all IID, 
# but the z test lets us reject the hypothesis that the estimated coefficients are equal to 0.
# In the end, an AR(1) is not the best choice for our data


# QUESTION B

library("forecast")
auto.arima(crsp, max.p = 0, stationary = T, seasonal = F, ic="aic")
# from here we get that the order of the MA we have to fit is equal to 1, according
# to AIC
ma1=arima(crsp1, order = c(0,0,1))
ma1
plot(ma1$residuals) # a couple of big jumps

acr2=acf(arima(crsp, order = c(0,0,1))$residuals, lag = 200)
plot(acr2) # we have some autocorrelations significantly different from 0, not the best case

# we test the independence of the residuals:
Box.test(acr2$acf, lag=10, type = "Ljung-Box") # we test if the residuals
Box.test(acr2$acf, lag=50, type = "Ljung-Box") # are independent, and we cant't always accept
Box.test(acr2$acf, lag=144, type = "Ljung-Box")# (p values for lag bigger than 143
# are smaller than 0.05)

coeftest(ma1) # we always reject our null hypothesis, i. e. all coefficients
# are different from 0

# The fitted model based on an MA(1) is not so good,
# because the result of the Ljung-Box test can't confirm that the residuals are all IID, 
# but the z test lets us reject the hypothesis that the estimated coefficients are equal to 0.
# In the end, an MA(1) is not the best choice for our data, as well as an AR(1)


# QUESTION C

predict(mod2, n.ahead = 1) # 1-step-forward prediction for our estimated AR(1)
predict(mod2, n.ahead = 2) # 2-step-forward prediction for our estimated AR(1)

predict(ma1, n.ahead = 1) # 1-step-forward prediction for our estimated MA(1)
predict(ma1, n.ahead = 2) # 2-step-forward prediction for our estimated MA(1),
# which is just the estimated mean (the intercept of the arima function output)


# QUESTION D

# The two models we fitted to our data are actually similar since both have coefficients
# significantly different from 0, but non IID residuals. The difference between 
# the two models is just the lag for which the Ljung-Box test makes us reject
# the independence hypothesis (for the AR(1) is 165, 144 for the MA(1)). Regarding
# the coefficients, the estimated intercept (estimated mean) is slightly bigger
# for our AR(1), while the coefficient for the stochastic part is bigger for our MA(1).
# The estimated variance is slightly bigger for the AR(1). After all, the two models
# produce almost the same fitting. 