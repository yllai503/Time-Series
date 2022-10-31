#install.packages('fGarch')
library(TSA)
library(fGarch)
data(google)
par(mfrow=c(1,1))
#a

plot(google)


length((google))

detectAO(model)
detectIO(model)
google2 = ts(google,start = c(2004,226),frequency = 365)
google2
plot(google2)



acf(google,lag = 10)
pacf(google,lag = 10)
eacf(google)


model=arima(google,order=c(0,0,0))
model=arima(google,order=c(4,0,0))
model=arima(google2,order=c(4,0,0),fixed=(c(0,0,0,NA,NA)))
model




res = residuals(model)



plot(res)
#acf
acf(res)
LB.test(model,lag=4)
#mean
hist(res)
t.test(res,mu=0)
#distribution
qqnorm(res)
qqline(res, col = 2)
shapiro.test(res)
#
t.test(google,mu=0)

model
#c


McLeod.Li.test(y=res)
#d
acf(res^2)
pacf(res^2)
eacf(res^2)
mean(google)
g1 = garchFit(~garch(1, 1), data = res, include.mean=F)
summary(g1)

vol = volatility(g1)
plot(vol,type='l')
plot(google,type='l')
plot(google/vol)


res1 = res/vol

var(res1)

par(mfrow=c(2,1))
acf(res1)
acf(res1^2)
Box.test(res1 , lag = 4,type = "Ljung")
Box.test(res1^2 , lag = 26,type = "Ljung")




par(mfrow=c(1,1))
plot(res1, type = 'l' )
hist(res1)
qqnorm(res1 ); qqline(res1,col='red')

t.test(res1)
shapiro.test(res1)
ks.test(res1,mean(res1),sd(res1))

#e
plot(vol^2,type='l',ylab='conditional variance',xlab='t')


vol[517:521]

#(g)
sd(res)
mean(google)
#i
predict(g1,n.ahead = 5)


fit = garchFit(~arma(0,0,0)+garch(1, 1), data =google)
predict(fit, n.ahead = 10)














