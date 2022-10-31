library(TSA)
#=================================================
# SARIMA(0,0,1)*(0,0,1)
n=(12*40)+13
theta= -0.5
stheta = 0.8

et=rnorm(n)
yt=et[-(1:13)] - theta*(et[-n][-(1:12)])- stheta*(et[-((n-11):n)][-1])+theta*stheta*et[-((n-12):n)]


plot(yt,type='o')
acf(yt,lag=60)

#=================================================
# SARIMA(1,0,0)*(0,0,1)
n=(12*40)+11
phi = 0.6
stheta = -0.8

et=rnorm(n)
yt=c(et[12]/sqrt(1-phi^2))

for(i in 13:(n)){
  
  yt=c(yt,phi*yt[(i-11)-1] + et[i] - stheta*et[i-12] )
  
}
length(yt)

plot(yt,type='o')
acf(yt,lag=60)

#==============================================
### HW10
#SARIMA(1,0,0)*(1,0,0)_12 
n=12*40+12
phi = 0.6
sphi = 0.8

et=rnorm(n)
yt=c(et[1]/sqrt(1-phi^2))

#Step 1 to step 12
for (j in 1:12){
  yt <- c(yt, phi*yt[j-1]+et[j])
}
#Step 13
yt <- c(yt, phi*yt[12]+et[13]+sphi*yt[1])
#Step 13 to step 492
for(i in 14:(n)){
  yt=c(yt,phi*yt[i-1]+sphi*yt[i-12]-phi*sphi*yt[i-13]+ et[i])
  
}
#48 steps
yt <- yt[-c(1:12)]
length(yt)

plot(yt,type='o', main="SARIMA(1,0,0)*(1,0,0)_12 ")
acf(yt,lag=60)
pacf(yt,lag=60)

#=================================================
#SARIMA(0,0,1)*(1,0,0)_12
n=(12*40)+13
theta= -0.5
sphi = 0.8

et=rnorm(n) 

#Step 1 to step 12
yt <- et[2:13]-theta*et[1:12]
#Step 13
for (i in 14:n){
  yt <- c(yt, sphi*yt[i-12]+et[i]-theta*et[(i-12)-1])
}

yt <- yt[-c(1:12)]
length(yt)

plot(yt,type='o', main="SARIMA(0,0,1)*(1,0,0)_12 ")
acf(yt,lag=120)
pacf(yt,lag=120)

#install.packages("sarima")  need this package

library(TSA)
library(sarima)
require("PolynomF")   #sarima need it



#10.8
data(co2)
co2


month=season(co2)
trend=time(co2)

model=lm(co2~month+trend)
summary(model)

acf(residuals(model))
# SARIMA(0,0,1)*(1,0,0)_12

x <- sim_sarima(n=12*40, model = list(ma=0.5, sar=-0.8, nseasons=12))
acf(x,lag=48)



#SARIMA(1,0,0)*(1,0,0)_12


x <- sim_sarima(n=12*40, model = list(ar=0.5, sar=-0.8, nseasons=12))
acf(x,lag=48)










