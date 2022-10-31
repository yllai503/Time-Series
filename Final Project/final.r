library(TSA)
library(tseries)


data = read.csv("C:/Users/miomi/Desktop/109/109-1/?ɶ??ǦC/?��????i/chart.csv",head=T)
data


View(data)
players = ts(data$Players, frequency = 365, start = c(2016,4))
players
viewer = ts(data$Twitch.Viewers, frequency = 365, start = c(2016,4))
viewer
logplayers = log(players)



#view
plot(players)
plot(diff(players))
bxh = BoxCox.ar(players)

bxh$mle # the mle of the power parameter
bxh$ci # corresponding 95% C.I.

data = players^-0.2
data


#stationary
acf((as.vector(data)))

adf.test(players)
kpss.test(players)

adf.test(diff(players))
kpss.test(diff(players))

#1 step diff
data1 = diff(data)

plot(data1)
acf(as.vector(data1),lag=60)
pacf(as.vector(data1))
eacf(as.vector(data1))
#1+7 step diff 
data2 = diff(data1,lag=7)
plot(data2)
acf(as.vector(data2),lag=60)#1,7,21?F15
pacf(as.vector(data2),lag=60)
eacf(as.vector(data2))#arma(0,1)

#sarima(1,1,1)*sarima(1,1,1)
#sarima(1,1,1)*sarima(1,1,0)
#sarima(0,1,1)*sarima(1,1,1)
#sarima(0,1,1)*sarima(1,1,0)
#sarima(1,1,0)*sarima(1,1,1)
#sarima(1,1,0)*sarima(1,1,0)

#7 step diff
data3 = diff(players,lag=7)

plot(data3)
acf(as.vector(data3),lag=60)
pacf(as.vector(data3),lag=60)
eacf(as.vector(data3))
#1+7+7 step diff 
data4 = diff(data2,lag=7)
plot(data4)
acf(as.vector(data4),lag=60)
pacf(as.vector(data4),lag=60)
eacf(as.vector(data4))


#SARIMAx(1,1,0)(1,1,0)_7,outlier
a = arima(data,order=c(7,1,0),seasonal=list(order=c(2,1,1),period=7),fixed = c(NA,0,0,0,0,0,NA,NA,NA,NA))
a
detectAO(a)
detectIO(a)
shift = 171
#SARIMAx(1,1,0)(1,1,0)_7,with outlier
xtf = data.frame(Ascendancy=1*(seq(players)==61),Prophecy=1*(seq(players)==152),
                 Atlas=1*(seq(players)==243),Breach=1*(seq(players)==334),
                 Legacy=1*(seq(players)==425),Oriath=1*(seq(players)==579),
                 Atlas2=1*(seq(players)==705),Bestiary=1*(seq(players)==789),
                 Incursion=1*(seq(players)==880),Delve=1*(seq(players)==971),
                 Betrayal=1*(seq(players)==1069),Synthesis=1*(seq(players)==1160),
                 Legion=1*(seq(players)==1251),Blight=1*(seq(players)==1342),
                 Atlas3=1*(seq(players)==1440),Delirium=1*(seq(players)==1531),
                 Harvest=1*(seq(players)==1629),Heist=1*(seq(players)==1720),
                 ac1=1*(seq(players)==1797),ac2=1*(seq(players)==1804),ac3=1*(seq(players)==1811),
                 ac4=1*(seq(players)==1223),ac5=1*(seq(players)==852),ac6=1*(seq(players)==943),
                 ac7=1*(seq(players)==677),ac8=1*(seq(players)==691),
                 what1=1*(seq(players)==124),what2=1*(seq(players)==444),what3=1*(seq(players)==558),
                 what4=1*(seq(players)==240),what5=1*(seq(players)==244))
tf = list(c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),c(0,0),
          c(1,0),c(0,0),c(0,0),
          c(1,0),c(1,0),
          c(0,0),c(1,0),c(0,0),
          c(1,0),c(1,0))


fixed1 = c(NA,NA,#AR
           NA,NA,#SAR
           NA,#SMA
           NA,NA,#Ascendancy
           NA,NA,#Prophecy
           NA,NA,#Atlas
           NA,NA,#Breach
           NA,NA,#Legacy
           NA,NA,#Oriath
           NA,NA,#Atlas2
           NA,NA,#Bestiary
           NA,NA,#Incursion
           NA,NA,#Delve
           NA,NA,#Betrayal
           NA,NA,#Synthesis
           NA,NA,#Legion
           NA,NA,#Blight
           NA,NA,#Atlas3
           NA,NA,#Delirium
           NA,NA,#Harvest
           NA,NA,#Heist
           NA,NA,#ac1
           NA,NA,#ac2
           NA,#ac3
           NA,NA,#ac4
           NA,#ac5
           NA,#ac6
           NA,NA,#ac7
           NA,NA,#ac8
           NA,#what1
           NA,NA,#what2
           NA,#what3
           NA,NA,#what4
           NA,NA#what5
           )
  
  
  
model=arima(data,order=c(2,1,0),seasonal=list(order=c(2,1,1),period=7),

             xtransf=xtf,
             transfer=tf,
             method = 'CSS')

model

detectAO(model)
detectIO(model)
res = residuals(a) 
res1 = residuals(model)
plot(res)
plot(res1)


acf(as.vector(res1))
acf(as.vector(res1*res1))
Box.test(as.vector(res1),lag =3)


hist(res1)
qqnorm(res1); qqline(res1)

t.test(res1)
shapiro.test(res1)
ks.test(res1,mean(res1),sd(res1))


res1[res1< -0.007]


abs(res1) >0.01


res1[abs(res1) >0.01]


POE=read.csv("D://?j?ǽҵ{/109 ?j??/?ɶ??ǦC?��R/?��????i/chart.csv")
POE$DateTime=NULL
sum(is.na(POE$Twitch.Viewers))


library(TSA)
library(aTSA)
library(tseries)
View(POE_Twitch)
POE_Twitch=ts(POE)

#???X?̾A?X?ഫ??lambda
BoxCox.ar(POE_Twitch,lambda = seq(-0.5, 0.5, 0.01))#lambda=-0.1
POE_Twitch1=POE_Twitch^(-0.1)
plot(POE_Twitch1)

#???w?T?خt?��G?@???t??lag=1, ?@???t??lag=7,?G???t??lag=1,7 ?ÿ??w?ҫ?
twitch1=diff(POE_Twitch1,lag=1)
plot(diff(POE_Twitch1,lag=1))
acf(diff(POE_Twitch1,lag=1))
pacf(diff(POE_Twitch1,lag=1))
eacf(diff(POE_Twitch1,lag=1))
#acf&pacf??ĳ
#SARIMA(1,1,1)x(1,0,1) AR1~4
#eacf??ĳ
#ARMA(0,1)

#?ˬdmean ??t.test
t.test(twitch1)
#?tSARIMA(1,1,1)x(1,0,1)>????
mod11=arima(twitch1,order=c(1,0,1),seasonal=list(order=c(1,0,1), period=7),
            include.mean = F)
detectAO(mod11)      
detectIO(mod11)

xtf_mod11 = data.frame(a1=1*(seq(twitch1)==52),a2=1*(seq(twitch1)==119),
                       a3=1*(seq(twitch1)==143),a4=1*(seq(twitch1)==217),
                       Atlas=1*(seq(twitch1)==242),a5=1*(seq(twitch1)==412),
                       Oriath=1*(seq(twitch1)==578),Incursion=1*(seq(twitch1)==879),
                       a6=1*(seq(twitch1)==968),Delve=1*(seq(twitch1)==970),
                       Betrayal=1*(seq(twitch1)==1068),Synthesis=1*(seq(twitch1)==1159),
                       Legion=1*(seq(twitch1)==1250),Blight=1*(seq(twitch1)==1341),
                       a7=1*(seq(twitch1)==1411),a8=1*(seq(twitch1)==1414),
                       Atlas3=1*(seq(twitch1)==1439),a9=1*(seq(twitch1)==1513),
                       Delirium=1*(seq(twitch1)==1530),a10=1*(seq(twitch1)==1702),
                       Heist=1*(seq(twitch1)==1719),ac1=1*(seq(twitch1)==1796))

tf_mod11 = list(c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0))
model11_intervene=arima(twitch1,order=c(1,0,1),seasonal=list(order=c(1,0,1), period=7),
                        include.mean = F,xtransf=xtf_mod11,transfer=tf_mod11,method = 'CSS')
model11_intervene
res11_intervene=residuals(model11_intervene)
plot(res11_intervene)
acf(res11_intervene)
hist(res11_intervene)
qqnorm(res11_intervene);qqline(res11_intervene)

fixed11 = c(NA,#AR
            NA,#MA
            0,#SAR
            NA,#SMA
            NA,NA,#a1
            NA,NA,#a2
            NA,NA,#a3
            NA,NA,#a4
            0,NA,#Atlas
            NA,NA,#a5
            0,NA,#Oriath
            0,NA,#Incursion
            NA,NA,#a6
            0,NA,#Delve
            0,NA,#Betrayal
            0,NA,#Synthesis
            0,NA,#Legion
            0,NA,#Blight
            0,NA,#a7
            0,NA,#a8
            0,NA,#Atlas3
            NA,NA,#a9
            0,NA,#Delirium
            NA,NA,#a10
            0,NA,#Heist
            0,NA)#ac1


model11_intervene_fixed=arima(twitch1,order=c(1,0,1),seasonal=list(order=c(1,0,1), period=7),
                              include.mean = F,fixed = fixed11,
                              xtransf=xtf_mod11,transfer=tf_mod11,method = 'CSS')
model11_intervene_fixed
res11_intervene_fixed=residuals(model11_intervene_fixed)
plot(res11_intervene_fixed)
acf(res11_intervene_fixed)
hist(res11_intervene_fixed)
qqnorm(res11_intervene_fixed);qqline(res11_intervene_fixed)

fixed11_1 = c(NA,#AR
              NA,#MA
              0,#SAR
              0,#SMA
              NA,NA,#a1
              NA,NA,#a2
              NA,NA,#a3
              NA,NA,#a4
              0,NA,#Atlas
              NA,NA,#a5
              0,NA,#Oriath
              0,NA,#Incursion
              NA,NA,#a6
              0,NA,#Delve
              0,NA,#Betrayal
              0,NA,#Synthesis
              0,NA,#Legion
              0,NA,#Blight
              0,NA,#a7
              0,NA,#a8
              0,NA,#Atlas3
              NA,NA,#a9
              0,NA,#Delirium
              NA,NA,#a10
              0,NA,#Heist
              0,NA)#ac1


model11_intervene_fixed_1=arima(twitch1,order=c(1,0,1),seasonal=list(order=c(1,0,1), period=7),
                                include.mean = F,fixed = fixed11_1,
                                xtransf=xtf_mod11,transfer=tf_mod11,method = 'CSS')
model11_intervene_fixed_1
res11_intervene_fixed_1=residuals(model11_intervene_fixed_1)
plot(res11_intervene_fixed_1)
acf(res11_intervene_fixed_1)
Box.test(res11_intervene_fixed_1, lag=6, type="Ljung-Box") #???q?L
hist(res11_intervene_fixed_1)
qqnorm(res11_intervene_fixed_1);qqline(res11_intervene_fixed_1)

#?tSARIMA(1,1,1)x(1,0,1)
twitch2=diff(POE_Twitch1,lag=7)
plot(diff(POE_Twitch1,lag=7))
acf(diff(POE_Twitch1,lag=7))
pacf(diff(POE_Twitch1,lag=7))
eacf(diff(POE_Twitch1,lag=7))
#acf&pacf??ĳ
#SARIMA(3,0,3)x(2,1,2)MA123 SMA12 AR123 SAR12
#eacf??ĳ
#ARMA(1,1) ARMA(1,3)
mod21=arima(twitch2,order=c(3,0,3),seasonal=list(order=c(2,0,2), period=7),
            include.mean = F,fixed=c(0,0,NA,NA,NA,NA,0,0,0,0))
mod21
mod21=arima(twitch2,order=c(3,0,3),seasonal=list(order=c(2,0,2), period=7),
            include.mean = F,fixed=c(0,0,NA,NA,NA,NA,0,0,NA,0))
detectAO(mod21)      
detectIO(mod21)

xtf_mod21 = data.frame(a1=1*(seq(twitch2)==46),a2=1*(seq(twitch2)==113),
                       a3=1*(seq(twitch2)==137),a4=1*(seq(twitch2)==234),
                       Atlas=1*(seq(twitch2)==236),a5=1*(seq(twitch2)==406),
                       Oriath=1*(seq(twitch2)==514),Incursion=1*(seq(twitch2)==572),
                       a6=1*(seq(twitch2)==782),Delve=1*(seq(twitch2)==873),
                       Betrayal=1*(seq(twitch2)==962),Synthesis=1*(seq(twitch2)==964),
                       Legion=1*(seq(twitch2)==1062),Blight=1*(seq(twitch2)==1153),
                       a7=1*(seq(twitch2)==1244),a8=1*(seq(twitch2)==1335),
                       Atlas3=1*(seq(twitch2)==1405),a9=1*(seq(twitch2)==1433),
                       Delirium=1*(seq(twitch2)==1507),a10=1*(seq(twitch2)==1696),
                       Heist=1*(seq(twitch2)==1790))

tf_mod21 = list(c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0),c(1,0),
                c(1,0))
model21_intervene=arima(twitch2,order=c(3,0,3),seasonal=list(order=c(2,0,2), period=7),
                        include.mean = F,fixed=c(0,0,NA,NA,NA,NA,0,0,NA,0,NA,NA,NA,
                                                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                        xtransf=xtf_mod21,transfer=tf_mod21,method = 'CSS')
model21_intervene
res21_intervene=residuals(model21_intervene)
plot(res21_intervene)
acf(res21_intervene)
hist(res21_intervene)
qqnorm(res21_intervene);qqline(res21_intervene)

fixed21 = c(0,0,NA,NA,NA,NA,0,0,NA,0,
            NA,NA,#a1
            0,NA,#a2
            0,NA,#a3
            0,0,#a4
            NA,NA,#Atlas
            NA,0,#a5
            0,0,#Oriath
            NA,NA,#Incursion
            NA,0,#a6
            0,NA,#Delve
            NA,NA,#Betrayal
            NA,0,#Synthesis
            NA,NA,#Legion
            NA,NA,#Blight
            NA,NA,#a7
            NA,0,#a8
            NA,NA,#Atlas3
            NA,NA,#a9
            NA,NA,#Delirium
            NA,NA,#a10
            NA,NA)#Heist



model21_intervene_fixed=arima(twitch2,order=c(3,0,3),seasonal=list(order=c(2,0,2), period=7),
                              include.mean = F,fixed=fixed21,
                              xtransf=xtf_mod21,transfer=tf_mod21,method = 'CSS')
model21_intervene_fixed
res21_intervene_fixed=residuals(model21_intervene_fixed)
plot(res21_intervene_fixed)
acf(res21_intervene_fixed)
Box.test(res21_intervene_fixed, lag=6, type="Ljung-Box")
hist(res21_intervene_fixed)
qqnorm(res21_intervene_fixed);qqline(res21_intervene_fixed)

#?˩w?@???t?��????T??
kpss.test(diff(POE_Twitch1))
adf.test(diff(POE_Twitch1))

twitch3=diff(diff(POE_Twitch1, lag=1),lag=7)
plot(diff(diff(POE_Twitch1, lag=1),lag=7))
acf(diff(diff(POE_Twitch1, lag=1),lag=7))
pacf(diff(diff(POE_Twitch1, lag=1),lag=7))
eacf(diff(POE_Twitch, lag=7))
#acf&pacf??ĳ
#SARIMA(1,1,1)x(1,1,1) (1,1,1)x(0,1,1)
#eacf??ĳ
#ARMA(1,2)

#?˩w?G???t?��????T??
kpss.test(diff(POE_Twitch1))
adf.test(diff(POE_Twitch1))


mod1=arima(diff(diff(POE_Twitch1, lag=1),lag=7),order=c(1,0,0),seasonal=list(order=c(1,0,0), period=7),include.mean = F)
res1=residuals(mod1)
plot(res1)
acf(res1)
detectAO(mod1)      
detectIO(mod1)

twitch=(diff(diff(POE_Twitch1, lag=1),lag=7))
xtf = data.frame(Ascendancy=1*(seq(twitch)==45),Prophecy=1*(seq(twitch)==116),
                 Atlas=1*(seq(twitch)==136),Breach=1*(seq(twitch)==218),
                 Legacy=1*(seq(twitch)==228),Oriath=1*(seq(twitch)==233),
                 Atlas2=1*(seq(twitch)==240),Bestiary=1*(seq(twitch)==405),
                 Incursion=1*(seq(twitch)==419),Delve=1*(seq(twitch)==571),
                 Betrayal=1*(seq(twitch)==872),Synthesis=1*(seq(twitch)==1243),
                 Legion=1*(seq(twitch)==1334),Blight=1*(seq(twitch)==1404),
                 Atlas3=1*(seq(twitch)==1695),Delirium=1*(seq(twitch)==1789))
tf = list(c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0),
          c(1,0),c(1,0))

tf = list(c(0,0),c(0,0),
          c(0,0),c(0,0),
          c(0,0),c(0,0),
          c(0,0),c(0,0),
          c(0,0),c(0,0),
          c(0,0),c(0,0),
          c(0,0),c(0,0),
          c(0,0),c(0,0))


model=arima(twitch,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=7),
            
            xtransf=xtf,
            transfer=tf,
            method = 'CSS')
model #Legacy(??228????????)

model1=arima(twitch,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=7),
             
             xtransf=xtf,
             transfer=tf,
             method = 'CSS',fixed = c(NA,NA,NA,0,NA,0,NA,NA,NA,0,NA,0,NA,NA,NA,NA,NA,NA,NA))
residuals1=residuals(model)
plot(residuals1)
acf(residuals1)
hist(residuals1)



xtf_newversion = data.frame(Ascendancy=1*(seq(twitch)==61),Prophecy=1*(seq(twitch)==152),
                            Atlas=1*(seq(twitch)==243),Breach=1*(seq(twitch)==334),
                            Legacy=1*(seq(twitch)==425),Oriath=1*(seq(twitch)==579),
                            Atlas2=1*(seq(twitch)==705),Bestiary=1*(seq(twitch)==789),
                            Incursion=1*(seq(twitch)==880),Delve=1*(seq(twitch)==971),
                            Betrayal=1*(seq(twitch)==1069),Synthesis=1*(seq(twitch)==1160),
                            Legion=1*(seq(twitch)==1251),Blight=1*(seq(twitch)==1342),
                            Atlas3=1*(seq(twitch)==1440),Delirium=1*(seq(twitch)==1531),
                            Harvest=1*(seq(twitch)==1629),Heist=1*(seq(twitch)==1720))
tf_newversion = list(c(1,0),c(1,0),
                     c(1,0),c(1,0),
                     c(1,0),c(1,0),
                     c(1,0),c(1,0),
                     c(1,0),c(1,0),
                     c(1,0),c(1,0),
                     c(1,0),c(1,0),
                     c(1,0),c(1,0),
                     c(1,0),c(1,0))

tf_newversion = list(c(0,0),c(0,0),
                     c(0,0),c(0,0),
                     c(0,0),c(0,0),
                     c(0,0),c(0,0),
                     c(0,0),c(0,0),
                     c(0,0),c(0,0),
                     c(0,0),c(0,0),
                     c(0,0),c(0,0),
                     c(0,0),c(0,0))

model_newversion=arima(twitch,order=c(1,0,0),seasonal=list(order=c(0,1,1),period=7),
                       
                       xtransf=xtf_newversion,
                       transfer=tf_newversion,
                       method = 'CSS')

model_newversion
residuals_newversion=residuals(model_newversion)
plot(residuals_newversion)
acf(residuals_newversion)
hist(residuals_newversion)
qqnorm(residuals_newversion);qqline(residuals_newversion)