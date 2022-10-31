#3.7
#(a)
library(TSA)
data(winnebago)
xyplot(winnebago)
#(b)
winn_fit1 <- lm(winnebago ~ time(winnebago))
pander(summary(winn_fit1))
xyplot(rstudent(winn_fit1) ~ time(winnebago), type = "l",
       xlab = "Time", ylab = "Studentized residuals")
#(c)
winn_fit_log <- lm(log(winnebago) ~ time(winnebago))
pander(summary(winn_fit_log))
xyplot(log(winnebago) ~ time(winnebago), type = "l",
       xlab = "Time", ylab = "log(sales)",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, pch = as.vector(season(winnebago)), col = 1)})
#(d)
xyplot(rstudent(winn_fit_log) ~ time(winnebago), type = "l",
       xlab = "Time", ylab = "Studentized residuals",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, pch = as.vector(season(winnebago)), col = 1)})
#(e)
winn_fit_seasonal <- lm(log(winnebago) ~ season(winnebago) + time(winnebag
                                                                  o))
pander(summary(winn_fit_seasonal))
#(f)
xyplot(rstudent(winn_fit_seasonal) ~ time(winnebago), type = "l",
       xlab = "Time", ylab = "Studentized residuals",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, col = 1, pch = as.vector(season(winnebago)))})
#3.9
#(a)
data(prescrip)
xyplot(prescrip, ylab = "Prescription costs",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, pch = as.vector(season(prescrip)), col = 1)})
#(b)
pchange <- diff(prescrip) / prescrip
xyplot(pchange ~ time(prescrip), type = "l",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, pch = as.vector(season(pchange)), col = 1)})
#(c)
pres_cos <- lm(pchange ~ harmonic(pchange))
pander(summary(pres_cos))
#(d)
xyplot(rstudent(pres_cos) ~ time(prescrip), type = "l")
#3.16
#(c)
phi <- seq(-1, 1, 0.01)
var_ybar <- (1 + phi) / (1 - phi)
xyplot(var_ybar ~ phi, ylab = expression(Var(bar(Y))), xlab = expression(phi),
       type = "l")