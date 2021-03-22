library(quantmod)
library(rugarch)
library(rmgarch)
library(tseries)
library(MTS)
library(fGarch)



startDate = as.Date("2010-12-03") #Specify period of time we are interested in
endDate = as.Date("2020-12-01")
getSymbols("GLD", from = startDate, to = endDate)
getSymbols("^IXIC", from = startDate, to = endDate)

Gold = `GLD`
Gold
Nasdaq = IXIC

#Log-Returns

logGold = diff(log(Gold$`GLD.Adjusted`), lag = 1)
logGold = logGold[!is.na(logGold)]

Lognasdaq = diff(log(Nasdaq$IXIC.Adjusted), lag = 1)
Lognasdaq = Lognasdaq[!is.na(Lognasdaq)]

str(Lognasdaq)

plot(logGold)
plot(logGold^2)

pacf(Lognasdaq)
chartSeries(Gold)
chartSeries(Nasdaq)
#Dicki fuller Test if small p value wir haben stationary 
print(adf.test(logGold))
print(adf.test(Lognasdaq))



#Garch(1,1)Gold

ug_spec = ugarchspec()
ug_spec
# change from ARMA(1,1) to ARMA(1,0)
ug_spec <- ugarchspec(mean.model=list(armaOrder=c(17,0)))

ugfitGold = ugarchfit(spec = ug_spec, data = logGold)
ugfitGold

plot(ugfitGold,which=3)

#Garch(1,1)Nasdaq

ug_spec = ugarchspec()
ug_spec
# change from ARMA(1,1) to ARMA(1,0)
ug_spec <- ugarchspec(mean.model=list(armaOrder=c(9,0)))

ugfitNas = ugarchfit(spec = ug_spec, data = Lognasdaq)
ugfitNas

plot(ugfitNas,which=1)
#Nasdaq Garch(1,1) squared Residual und Var 

ugfitNas@fit$coef

ug_var_nas <- ugfitNas@fit$var   # save the estimated conditional variances
ug_res2_nas <- (ugfitNas@fit$residuals)^2   # save the estimated squared residuals

plot(ug_res2_nas, type = "l")
lines(ug_var_nas, col = "blue")

#Gold Garch(1,1) squared Residual und Var 

ugfitGold@fit$coef

ug_var_Gold <- ugfitGold@fit$var   # save the estimated conditional variances
ug_res2_Gold <- (ugfitGold@fit$residuals)^2   # save the estimated squared residuals

plot(ug_res2_Gold, type = "l")
lines(ug_var_Gold, col = "green")


#VGL conditional variances (Nasdaq,Gold)

plot(ug_res2_nas, type = "l")
lines(ug_res2_Gold, col = "blue")


plot(ug_var_nas, type = "l")
lines(ug_var_Gold, col = "blue")

plot(Lognasdaq^2, type = "l")
lines(logGold^2, col = "blue")

plot(Lognasdaq, type = "l")
lines(logGold, col = "blue")

#AR(1) Models
pacf(logGold)
pacf(Lognasdaq)

acf(logGold)
acf(Lognasdaq)

ARGOLD = arima(logGold, order = c(17,0,0))
ARNAS = arima(Lognasdaq, order = c(9,0,0))

print(ARGOLD)
ARNAS

names(ARGOLD)
ug_var_ARGOLD <- ARGOLD$var.coef   # save the estimated conditional variances
ug_res2_ARGOLD <- (ARGOLD$residuals)^2   # save the estimated squared residuals

ug_var_ARNAS <- ARNAS$var.coef   # save the estimated conditional variances
ug_res2_ARNAS <- (ARNAS$residuals)^2

plot(ug_var_ARGOLD)
lines(ug_var_ARGOLD, col = "blue")

#ARCH Test LM-Test of Engle(1982) Lagrange-Multplier-Test

res_Gold_AR =resid(ARGOLD)
res_Nas_AR =resid(ARNAS)

archTest(res_Gold_AR)
archTest(res_Nas_AR)
#Since p values is less than α, we reject Ho.
#Therefore, we can conclude the presence of ARCH effects

plot(res_Gold_AR)
plot(g)
plot(g^2)

plot(res_Nas_AR)
lines(res_Gold_AR, col = "blue")

plot(res_Nas_AR^2)
lines(res_Gold_AR^2, col = "blue")

