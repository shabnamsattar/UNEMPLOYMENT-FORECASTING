###Libraries###
library(readxl)
library(lmtest) 
library(forecast)
library(fpp2)
library(DIMORA)
library(gam)
library(caret)
library(ggplot2)
library(tidyr)
library(Metrics)
library(splines)
library(corrplot)

##########Total EU unemployment##########

UnepmEU<- read_excel(path = file.choose())
str(UnepmEU)
##create a variable 'time'
EuropeanUnion <- UnepmEU$EuropeanUnion
tt<- 1:NROW(UnepmEU)
EU <- UnepmEU$rates

##make a plot
plot(tt, EU, xlab = "Time", ylab = "European Union Rates", xaxt = "n")
axis(1, at = tt, labels = EuropeanUnion)
acf(EU) 

##fit a linear regression model 
fit1 <- lm(EU~ tt)
summary(fit1) 

##plot of the model
plot(tt, EU, xlab = "Time", ylab = "European Union Rates", xaxt = "n")
axis(1, at = tt, labels = EuropeanUnion)
abline(fit1, col=3)

##check the residuals
dwtest(fit1) 

##check the residuals
resfit1<- residuals(fit1)
plot(resfit1,xlab="Time", ylab="residuals" , xaxt = "n")
axis(1, at = tt, labels = EuropeanUnion)

EU.ts <- ts(EU, frequency = 12) #transform our EU numerical values to a ts object
ts.plot(EU.ts, type = "o")
## we fit a new linear model with the tslm function
#fit of ts 
fitts<- tslm(EU.ts~trend) 

summary(fitts)
dwtest(fitts)

##########Germany##########

#Importing Dataset
Germany <- read_excel(path = file.choose())
View(Germany)

Ge <- Germany$rates[301:489]
G <- ts(Ge, frequency = 12)

G2 <- Germany$rates[301:456]
GG2 <- ts(G2, frequency = 12)

G3 <- Germany$rates[457:489]
GG3 <- ts(G3, frequency = 12)

t <- (1:189)
t1 <- (1:156)
t2 <- (157:189)


#Visualization
#we are seeing a decreasing trend
plot(Ge, type="l", xlab = "Month", ylab = "Unemployment Rate", main="Germany"
     , pch=16, xaxt="n", cex=0.6, col = "dark blue", lwd = 2)
axis(1, at=c(1,48,95,142,189), labels=Germany$Germany[c(301,348,395,442,489)])
abline(v=156, col="black", lty=2)

#all correlations are positive and significant (decreasing) and this is a 
#confirmation of existence of the trend.
Acf(Ge, lag.max = 36, col = "dark blue", lwd = 1.5, main="Auto Correlation ")


decomposition_result <- decompose(Ge.ts)
plot(decomposition_result)

##to perform a comparison between seasons of different years## 
seasonplot(Ge.ts, ylab="Unemployment Rates", xlab="Month", main="Seasonal plot",
           col=1:20, pch=19)


#Modelling
##Linear Regression(trend + seasonality)

m1<- tslm(GG2~trend + season)
summary(m1)
AIC(m1)
BIC(m1)
accuracy(m1)

pm1 <- forecast(m1, h = 33)

autoplot(pm1) +
  autolayer(G, series = "Original Data", col = 3, lwd = 1) +
  autolayer(fitted(m1), col = 2, lwd = 1, series = "Predicted Values") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_x_continuous(breaks = c(1,4.9,8.8, 12.7, 16.7),
                     labels = Germany$Germany[c(301,348,395,442,489)])+
  ggtitle("Forecasts of unemployment rate using regression") +
  xlab("Month") + ylab("Rate")+
  geom_vline(xintercept = 14, col = "black", lty = 2)
  


f1 <- as.vector(pm1$mean)
RMSE(f1, GG3)

n <- length(GG3)
mape <- (1/n) * sum(abs((GG3 - f1) / GG3) * 100)
mape

#analysis of residuals#

#the value is close to zero so we can conclude that the residuals are
#significantly correlated.
dwtest(m1, alt="two.sided")

checkresiduals(m1)


##Linear Regression(trend + trend^2 + seasonality)
m2<- tslm(GG2~ trend+ I(trend^2) + season)
summary(m2)
accuracy(m2)
AIC(m2)
BIC(m2)

pm2 <- forecast(m2, h = 33)

autoplot(pm2) +
  autolayer(G, series = "Original Data", col = 3, lwd = 1) +
  autolayer(fitted(m2), col = 2, lwd = 1, series = "Predicted Values") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_x_continuous(breaks = c(1,4.9,8.8, 12.7, 16.7),
                     labels = Germany$Germany[c(301,348,395,442,489)])+
  ggtitle("Forecasts of unemployment rate using regression") +
  xlab("Month") + ylab("Rate")+
  geom_vline(xintercept = 14, col = "black", lty = 2)



f2 <- as.vector(pm2$mean)
RMSE(f2, GG3)

n <- length(GG3)
mape <- (1/n) * sum(abs((GG3 - f2) / GG3) * 100)
mape

#analysis of residuals#
dwtest(m2, alt="two.sided")

checkresiduals(m2)

##Exponential Smoothing
autoplot(Ge.ts) + ylab("Unemployment Rate") + xlab("Month")

m3<- ses(GG2, h=5)
round(accuracy(m3), 2)
summary(m3)

autoplot(m3)+
  autolayer(fitted(m3), series="Fitted")+ylab("Unemployment Rates")+xlab("Time")

p <- predict(m3, newdata = GG3)

checkresiduals(m3)

##2.Trend methods (Holt method)

m4<- holt(GG2, h=15)
round(accuracy(m4), 2)
summary(m4)

autoplot(m4)+
  autolayer(fitted(m4), series="Fitted")+ylab("Unemployment Rates")+xlab("Time")


checkresiduals(m4)

m5<- holt(GG2, damped=TRUE, phi=0.9, h=15)
round(accuracy(m5), 2)
summary(m5)

checkresiduals(m5)

autoplot(Ge.ts)+
  autolayer(m4, series="Holt's method", PI=F)+
  autolayer(m5, series="Damped Holt's method", PI=F)


###3.Trend and seasonality methods (Holt-Winters method)

m6<- hw(GG2, seasonal="additive", h = 33)
round(accuracy(m6), 2)
summary(m6)


pm6 <- forecast(m6, h = 33)


autoplot(m6)+
  autolayer(G, series = "Original Data", col = 2, lwd = 1)

autoplot(pm6) +
  autolayer(G, series = "Original Data", col = 3, lwd = 1) +
  autolayer(fitted(m6), col = 2, lwd = 1, series = "Predicted Values") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_x_continuous(breaks = c(1,4.9,8.8, 12.7, 16.7),
                     labels = Germany$Germany[c(301,348,395,442,489)])+
  ggtitle("Forecasts of unemployment rate using Holt-winters") +
  xlab("Month") + ylab("Rate")+
  geom_vline(xintercept = 14, col = "black", lty = 2)


f6 <- as.vector(pm6$mean)
RMSE(f6, G3)

n <- length(GG3)
mape <- (1/n) * sum(abs((GG3 - f6) / GG3) * 100)
mape

checkresiduals(m6)


m7<- hw(GG2, seasonal="multiplicative", h = 33)
round(accuracy(m7), 3)
summary(m7)

pm7 <- forecast(m7, h = 33)

autoplot(pm7) +
  autolayer(G, series = "Original Data", col = 3, lwd = 1) +
  autolayer(fitted(m7), col = 2, lwd = 1, series = "Predicted Values") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_x_continuous(breaks = c(1,4.9,8.8, 12.7, 16.7),
                     labels = Germany$Germany[c(301,348,395,442,489)])+
  ggtitle("Forecasts of unemployment rate using Holt-winters") +
  xlab("Month") + ylab("Rate")+
  geom_vline(xintercept = 14, col = "black", lty = 2)


f7 <- as.vector(pm7$mean)
RMSE(f7, GG3)

n <- length(GG3)
mape <- (1/n) * sum(abs((GG3 - f7) / GG3) * 100)
mape

checkresiduals(m7)

##ARIMA models
##SARIMA
m8<- auto.arima(GG2)
summary(m8)

pm8<- forecast(m8, h = 33)

autoplot(pm8) +
  autolayer(G, series = "Original Data", col = 2, lwd = 1)+
  autolayer(fitted(m8), col=3, lwd = 1)


f8 <- as.vector(pm8$mean)
RMSE(f8, G3)

n <- length(GG3)
mape <- (1/n) * sum(abs((GG3 - f8) / GG3) * 100)
mape

checkresiduals(m8)

Box.test(residuals(m8), lag=36,fitdf=6, type="Ljung")


##GAM

seas <- factor(c(rep(1:12,length(G)/12),1:9))

g1 <- gam(G~s(t)+seas)
summary(g1)
par(mfrow=c(1,2))
plot(g1, se=TRUE)
AIC(g1)


par(mfrow=c(1,1))
plot(as.numeric(G),type = "l", xlab = "Month", ylab = "Rate", main="Unemployment in Germany"
     , pch=16, xaxt="n", cex=0.6, col = "blue", lwd = 1.8)
lines(fitted(g1), col=2, lwd = 1.8)
axis(1, at=c(1,48,95,142,189), labels=Germany$Germany[c(301,348,395,442,489)])
abline(v=156, col="black", lty=2)
legend("topright", legend = c("Original Data", "Predicte Values"), col = c("blue", "red"), lty = 1:1, lwd = 1.8, cex = 0.8)


accuracy(g1)
BIC(g1)

checkresiduals(g1)



##########Italy##########

#Importing Dataset
Italy <- read_excel(path = file.choose())
View(Italy)


autoplot(It.ts, ylab = "Unemployment Rate", xlab = "Month") 


It <- Italy$rates[301:489]
I <- ts(It, frequency = 12)

It1 <- Italy$rates[301:456]
I1 <- ts(It1, frequency = 12)

It2 <- Italy$rates[457:489]
I2 <- ts(It2, frequency = 12)

t <- (1:189)
t1 <- (1:156)
t2 <- (157:189)

#Visualization
plot(It, type="l", xlab = "Month", ylab = "Unemployment Rate", main="Italy"
     , pch=16, xaxt="n", cex=0.6, col = "dark blue", lwd = 2)
axis(1, at=c(1,48,95,142,189), labels=Italy$Italy[c(301,348,395,442,489)])
abline(v=156, col="black", lty=2)


Acf(It, lag.max = 36, col = "dark blue", lwd = 1.5, main="Auto Correlation ")


decomposition_result <- decompose(It.ts)
plot(decomposition_result)

seasonplot(It.ts, ylab="Unemployment Rate", xlab="Month", main="Seasonal plot", col=2:20, pch=19)
axis(1, at=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep"))

#Modelling
##Linear Regression(trend + trend^2 + seasonality)

n1<- tslm(I1~ trend+ I(trend^2) + season)
summary(n1)
accuracy(n1)
AIC(n1)
BIC(n1)

pn1 <- forecast(n1, h = 33)

autoplot(pn1) +
  autolayer(I, series = "Original Data", col = 3, lwd = 1) +
  autolayer(fitted(n1), col = 2, lwd = 1, series = "Predicted Values") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_x_continuous(breaks = c(1,4.9,8.8, 12.7, 16.7),
                     labels = Italy$Italy[c(301,348,395,442,489)])+
  ggtitle("Forecasts of unemployment rate using regression") +
  xlab("Month") + ylab("Rate")+
  geom_vline(xintercept = 14, col = "black", lty = 2)



fn1 <- as.vector(pn1$mean)
RMSE(fn1, I2)

n <- length(I2)
mape <- (1/n) * sum(abs((I2 - fn1) / I2) * 100)
mape

#analysis of residuals#
dwtest(n1, alt="two.sided")

checkresiduals(n1)


##Exponential Smoothing
f2<- ses(It.ts, h=5)
round(accuracy(f2), 2)
summary(f2)

autoplot(f2)+
  autolayer(fitted(f2), series="Fitted")+ylab("Unemployment Rates")+xlab("Time")

checkresiduals(f2)

##2.Trend methods (Holt method)

f3<- holt(It.ts, h=15)
round(accuracy(f3), 2)

checkresiduals(f3)

f4<- holt(It.ts, damped=TRUE, phi=0.9, h=15)
round(accuracy(f4), 2)

autoplot(f3)+
  autolayer(fitted(f3), series="Fitted")+ylab("Unemployment Rates")+xlab("Time")

autoplot(It.ts)+
  autolayer(f3, series="Holt's method", PI=F)+
  autolayer(f4, series="Damped Holt's method", PI=F)

checkresiduals(f4)

###3.Trend and seasonality methods (Holt-Winters method)
n5<- hw(I1, seasonal="additive", h = 33)
round(accuracy(n5), 2)
summary(n5)


pn5 <- forecast(n5, h = 33)




autoplot(pn5) +
  autolayer(I, series = "Original Data", col = 3, lwd = 1) +
  autolayer(fitted(n5), col = 2, lwd = 1, series = "Predicted Values") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_x_continuous(breaks = c(1,4.9,8.8, 12.7, 16.7),
                     labels = Italy$Italy[c(301,348,395,442,489)])+
  ggtitle("Forecasts of unemployment rate using Holt-winters") +
  xlab("Month") + ylab("Rate")+
  geom_vline(xintercept = 14, col = "black", lty = 2)


fn5 <- as.vector(pn5$mean)
RMSE(fn5, I2)

n <- length(I2)
mape <- (1/n) * sum(abs((I2 - fn5) / I2) * 100)
mape

checkresiduals(n5)



n6<- hw(I1, seasonal="multiplicative", h = 33)
round(accuracy(n6), 2)
checkresiduals(n5)
summary(f6)
autoplot(It.ts)+
  autolayer(f5, series="HW additive forecasts", PI=F)+
  autolayer(f6, series="HW multiplicative forecasts", PI=F)


##SARIMA
n8<- auto.arima(I1)
summary(n8)

pn8<- forecast(n8, h = 33)

autoplot(pn8) +
  autolayer(I, series = "Original Data", col = 3, lwd = 1) +
  autolayer(fitted(n8), col = 2, lwd = 1, series = "Predicted Values") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_x_continuous(breaks = c(1,4.9,8.8, 12.7, 16.7),
                     labels = Ialy$Italy[c(301,348,395,442,489)])+
  xlab("Month") + ylab("Rate")+
  geom_vline(xintercept = 14, col = "black", lty = 2)


fn8 <- as.vector(pn8$mean)
RMSE(fn8, I2)

n <- length(I2)
mape <- (1/n) * sum(abs((I2 - fn8) / I2) * 100)
mape

checkresiduals(n8)

Box.test(residuals(m8), lag=36,fitdf=6, type="Ljung")



##Diffusion Models## 
plot(It, type= "b",xlab="Month", ylab="Unemployment",  pch=16, lty=3, xaxt="n", 
     cex=0.6)
axis(1, at=c(1,48,95,142,189), labels=Italy$Italy[c(301,348,395,442,489)])

plot(cumsum(It), type="l")

##Generalized Bass Model(Rectangular shock)
GBMr1<- GBM(It,shock = "rett",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 24,38,-0.1))
summary(GBMr1)

pred<- predict(GBMr1, newx=c(1:189)) 
pred.inst<- make.instantaneous(pred) 

RMSE(pred.inst, It)

checkresiduals(GBMr1)

#plot of fitted model#
plot(It.ts, type= "b",xlab="Month", ylab="Unemployment Rates",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,48,95,142,189), labels=Italy$Italy[c(301,348,395,442,489)])
lines(pred.inst, lwd=2, col=2)

##GAM##

seas <- factor(c(rep(1:12,length(I)/12),1:9))

ng1 <- gam(I~s(t)+seas)
summary(ng1)
par(mfrow=c(1,2))
plot(ng1, se=TRUE)
AIC(ng1)


par(mfrow=c(1,1))
plot(as.numeric(I),type = "l", xlab = "Month", ylab = "Rate", main="Unemployment in Italy"
     , pch=16, xaxt="n", cex=0.6, col = "blue", lwd = 1.8)
lines(fitted(ng1), col=2, lwd = 1.8)
axis(1, at=c(1,48,95,142,189), labels=Italy$Italy[c(301,348,395,442,489)])
abline(v=156, col="black", lty=2)
legend("topright", legend = c("Original Data", "Predicte Values"), col = c("blue", "red"), lty = 1:1, lwd = 1.8, cex = 0.8)


accuracy(ng1)
BIC(ng1)

checkresiduals(ng1)


tt <- (301:489)
seas <- factor(c(rep(1:12,length(It.ts)/12),1:9))



#Values for df should be greater than 1, with df=1 implying a linear fit. Default is df=4
g1 <- gam(It.ts~s(tt)+seas)
par(mfrow=c(2,2))
plot(g1, se=TRUE)
AIC(g1)

plot(It.ts)

g2 <- gam(It.ts~s(tt)) #just consider smoother for tt and japan and it does not consider seasonality. there is no way to smooth seasonality
par(mfrow=c(1,2))
plot(g2, se=TRUE)
summary(g2)
AIC(g2)

#another option with loess (lo)
g2<- gam(I~lo(t)+seas) #local regression with the loess variant

summary(g2)
par(mfrow=c(2,2))
plot(g2, se=TRUE)
AIC(g2)
plot(Ge.ts)
lines(fitted(g2))
#######perform analysis of residuals
tsdisplay(residuals(g1))
aar1<- auto.arima(residuals(g1))

plot(as.numeric(It.ts), type="l")
lines(fitted(aar1)+ fitted(g1), col=4)

# Calculating metrics
combined_fit <- fitted(aar1) + fitted(g1)
combined_residuals <- RS - combined_fit
aic_combined <- AIC(arima(combined_fit, order = c(1, 0, 1)))
bic_combined <- BIC(arima(combined_fit, order = c(1, 0, 1)))
rmse_combined <- sqrt(mean(combined_residuals^2))
mape_combined <- mean(abs(combined_residuals / RS) * 100)

results_combined <- data.frame(
  Model = "Combined (aar1 + g1)",
  AIC = aic_combined,
  BIC = bic_combined,
  RMSE = rmse_combined,
  MAPE = mape_combined
)

print(results_combined)


########### Greece ##########

UnepmGr<- read_excel(path = file.choose())
str(UnepmGr)
#create a variable 'time'
Greece <- UnepmGr$Greece
####
Gr <- UnepmGr$rates[97:285]
Grr <- ts(Gr, frequency = 12)

Gr1 <- UnepmGr$rates[97:252]
Grr1 <- ts(Gr1, frequency = 12)

Gr2 <- UnepmGr$rates[253:285]
Grr2 <- ts(Gr2, frequency = 12)

t <- (1:189)
t1 <- (1:156)
t2 <- (157:189)
####


tt<- 98:285
RS <- UnepmGr$rates[tt]
RS.ts <- ts(RS, frequency = 12)
##make a plot
plot(RS, type="l", xlab = "Time", ylab = "Unemployment Rate", main="Greece"
     , pch=16, xaxt="n", cex=0.6, lwd = 1.8)
      axis(1, at=c(1,48,95,142,188), labels=UnepmGr$Greece[c(97,145,193,241,285)])
##acf of variable "RS"

acf(RS,lag.max =188) #there is a trend

Acf(RS, lag.max = 36,  lwd = 2)

decomposition_result <- decompose(RS.ts)
plot(decomposition_result)

seasonplot(RS.ts, ylab="Unemployment Rate", xlab="Month", main="Seasonal plot", col=2:20, pch=19)
axis(1, at=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep"))


##fit a tslm 

b2<- tslm(Grr1~ trend+ I(trend^2) + season)
summary(b2)
accuracy(b2)
AIC(b2)
BIC(b2)

pb2 <- forecast(b2, h = 33)

autoplot(pb2) +
  autolayer(Grr, series = "Original Data", col = 3, lwd = 1) +
  autolayer(fitted(b2), col = 2, lwd = 1, series = "Predicted Values") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_x_continuous(breaks = c(1,4.9,8.8, 12.7, 16.7),
                     labels = UnepmGr$Greece[c(97,144,191,238,285)])+
  ggtitle("Forecasts of unemployment rate using regression") +
  xlab("Month") + ylab("Rate")+
  geom_vline(xintercept = 14, col = "black", lty = 2)



fb2 <- as.vector(pb2$mean)
RMSE(fb2, Grr2)

n <- length(Grr2)
mape <- (1/n) * sum(abs((Grr2 - fb2) / Grr2) * 100)
mape

#analysis of residuals#
dwtest(b2, alt="two.sided")

checkresiduals(b2)
###



###diffusion models (which may not be good because of their assumptions)
bm_Gr<-BM(RS,display = T) 
summary(bm_Gr)

GBMr1tw<- GBM(RS,shock = "rett",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 24,38,-0.1)) #acting on a longer period of time
summary(GBMr1tw)
pred_bmGr<- predict(bm_Gr, newx=c(1:188)) 
pred.instGr<- make.instantaneous(pred_bmGr)
pred_gbmGr<- predict(GBMr1tw, newx=c(1:188))
pred.instGr2<- make.instantaneous(pred_gbmGr)

GGM_Gr<- GGM(RS, prelimestimates=c(56.339566617, 0.001, 0.01, 0.001481412, 0.129385437))
summary(GGM_Gr)
fit_GGMGr<- fitted(GGM_Gr)
fit_GGMGr_inst<- make.instantaneous(fit_GGMGr)

y_values <- c(RS, pred.instGr, pred.instGr2)
plot(RS, type= "b",xlab="month", ylab="monthly rates",  pch=16, lty=3, cex=0.6, xlim=c(1,188), ylim = range(y_values))
lines(pred.instGr, lwd=2, col=2)
lines(pred.instGr2, lwd=3, col=3)
lines(fit_GGMGr_inst, lwd=4, col=4)
legend("topright", legend = c("Observed", "BM", "GBM", "GGM"), col = c("black", "red", "blue", "green"), lty = c(1, 1, 1, 1), lwd = c(1, 2, 3, 4), cex = 0.8)
#### ARIMA models

Acf(RS,lag.max =188)
Pacf(RS,lag.max =188)
arima1<- Arima(RS, order=c(1,1,0)) 
summary(arima1)

resid1<- residuals(arima1)
tsdisplay(resid1)
plot(RS, xlab = "Time", ylab = "Greece Rates", xaxt = "n")
lines(fitted(arima1), col=2)

auto.a<- auto.arima(RS)
auto.a

autoplot(forecast(auto.a))
checkresiduals(auto.a)

resid2<- residuals(auto.a)
tsdisplay(resid2)
plot(RS, xlab = "Time", ylab = "Greece Rates", xaxt = "n")
plot(forecast(auto.a))
lines(fitted(auto.a), col=2)

actual_values <- window(RS, start = end(RS) - length(fitted(auto.a)) + 1)
fitted_values <- fitted(auto.a)
residuals_auto <- resid2

# Calculate AIC, BIC, RMSE, and MAPE
aic_value <- AIC(auto.a)
bic_value <- BIC(auto.a)
rmse_value <- sqrt(mean(residuals_auto^2))
mape_value <- mean(abs(residuals_auto / actual_values) * 100)

# Create a data frame for results
results_auto <- data.frame(
  Model = "auto.arima",
  AIC = aic_value,
  BIC = bic_value,
  RMSE = rmse_value,
  MAPE = mape_value
)
print(results_auto)




###Regression splines (cubic splines) For all data
#install.packages("splines")
xxx<-seq(min(tt),max(tt),length=188)
xxx
# first model with 13 internal knots
plot(tt,RS, type= "b",xlab="month", ylab="Rate",  pch=16, lty=3, xaxt="n", cex=0.6)
m1<-lm(RS~bs(tt, df=15, degree=3)) 
fit1<-predict(m1, data.frame(x=xxx))
lines(xxx,fit1,col=2,lwd = 2)

#Smoothing splines
plot(tt,RS, type= "b",xlab="month", ylab="Rate",  pch=16, lty=3, xaxt="n", cex=0.6)
s2 <- smooth.spline(tt,RS, lambda=0.00001)
p2<- predict(s2, x=xxx)
lines(p2, col=3, lwd = 2)
legend("topright", legend = c("Observed", "Cubic Splines", "Smoothing Splines"), col = c("black", "red", "green"), lty = c(1, 1, 1), lwd = c(1, 2, 2), cex = 0.8)

#calculating metrics
# Cubic Splines
cubic_splines_pred <- predict(m1, data.frame(x = tt))
cubic_splines_resid <- RS - cubic_splines_pred
# Smoothing Splines
smoothing_splines_pred <- predict(s2, x = tt)$y
smoothing_splines_resid <- RS - smoothing_splines_pred

# Define metrics function
metrics <- function(observed, predicted, df) {
  if (length(unique(observed)) == 1) {
    # Zero variance, return NA for AIC, BIC and 0 for other metrics
    return(c(AIC = NA, BIC = NA, RMSE = 0, MAPE = 0))
  } else {
    aic <- AIC(lm(observed ~ predicted, df = df))
    bic <- BIC(lm(observed ~ predicted, df = df))
    rmse <- sqrt(mean((observed - predicted)^2))
    mape <- mean(abs((observed - predicted) / observed) * 100)
    
    return(c(AIC = aic, BIC = bic, RMSE = rmse, MAPE = mape))
  }
}

# Results for each model
results_cubic_splines <- metrics(RS, cubic_splines_pred, data.frame(x = tt))
results_smoothing_splines <- metrics(RS, smoothing_splines_pred, data.frame(x = tt))

# Create a data frame for results
results_df <- data.frame(
  Model = c("Cubic Splines", "Smoothing Splines"),
  AIC = c(results_cubic_splines["AIC"], results_smoothing_splines["AIC"]),
  BIC = c(results_cubic_splines["BIC"], results_smoothing_splines["BIC"]),
  RMSE = c(results_cubic_splines["RMSE"], results_smoothing_splines["RMSE"]),
  MAPE = c(results_cubic_splines["MAPE"], results_smoothing_splines["MAPE"])
)
print(results_df)


# Smoothing Splines #train and test data
time <- c(t1, t2)
response <- c(Gr1, Gr2)

# Create a vector indicating whether each observation is from the train or test set
data_type <- rep(c("Train", "Test"), times = c(length(t1), length(t2)))

# Fit smooth splines for each data type
spline_fit_train <- smooth.spline(x = t1, y = Gr1)
spline_fit_test <- smooth.spline(x = t2, y = Gr2)

# Create a color vector based on the data_type
colors <- ifelse(data_type == "Train", "red", "blue")

# Plot the original data and fitted splines with different colors for train and test
plot(time, response, col = 1, pch = 19, cex = 0.7, main = "smoothing Splines for Train and Test", xlab = "Time", ylab = "Response Variable")
lines(spline_fit_train, col = "red", lty = 2, lwd = 3)
lines(spline_fit_test, col = "blue", lty = 2, lwd = 3)
legend("topright", legend = c("Train", "Test"), col = c("red", "blue"), lty = 2)

predicted_train <- predict(spline_fit_train, newdata = data.frame(x = t1))$y
predicted_test <- predict(spline_fit_test, newdata = data.frame(x = t2))$y

# Define metrics function
metrics <- function(observed, predicted, df) {
  if (length(unique(observed)) == 1) {
    # Zero variance, return NA for AIC, BIC, and 0 for other metrics
    return(c(AIC = NA, BIC = NA, RMSE = 0, MAPE = 0))
  } else {
    aic <- AIC(lm(observed ~ predicted, df = df))
    bic <- BIC(lm(observed ~ predicted, df = df))
    rmse <- sqrt(mean((observed - predicted)^2))
    mape <- mean(abs((observed - predicted) / observed) * 100)
    
    return(c(AIC = aic, BIC = bic, RMSE = rmse, MAPE = mape))
  }
}

###Trend and seasonality methods (Holt-Winters method)

b6<- hw(Grr1, seasonal="additive", h = 33)
round(accuracy(b6), 2)
summary(b6)


pb6 <- forecast(b6, h = 33)

autoplot(pb6) +
  autolayer(Grr, series = "Original Data", col = 3, lwd = 1) +
  autolayer(fitted(b6), col = 2, lwd = 1, series = "Predicted Values") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_x_continuous(breaks = c(1,4.9,8.8, 12.7, 16.7),
                     labels = UnepmGr$Greece[c(97,144,191,238,285)])+
  ggtitle("Forecasts of unemployment rate using Holt-winters") +
  xlab("Month") + ylab("Rate")+
  geom_vline(xintercept = 14, col = "black", lty = 2)


fb6 <- as.vector(pb6$mean)
RMSE(fb6, Grr2)

n <- length(Grr2)
mape <- (1/n) * sum(abs((Grr2 - fb6) / Grr2) * 100)
mape

checkresiduals(b6)


#### adding features 
dataGr<- read_excel(path = file.choose()) 
summary(dataGr[,c(2,3,4,5)])
head(dataGr)

selected_columns <- c("GDP", "ConsumerPriceIndex", "BalanceOfPayment", "rates")
# Add correlation coefficients
cor_matrix <- cor(dataGr[, selected_columns], use="complete.obs")
corr_text <- round(cor_matrix, digits=2)
text(0.5, 0.5, corr_text, cex=1.2, pos=4)

# Convert to numeric and handle missing values
dataGr[, selected_columns] <- sapply(dataGr[, selected_columns], function(x) as.numeric(as.character(x)))

# Calculate correlation matrix
cor_matrix <- cor(dataGr[, selected_columns], use="complete.obs")

# Plot scatterplot matrix
par(mfrow=c(1,1))


# Add correlation coefficients using corrplot
corrplot(cor_matrix, method="number", type="upper", tl.col="black", diag=FALSE)

###Gradient Boosting

# Set train and test
set.seed(1)
train = sample (1:nrow(dataGr), 0.7*nrow(dataGr))
data.train=dataGr[train ,]
data.test=dataGr[-train ,]

library (gbm)
attach(dataGr)
boost.greece=gbm(rates ~ .-Greece, data=data.train, n.trees=5000, interaction.depth=1, shrinkage=0.01)

#modify the graphical parameters to obtain a better plot
# default vector of parameters
mai.old<-par()$mai
mai.old
#new vector
mai.new<-mai.old
#new space on the left
mai.new[2] <- 2.5 
mai.new
#modify graphical parameters
par(mai=mai.new)
summary(boost.greece, las=1) 
#las=1 horizontal names on y
summary(boost.greece, las=1, cBar=10) 
#cBar defines how many variables
#back to orginal window
par(mai=mai.old)

# test set prediction for every iteration (1:5000)
yhat.boost=predict(boost.greece, newdata=data.test, n.trees=1:5000)

# calculate the error for each iteration
#use 'apply' to perform a 'cycle for' 
# the first element is the matrix we want to use, 2 means 'by column', 
#and the third element indicates the function we want to calculate

err = apply(yhat.boost, 2, function(pred) mean((data.test$rates - pred)^2))
plot(err, type="l")

# error comparison (train e test)
plot(boost.greece$train.error, type="l")
lines(err, type="l", col=2)
#minimum error in test set
best=which.min(err)
abline(v=best, lty=2, col=4)

min(err) #minimum error


plot(boost.greece, i.var=1, n.trees = which.min(err))
plot(boost.greece, i.var=2, n.trees = which.min(err))
plot(boost.greece, i.var=3, n.trees = which.min(err))

yhat_best <- predict(boost.greece, newdata = data.test, n.trees = best)
residuals_best <- data.test$rates - yhat_best

# calculate R-squared
rsquared <- 1 - sum(residuals_best^2) / sum((data.test$rates - mean(data.test$rates))^2)

# calculate ME, RMSE, MAE, MAPE
me <- mean(residuals_best)
rmse <- sqrt(mean(residuals_best^2))
mae <- mean(abs(residuals_best))
mape <- mean(abs(residuals_best / data.test$rates) * 100)
results_df <- data.frame(
  R_squared = rsquared,
  Mean_Error = me,
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)
print(results_df)


# Calculate R-squared, RMSE, MAE, and MAPE for the training set
residuals_train <- data.train$rates - predict(boost.greece, newdata = data.train, n.trees = best)

# Calculate R-squared
rsquared_train <- 1 - sum(residuals_train^2) / sum((data.train$rates - mean(data.train$rates))^2)

# Calculate ME, RMSE, MAE, MAPE for the training set
me_train <- mean(residuals_train)
rmse_train <- sqrt(mean(residuals_train^2))
mae_train <- mean(abs(residuals_train))
mape_train <- mean(abs(residuals_train / data.train$rates) * 100)

# Create a data frame for training set results
results_train <- data.frame(
  Set = "Training",
  R_squared = rsquared_train,
  Mean_Error = me_train,
  RMSE = rmse_train,
  MAE = mae_train,
  MAPE = mape_train
)
print(results_train)


dev.gbm<- (sum((yhat.boost[,best]-data.test$rates)^2))
dev.gbm

# Residuals for the best model on the test set
residuals_gbm <- data.test$rates - yhat.boost[, best]

# Plot the residuals time series
plot(seq_along(residuals_gbm), residuals_gbm, type = "l", col = "blue", xlab = "Index", ylab = "Residuals", main = "Residuals")

# Plot the ACF of residuals
acf_res <- acf(residuals_gbm, main = "Autocorrelation Function of Residuals")


