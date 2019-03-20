

source("http://www.rmetrics.org/Rmetrics.R")    ## this connects to the file and reads the R code there
install.Rmetrics()                                       
setwd ("~/Desktop")                                                

MSFT.day.data=read.csv("TestMSFT20051111.csv",header=T)            ## second file is minute data from some random day

MSFT.price= MSFT.day.data$close    
     
MSFT.dayreturn=diff(MSFT.price)/ MSFT.price[-length(MSFT.price)] ## daily simple return

MSFT.logreturn=diff(log(MSFT.price))  ## Cont compounded return

hist(MSFT.dayreturn)
hist(MSFT.logreturn)
par(mfrow=c(1,2))
hist(MSFT.dayreturn, freq=F,xlim=c(-0.25,0.1))
hist(MSFT.logreturn,freq=F,xlim=c(-0.25,0.1))
par(mfrow=c(1,1))

plot(1:length(MSFT.price),MSFT.price,type="l")
plot(1:length(MSFT.dayreturn),MSFT.dayreturn,type="l")

lines(1:length(MSFT.logreturn),MSFT.logreturn,col="red")
lines(1:length(MSFT.dayreturn),MSFT.dayreturn,col="green")

hist(MSFT.dayreturn, freq=F)
points(density(MSFT.dayreturn),type="l",col="blue")

qqnorm(MSFT.dayreturn)

#calcultes basic stats of logreturn
mean(MSFT.logreturn)
sd(MSFT.logreturn)

library(fBasics) ## This loads the package fBasic

basicStats(MSFT.logreturn) ## all the stats
                            ## some stats can be accessed directly
mean(MSFT.dayreturn)
sd(MSFT.dayreturn)
skewness(MSFT.dayreturn)
kurtosis(MSFT.dayreturn)

basicStats( MSFT.dayreturn)
t.test(MSFT.dayreturn)

basicStats( MSFT.logreturn)
t.test(MSFT.logreturn)

## Normality tests
## Check documentation on  normalTest
?normalTest
normalTest(MSFT.dayreturn,method="jb")

acf(MSFT.dayreturn,lag=15) # Obtain the ACF plot

Box.test(MSFT.dayreturn,lag=10)

Box.test(MSFT.dayreturn,lag=10,type="Ljung")

length(MSFT.dayreturn)

#par(mfcol=c(2,2)) # put 4 plots on one page
plot(MSFT.dayreturn,type='l') # first plot
plot(MSFT.dayreturn[1:(length(MSFT.dayreturn)-1)],MSFT.dayreturn[2:length(MSFT.dayreturn)]) # lag 1 plot
plot(MSFT.dayreturn[1:(length(MSFT.dayreturn)-2)],MSFT.dayreturn[3:length(MSFT.dayreturn)]) # lag 2 plot
acf(MSFT.dayreturn,lag=15)
par(mfcol=c(1,1))

model1=ar( MSFT.dayreturn ,method="mle") # Automatic AR fitting using AIC criterion.
model1             ## AR(2) is specified
names(model1)

plot(model1$resid,type='l')                ## checks residuals
Box.test(model1$resid,lag=10,type='Ljung') 

model1$x.mean # Predicted overal mean value (daily)
[1] 0.0003425036



## Another approach with order specified

model2= arima(MSFT.dayreturn, order=c(3,0,0))
model2

par(mai=c(0.5,0.5,0.5,0.5))
tsdiag(model2)
Box.test(model2$residuals,lag=10,type='Ljung')
plot(model2$residuals,type='l')

## Further analysis:

poly1=c(1,-model2$coef[1:3])
roots=polyroot(poly1)
roots

Mod(roots)
## All roots lie outside the unit circle therefore stationary time series

## Prediction

predict(model2,10) ## predict 10 days ahead

