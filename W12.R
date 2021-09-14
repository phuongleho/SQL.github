#W12 R
#Question 1:
#AR(2) model
#simulate 
 x<-numeric()
 x[1]=0.02+0.4*0.05+0.2*0.05+0.01*rnorm(1) #X0= 0.05 we choose, choose any small values
 x[2]=0.02+0.4*x[1]+0.2*0.05+0.01*rnorm(1)
 for (t in 3:500) { x[t]=0.02+0.4*x[t-1]+0.2*x[t-2]+0.01*rnorm(1) }
 plot(c(1:500),x,xlab="t",ylab="x(t)",type="l") #bound
 #ACF
  acf<-numeric()
  for (k in 1:10) { acf[k]=cor(x[1:(500-k)],x[(k+1):500]) } #calculate correlation of observations with do for 10 lags
  barplot(acf,xlab="Lag",ylab="ACF",names.arg=c(1:10),space=50,axis.lty=1) 
  abline(h=2/sqrt(500),lty=3)#Blue dotted line
 #PACF
 > pacf<-numeric()
 > pacf[1]=lm(x[2:500]~x[1:499])$coefficient[2] #order 1
 > pacf[2]=lm(x[3:500]~x[2:499]+x[1:498])$coefficient[3] #x order 2
 > pacf[3]=lm(x[4:500]~x[3:499]+x[2:498]+x[1:497])$coefficient[4]
 > pacf[4]=lm(x[5:500]~x[4:499]+x[3:498]+x[2:497]+x[1:496])$coefficient[5]
  #plot to give pacf => can find lag significant
  
   arima(x,order=c(2,0,0)) #ARMA (p,d,q) function (Ar(2))
  #Calculate AIC
 aic<-numeric()
   for (p in 1:10) { aic[p]=arima(x,order=c(p,0,0))$aic }
   aic
  #Residuals ACF:
  fit=lm(x[2:500]~x[1:499]) (#AR(1))
   r=residuals(fit)
   acfres<-numeric()
   for (k in 1:10) { acfres[k]=cor(r[1:(499-k)],r[(k+1):499]) }
   barplot(acfres,xlab="Lag",ylab="ACF (residuals)",ylim=c(-
                                                              0.1,0.5),names.arg=c(1:10),space=50)
  > abline(h=0)
  > abline(h=2/sqrt(499),lty=3)
  abline(h=-2/sqrt(499),lty=3)
 #Question 2: MA(2)
  #Plot ACF and PACF of the time series observation
  acf(x,lag.max=10)
   pacf(x,lag.max=10)
   #Residuals:
   fit=arima(x,order=c(0,0,2))
    r=residuals(fit)
    acf(r,lag.max=10)
    #Model order selection
  #AIC: Want smallest
    > aic<-numeric()
    > for (q in 1:10) { aic[q]=arima(x,order=c(0,0,q))$aic } #check AIC for each order q
    > aic
    [1] -3138.448 -3156.384 -3156.198 -3155.574 -3154.143 -3154.521 -3155.224
    [8] -3156.138 -3154.365 -3154.762
    AR(P)
    > aic<-numeric()
    > for (p in 1:10) { aic[p]=arima(x,order=c(p,0,0))$aic }