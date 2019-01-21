#Load libraries
library(ggplot2)
install.packages("naniar")
library(naniar)
library(lubridate)
library(xts)
library(forecast)
library(tseries)
install.packages("imputeTS")
library(imputeTS)
library(car)
library(reshape2)
library(psych)

###########GETTING THE DATA I NEED#############

#Load data
#The data used is from database site UCI with title:"Beijing PM2.5 Data Data Set ", website link: https://archive.ics.uci.edu/ml/datasets/Beijing+PM2.5+Data
beijing = read.csv('PRSA_data_2010.1.1-2014.12.31.csv',header = TRUE)

#Take a look at the summary
summary(beijing)

# The Combined wind direction has for directions : 'cv','NE','NW','SE', for the convenience, let's make it into categories
#(Northwest (NW), W, WNW, NW, NNW and N; 
#northeast (NE), NNE, NE and ENE; 
#southeast (SE), E, ESE, SE, SSE and S; 
#southwest (SW), SSW, SW and WSW
#calm and variable (CV))
beijing$cbwd = as.factor(beijing$cbwd)

###########IS MY DATA FIT FOR USE#############


#There are variables 'year','month','day' represent time, let's transfer them into
#one variable
beijing$time = as.Date(with(beijing, paste(year, month, day,sep="-")), "%Y-%m-%d")
beijing$time = with(beijing, ymd_h(paste(year, month, day, hour, sep= ' ')))



#In summary, it looks like the NA's is concentrated in variable 'variable'

##Data Integration##
#Because the miss data is real record from sensor station, a good way to imputate the missing value is to
#find any other data collected by nearby station

#We found another data collected between 2010-2015 from UCI, website link:https://archive.ics.uci.edu/ml/datasets/PM2.5+Data+of+Five+Chinese+Cities
beijing_add= read.csv('BeijingPM20100101_20151231.csv',header = TRUE)

#We only interested in data collected between 2010-2014
beijing_add = beijing_add[which(beijing_add$year < 2015),]
summary(beijing_add)

#There are four sensor stations for PM2.5 records, 'Dongsihuan','Nongzhanguan','Dongsi','US.Post'
#From the summary, the US post station seems like having same results as our dataset.
#Varify the data to make sure two datasets are matching (we ignore the NA's for this purpose)

all.equal(beijing_add$PM_US.Post[which(complete.cases(beijing_add$PM_US.Post))],beijing$pm2.5[which(complete.cases(beijing$pm2.5))])
#Not indentical,let's look at the reason:
head(beijing_add[which(complete.cases(beijing_add$PM_US.Post)),])
head(beijing[which(complete.cases(beijing$pm2.5)),])

#The daily record in new dataset starts from 23pm previous day, let's fix it by shift the data downward by one.

beijing_add = transform(beijing_add,  PM_US.Post= c(NA, PM_US.Post[-nrow(beijing_add)]))
beijing_add = transform(beijing_add,  PM_Dongsi= c(NA, PM_Dongsi[-nrow(beijing_add)]))
beijing_add = transform(beijing_add,  PM_Dongsihuan= c(NA, PM_Dongsihuan[-nrow(beijing_add)]))
beijing_add = transform(beijing_add,  PM_Nongzhanguan= c(NA, PM_Nongzhanguan[-nrow(beijing_add)]))


temp = data.frame(beijing$pm2.5,beijing_add$PM_US.Post,beijing_add$PM_Dongsi,beijing_add$PM_Dongsihuan,beijing_add$PM_Nongzhanguan)

which(temp$beijing_add.PM_US.Post != temp$beijing.pm2.5)
#The dataset are matching, and ready for integration 


for (i in 1:nrow(temp)) {
  if (is.na(temp$beijing.pm2.5[i]) && !is.na(temp$beijing_add.PM_Dongsi[i])){
    temp$beijing.pm2.5[i]=temp$beijing_add.PM_Dongsi[i]
  }
  if (is.na(temp$beijing.pm2.5[i]) && !is.na(temp$beijing_add.PM_Dongsihuan[i])){
    temp$beijing.pm2.5[i]=temp$beijing_add.PM_Dongsihuan[i]
  }
  if (is.na(temp$beijing.pm2.5[i]) && !is.na(temp$beijing_add.PM_Nongzhanguan[i])){
    temp$beijing.pm2.5[i]=temp$beijing_add.PM_Nongzhanguan[i]
  }
}



summary(beijing)
#The missing data for pm2.5 has decreased to 1893, it's not huge improvement due to
# NA in data from other sensor station are large

#Fill up rest of the missing value by replace with next observation since it's time-series data
# Missing value Imputation: Time-Series specific method: Last Observartion Carried Forward
beijing = na.locf(beijing, option = "locf")
#######Making the data confess#########


##APPROCHING #1 MULTI-VARIABLES MODEL

#Let's start by looking at 2010-2011 data, and see what the pairs are

subset = beijing[beijing$time < as.POSIXct('2011-01-01 00:00:00'),]

# look at the relationships
pairs(~ pm2.5 + DEWP + TEMP + PRES + Iws + Is + Ir + cbwd, data=subset)

# The variables 'Dew point','Temperature', 'Pressure' do not show any interesting relation with 'pm2.5'
# However, 'Cumulated wind speed', 'hourly precipitation', 'Cumulated precipitation' show a pattern of
# with 'pm2.5'

# According to experience as Chinese, the pm2.5 concentration vary on season.

ggplot(subset,aes(x=Iws,y=pm2.5))+
  geom_point()+
  facet_wrap(~month)

# The plots show inverse relationship between cumulated wind speed and pm2.5 concentration
# almost all time in a year except June&July

ggplot(subset,aes(x=Is,y=pm2.5))+
  geom_point()+
  facet_wrap(~month)

# The plots show inverse relationship between cumulated hour of snow and pm2.5 concentration
# only in the month of Jan&Feb$Mar

ggplot(subset,aes(x=Ir,y=pm2.5))+
  geom_point()+
  facet_wrap(~month)

# The plots show inverse relationship between cumulated hour of rain and pm2.5 concentration
# almost all time in a year except Jan&Feb$Nov$Dec


ggplot(subset,aes(x=DEWP,y=pm2.5))+
  geom_point()+
  facet_wrap(~month)
# The plots sugguest there are weak linear relationship between pm2.5 concentration and dew point.

ggplot(subset,aes(x=TEMP,y=pm2.5))+
  geom_point()+
  facet_wrap(~month)
# The plots show there are patterns that low and high temperature usually has low pm2.5 concentration
# High pm2.5 concentration occurs at average temperature mostly

ggplot(subset,aes(x=PRES,y=pm2.5))+
  geom_point()+
  facet_wrap(~month)

# The plots show there are some weak linear pattern between pressure and pm2.5 concentration
#in month Feb,Mar,Oct,Nov,Dec

ggplot(subset,aes(x=cbwd,y=pm2.5))+
  geom_bar(stat='identity')+
  facet_wrap(~month)

#The plots indicates:
# In summer: SE wind direction has strong impact on pm2.5 concentration, cv wind direction has mild impact
# In winter: NW wind direction has strong impact on pm2.5 concentration, cv wind direction has mild impact

### Select variables for regression modelling
beijing.sel <- subset(beijing, select = c(pm2.5,DEWP,TEMP,PRES,cbwd,Iws,Is,Ir))


#Let's devide our dataset into train set and test set
#Let's use data collected in 2010-2013 as train set, and use data collected in 2014 as test set
beijing_train =  beijing[beijing$time < as.POSIXct('2014-01-01 00:00:00'),]
beijing_test =  beijing[beijing$time >= as.POSIXct('2014-01-01 00:00:00'),]


#Fit with multi-linear model

beijing_lmfit = lm(pm2.5~DEWP+TEMP+PRES+cbwd+Iws+Is+Ir, data = beijing_train)
summary(beijing_lmfit)

pairs.panels(beijing.sel, col="red")
#The summary show variables DEWP,TEMP,PRES,cbwdNE,cbwdNW,cbwdSE,Iws,Is,Ir are significant to pm2.5 concentration
#Low p-value mean it is confident to fit this model


#prediction
lmfitpi<-predict(beijing_lmfit,beijing_test,interval="prediction",level = 0.99,type="response")

temp = data.frame(no= beijing_test$No-35054,real = beijing_test$pm2.5,lmfitpi)


  ggplot(data=temp)+
  geom_jitter(aes(x=no,y=real),size=1)+
  geom_line(aes(x=no,y=fit),color="darkorange")+
  labs(x="Time hr", y="PM2.5") +
  xlim(0, nrow(temp))+
  ggtitle("Real and prediction Pm2.5 in 2014 and \n(Multiple regression)")

# From the plot above, we are able to see the prediction line follow the real values when it's normal
# However, it does not has the ability to forecast the extreme case of pm2.5 value
# This limitation can be etismated by removing extreme cases manually(time consumming)
  
  
##APPROCHING #2 TIME-SERIES MODEL


#Convert data into time-series data

pm25_ts = xts(beijing$pm2.5, order.by=beijing$time,frequency= 1)

statsNA(pm25_ts)

# Missing value Imputation: Time-Series specific method: Last Observartion Carried Forward
pm25_ts_fix = na.locf(pm25_ts, option = "locf")
statsNA(pm25_ts_fix)


adf.test(pm25_ts_fix, alternative="stationary", k=0)

plot(pm25_ts_fix, type = "h", main = 'Time-Series Data for PM2.5')

# 1.It is a stationary time series
# 2.There is minimized seasonality
# 2.There is minimized trend effect

pm25_ts.train = xts(beijing_train$pm2.5, order.by=beijing_train$time,frequency = 1)
pm25_ts.test = xts(beijing_test$pm2.5, order.by=beijing_test$time,frequency = 1)

pm25_ts.train.fix = na.locf(pm25_ts.train, option = "locf")
pm25_ts.test.fix = na.locf(pm25_ts.test, option = "locf")


autoplot(acf(pm25_ts.train.fix, plot = FALSE))
autoplot(pacf(pm25_ts.train.fix, plot = FALSE))

# From cutoff in acf and pacf plots, it looks like a Moving average model with q=3 or 5.

ts_fit = auto.arima(pm25_ts.train.fix)
#auto arima shows p=1,q=5


ts.fo = forecast(ts_fit,h = 24*365)
autoplot(ts.fo)
plot(pm25_ts.test)
plot(ts.fo[["mean"]])
lines(ts.fo[["mean"]],col="black",lty=1)