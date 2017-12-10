install.packages("ggplot2")
install.packages('forecast')
install.packages("tseries")
library(ggplot2)
library(forecast)
library(tseries)

setwd("C:/Users/amber/Desktop/Time Series Analysis")

# load data
daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)

head(daily_data)#view variables


daily_data$Date = as.Date(daily_data$dteday)#convert to date

# graph count of bikes rented per day against the date 
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")


count_ts = ts(daily_data[, c('cnt')])#convert to time series object


daily_data$clean_cnt = tsclean(count_ts)# remove outliers and missing values

#graph after cleaning
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

# weekly moving average
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
# montly moving average
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)

# compare weekly and montly moving average to cleaned count

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')


count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
# find seasonal component
decomp = stl(count_ma, s.window="periodic")
#subtract seasonal componenet
deseasonal_cnt <- seasadj(decomp)

plot(decomp)#examine seasonal component

#test for stationarity
adf.test(count_ma, alternative = "stationary")

acf(count_ma, main='')#autocorrelation plot to find p

pacf(count_ma, main='')#partial autocorrelation plot to find q

count_d1 = diff(deseasonal_cnt, differences = 1)#take difference
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

# determine p and q based on differenced data
acf(count_d1, main='ACF for Differenced Series')
pacf(count_d1, main='PACF for Differenced Series')

# fit arima model
auto.arima(deseasonal_cnt, seasonal=FALSE)

#check residuals for model fit
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

#fit with q=7
fit2 = arima(deseasonal_cnt, order=c(1,1,7))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

#forecast for next 30 days
fcast <- forecast(fit2, h=30)
plot(fcast)

#hold out values to compare to results
hold <- window(ts(deseasonal_cnt), start=700)
#fit model without holdout data
fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))
#forecast new model
fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

#fit with seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)
tsdisplay(residuals(fit_w_seasonality), lag.max=45, main='(1,1,1) Model Residuals')

#fit with seasonality
fit_w_seasonality_2 = auto.arima(deseasonal_cnt[-c(700:725)], seasonal=TRUE)
fit_w_seasonality_2

seas_fcast_2 <- forecast(fit_w_seasonality_2, h=30)
plot(seas_fcast_2)
lines(ts(deseasonal_cnt))
tsdisplay(residuals(fit_w_seasonality), lag.max=45, main='(1,1,1) Model Residuals')

# arima with independent variables
# load data
daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)

head(daily_data)#view variables


daily_data$Date = as.Date(daily_data$dteday)#convert to date


count_ts = ts(daily_data[, c('cnt')])#convert to time series object


daily_data$clean_cnt = tsclean(count_ts)# remove outliers and missing values

head(daily_data)
# run linear regression to find variables of value 
names(daily_data)
# partition the data
data_holdout<-daily_data[c(700:731),]
daily_data<-daily_data[c(1:700),]
reg_vars<-daily_data[,c(3, 6:13, 18)]
reg<-lm(clean_cnt~., data=reg_vars)
print(reg)
summary(reg)
#most significant: season, weathersit, hum, and windspeed

ind_data=cbind('season'=daily_data$season,'weather'=daily_data$weathersit, 'humidity'=daily_data$hum, "windspeed"=daily_data$windspeed)
head(ind_data)
str(ind_data)
#create time series
data_ts=ts(daily_data$clean_cnt,frequency=1)
#plot to get initial look
plot(data_ts)
acf(data_ts)
pacf(data_ts)

data_ts_diff<-diff(data_ts)
plot(data_ts_diff)
acf(data_ts_diff)
pacf(data_ts_diff)
adf.test(data_ts_diff, alternative = "stationary")

# build arima 
model_arima= auto.arima(data_ts, seasonal = TRUE)
summary(model_arima)
acf(model_arima$residuals)
pacf(model_arima$residuals)
Box.test(model_arima$residuals, lag = 20, type = c("Ljung-Box"))
forecast_3<-forecast(model_arima, h=30)
plot(forecast_3)
lines(ts(deseasonal_cnt))
tsdisplay(residuals(fit_w_seasonality), lag.max=45, main='(1,1,1) Model Residuals')