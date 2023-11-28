#install.packages('fpp2')
#install.packages('import')
#install.packages('caret')
#install.packages('readr')
#install.packages('tidyverse')
#install.packages('zoo')
#install.packages('gridExtra')
#install.packages('forecast')

library(caret)
library(forecast)
library(gridExtra)
library(import)
library(readr)
library(fpp2)
library(zoo)
library(tidyverse)

# read in data
df <- read_csv('PLOO_water_temperature_2022_datasd.csv', show_col_types = FALSE)
head(df)

# convert the start date string into a POSIXct object
df$datetime_pst <- as.POSIXct(df$datetime_pst, format='%Y-%m-%d %H:%M:%S')
head(df)

## splice data for each depth ##
df_1m <- subset(df, depth_m==1)
df_9m <- subset(df, depth_m==9)
df_20m <- subset(df, depth_m==20)
df_30m <- subset(df, depth_m==30)
df_45m <- subset(df, depth_m==45)
df_60m <- subset(df, depth_m==60)
df_74m <- subset(df, depth_m==74)
df_87m <- subset(df, depth_m==87)

#Create empty data frame
output = data.frame()

# add the summary table for each depth to the output df
depths_df <- list(df_1m, df_9m, df_20m, df_30m, df_45m, df_60m, df_74m, df_87m)
for (x in depths_df) {
  sum = summary(x$value)
  output = rbind(output,sum)
}

# name columns
colnames(output) = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.", "NA's")

# add a cxolumn to label depth
depths <- list(1, 9, 20, 30, 45, 60, 74, 87)
output$Depth <- depths
output <- output[c("Depth","Min.","1st Qu.","Median","Mean","3rd Qu.","Max.", "NA's")]
output

for (x in depths_df) {print(sum(is.na(x)))}   

impute_null_with_average <- function(x, y) {
  if (is.na(x)) {
    previous_index <- which(!is.na(y))[1]
    next_index <- which(!is.na(y))[2]
    if (!is.na(previous_index) && !is.na(next_index)) {
      previous_value <- y[previous_index]
      next_value <- y[next_index]
      return((previous_value + next_value) / 2) 
    }
  }
  return(x) 
}

# apply the function to impute null values
df_1m <- df_1m %>% rowwise() %>% mutate(value = impute_null_with_average(value, df_1m$value))
df_9m <- df_9m %>% rowwise() %>% mutate(value = impute_null_with_average(value, df_9m$value))
df_20m <- df_20m %>% rowwise() %>% mutate(value = impute_null_with_average(value, df_20m$value))
df_30m <- df_30m %>% rowwise() %>% mutate(value = impute_null_with_average(value, df_30m$value))
df_45m <- df_45m %>% rowwise() %>% mutate(value = impute_null_with_average(value, df_45m$value))
df_60m <- df_60m %>% rowwise() %>% mutate(value = impute_null_with_average(value, df_60m$value))
df_74m <- df_74m %>% rowwise() %>% mutate(value = impute_null_with_average(value, df_74m$value))
df_87m <- df_87m %>% rowwise() %>% mutate(value = impute_null_with_average(value, df_87m$value))

for (y in depths_df) {print(sum(is.na(df_9m)))}

# Create time series object
df_1m_ts <- ts(df_1m$value)
df_9m_ts <- ts(df_9m$value)
df_20m_ts <- ts(df_20m$value)
df_30m_ts <- ts(df_30m$value)
df_45m_ts <- ts(df_45m$value)
df_60m_ts <- ts(df_60m$value)
df_74m_ts <- ts(df_74m$value)
df_87m_ts <- ts(df_87m$value)

# plot all depths
autoplot(df_1m_ts, main='Real-time Oceanographic Mooring System: Point Loma Ocean 2022', 
    series='1m depth', ylab='Water Temperature (Degrees Celsius)') + 
    autolayer(df_9m_ts, series='9m depth') +
    autolayer(df_20m_ts, series='20m depth') +
    autolayer(df_30m_ts, series='30m depth') +
    autolayer(df_45m_ts, series='45m depth') +
    autolayer(df_60m_ts, series='60m depth') +
    autolayer(df_74m_ts, series='74m depth') +
    autolayer(df_87m_ts, series='87m depth')

depths_ts <- list(df_1m_ts, df_9m_ts, 
                  df_20m_ts, df_30m_ts, 
                  df_45m_ts, df_60m_ts, 
                  df_74m_ts, df_87m_ts)

n = 1
myplots <- list()
for (x in depths_ts) {
    y <- autoplot(x, main=paste0('RTOMS Readings at ', 
        depths[[n]] ,'m'), ylab='Water Temperature (Degrees Celsius)')
    myplots[[n]] <- y
    n = n +1
}

len <- length(myplots)
nCol <- floor(sqrt(len))
do.call("grid.arrange", c(myplots, ncol=nCol))

# see distribution of water temperatures
par(mfrow=c(2,4))
hist(df_1m$value, main='1m', xlab= 'Water Temperature (Degrees Celsius)')
hist(df_9m$value, main='9m', xlab= 'Water Temperature (Degrees Celsius)')
hist(df_20m$value, main='20m', xlab= 'Water Temperature (Degrees Celsius)')
hist(df_30m$value, main='30m', xlab= 'Water Temperature (Degrees Celsius)')
hist(df_45m$value, main='45m', xlab= 'Water Temperature (Degrees Celsius)')
hist(df_60m$value, main='60m', xlab= 'Water Temperature (Degrees Celsius)')
hist(df_74m$value, main='74m', xlab= 'Water Temperature (Degrees Celsius)')
hist(df_87m$value, main='87m', xlab= 'Water Temperature (Degrees Celsius)')

## 9m ##
daily_avg_1m <- df_1m %>% group_by(date=as.Date(datetime_pst)) %>% 
    summarize(avg_value = mean(value, na.rm=TRUE))
daily_avg_9m <- df_9m %>% group_by(date=as.Date(datetime_pst)) %>% 
    summarize(avg_value = mean(value, na.rm=TRUE))
daily_avg_20m <- df_20m %>% group_by(date=as.Date(datetime_pst)) %>% 
    summarize(avg_value = mean(value, na.rm=TRUE))
daily_avg_30m <- df_30m %>% group_by(date=as.Date(datetime_pst)) %>% 
    summarize(avg_value = mean(value, na.rm=TRUE))
daily_avg_45m <- df_45m %>% group_by(date=as.Date(datetime_pst)) %>% 
    summarize(avg_value = mean(value, na.rm=TRUE))
daily_avg_60m <- df_60m %>% group_by(date=as.Date(datetime_pst)) %>% 
    summarize(avg_value = mean(value, na.rm=TRUE))
daily_avg_74m <- df_74m %>% group_by(date=as.Date(datetime_pst)) %>% 
    summarize(avg_value = mean(value, na.rm=TRUE))
daily_avg_87m <- df_87m %>% group_by(date=as.Date(datetime_pst)) %>% 
    summarize(avg_value = mean(value, na.rm=TRUE))

daily_avg_1m_ts <- ts(daily_avg_1m$avg_value, frequency=365, start=c(2022,1,1))
daily_avg_9m_ts <- ts(daily_avg_9m$avg_value, frequency=365, start=c(2022,1,1))
daily_avg_20m_ts <- ts(daily_avg_20m$avg_value, frequency=365, start=c(2022,1,1))
daily_avg_30m_ts <- ts(daily_avg_30m$avg_value, frequency=365, start=c(2022,1,1))
daily_avg_45m_ts <- ts(daily_avg_45m$avg_value, frequency=365, start=c(2022,1,1))
daily_avg_60m_ts <- ts(daily_avg_60m$avg_value, frequency=365, start=c(2022,1,1))
daily_avg_74m_ts <- ts(daily_avg_74m$avg_value, frequency=365, start=c(2022,1,1))
daily_avg_87m_ts <- ts(daily_avg_87m$avg_value, frequency=365, start=c(2022,1,1))

average_ts <- list(daily_avg_1m_ts, daily_avg_9m_ts, 
                   daily_avg_20m_ts, daily_avg_30m_ts, 
                   daily_avg_45m_ts, daily_avg_60m_ts, 
                   daily_avg_74m_ts, daily_avg_87m_ts)

n = 1
myplots_avg <- list()
for (x in average_ts) {
    y <- autoplot(x, main=paste0('Daily Average at ', depths[[n]] ,'m'), 
                  ylab='Water Temperature (Degrees Celsius)')
    myplots_avg[[n]] <- y
    n = n +1
}

len <- length(myplots_avg)
nCol <- floor(sqrt(len))
do.call("grid.arrange", c(myplots_avg, ncol=nCol))

df_1m_diff <- diff(df_1m_ts, frequency=365,start=c(2022,1,1))
df_9m_diff <- diff(df_9m_ts, frequency=365,start=c(2022,1,1))
df_20m_diff <- diff(df_20m_ts, frequency=365,start=c(2022,1,1))
df_30m_diff <- diff(df_30m_ts, frequency=365,start=c(2022,1,1))
df_45m_diff <- diff(df_45m_ts, frequency=365,start=c(2022,1,1))
df_60m_diff <- diff(df_60m_ts, frequency=365,start=c(2022,1,1))
df_74m_diff <- diff(df_74m_ts, frequency=365,start=c(2022,1,1))
df_87m_diff <- diff(df_87m_ts, frequency=365,start=c(2022,1,1))

#Create empty data frame
diff_df = data.frame()

# add the summary table for each depth to the output df
diff_dfs <- list(df_1m_diff, df_9m_diff, df_20m_diff, df_30m_diff, df_45m_diff, df_60m_diff, df_74m_diff, df_87m_diff)
for (x in diff_dfs) {
  sum = summary(x)
  diff_df = rbind(diff_df,sum)
}

# name columns
colnames(diff_df) = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")

# add a cxolumn to label depth
diff_df$Depth <- depths
diff_df <- diff_df[c("Depth","Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")]
diff_df

par(mfrow=c(2, 4))
# the other fxn to make a nice grid doesnt work with the acf graphs
for (z in diff_dfs) {
    acf(z, main = "ACF of Differenced Time Series")
}

par(mfrow=c(2, 4))
for (z in diff_dfs) {    
    pacf(z, main = "PACF of Differenced Time Series")
}

chosen_ts <- daily_avg_45m_ts

autoplot(chosen_ts, main=paste0('Daily Average at ', 45 ,'m'), ylab='Water Temperature (Degrees Celsius)')

valid <- 7
train <- length(chosen_ts) - valid
train.ts <- window(chosen_ts, end = c(2022, train))
valid.ts <- window(chosen_ts, start = c(2022, train + 1), end = c(2022, train + valid))

length(train.ts)

length(valid.ts)

set.seed(201)
nn_model <- nnetar(train.ts, repeats = 20, p = 7, P = 0, size = 5)
summary(nn_model)

nn_model_pred <- forecast(train.ts, h = 7)

accuracy(nn_model_pred, valid.ts)

df_45m_diff_model <- Arima(df_45m_ts, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1)))
arima_model <- residuals(df_45m_diff_model)
plot(arima_model, main = "Differenced Time Series", ylab = "Differenced Values")

summary(arima_model)

ets_model <- ets(daily_avg_45m_ts)
summary(ets_model)

ets_forecast <- forecast(ets_model, h = 12)
plot(ets_forecast, main = "ETS Model Forecast", ylab = "value", xlab = "time")
# Add the actual time series for comparison
lines(daily_avg_45m_ts, col = "blue", lty = 1)
# Add a legend
legend("topright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1:1)

lin_model <- lm(daily_avg_45m_ts ~ time(daily_avg_45m_ts))
summary(lin_model)

predictions <- predict(lin_model)

predictions <- predict(lin_model)

# Create a data frame for plotting
plot_data <- data.frame(
  Time = time(daily_avg_45m_ts),
  Actual = as.numeric(daily_avg_45m_ts),
  Predicted = as.numeric(predictions)
)

# Plotting
library(ggplot2)

ggplot(plot_data, aes(x = Time)) +
  geom_line(aes(y = Actual), color = 'blue', size = 1, linetype = 'solid') +
  geom_line(aes(y = Predicted), color = 'red', size = 1, linetype = 'dashed') +
  labs(x = 'Time', y = 'Linear Regression Model') +
  theme_minimal()



list_of_models <- c(nn_model, arima_model, ets_model, lin_model)








