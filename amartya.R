library(lubridate)
library(tidyverse)
library(forecast)

###################### This section will go to myMain.R #########################

mypredict = function() {
  ##### Create train and test time-series ######
  # took from @280 piazza
  
  # Filter test dataframe for the month that do need predictions
  # Backtesting starts from March 2011
  start_date = ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date = ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_month = test %>% filter(Date >= start_date & Date < end_date) %>% 
    add_column(Weekly_Pred = NA)

  if (t > 1) {
    train <<- train %>% add_row(new_train)
  }
  
  # Get the dates for test dataframe
  test_dates = unique(test_month$Date)
  num_test_dates = length(test_dates)
  
  # not considering the store and department
  all_stores = unique(test_month$Store)
  num_stores = length(all_stores)
  test_depts = unique(test_month$Dept)
  
  # test dataframe with (num_test_dates, num_stores)
  test_frame = data.frame(
    Date = rep(test_dates, num_stores),
    Store = rep(all_stores, each = num_test_dates)
  )
  
  # Create the structure of training dataframe with 
  # the shape (num_train_dates, num_stores)
  train_dates = unique(train$Date)
  num_train_dates = length(train_dates)
  train_frame = data.frame(
    Date = rep(train_dates, num_stores),
    Store = rep(all_stores, each = num_train_dates)
  )
  
  ##### Perform individual forecasts for each department ######
  for (dept in test_depts) {
    # Extract the current department from the training data
    train_dept_ts = train %>% filter(Dept == dept) %>% 
      select(Store, Date, Weekly_Sales)
    
    # Reformat train_dept_ts so that it has a shape (num_train_dates, num_stores)
    # Each column is a weekly time-series for a store's department
    train_dept_ts = train_frame %>% 
      left_join(train_dept_ts, by = c("Date", "Store")) %>% 
      spread(Store, Weekly_Sales)
    
    # Create a similar dataframe to hold the forecast on the dates in the 
    # testing window
    test_dept_ts = test_frame %>% mutate(Weekly_Sales = 0) %>% 
      spread(Store, Weekly_Sales)

    f_linear = linear_model(train_dept_ts, test_dept_ts)
 
    #shift function for holiday
    if(t==5) f_linear = shift(f_linear)
    
    test_month = update_forecast(test_month, f_linear, dept)
    
  }
  test_month = test_month %>% select(-IsHoliday)
  return(test_month)
}

update_forecast = function(test_month, dept_preds, dept) {
  
  dept_preds = gather(dept_preds, Store, Weekly_Price, -Date, convert = TRUE)
  
  # Obtain the index 
  pred.d.idx = test_month$Dept == dept
  
  # Rearrange dept_preds 
  pred.d = test_month[pred.d.idx, c('Store', 'Date')] %>% 
    left_join(dept_preds, by = c('Store', 'Date'))
  
  test_month$Weekly_Pred[pred.d.idx] = pred.d$Weekly_Price
  
  return(test_month)
}

linear_model = function(train, test) {
  num_forecasts = nrow(test)
  train[is.na(train)] = 0
  
  # Forecast using linear regression model per store
  for (j in 2:ncol(train)) {
    s = ts(train[, j], frequency = 52)
    model = tslm(s ~ trend + season)
    fc = forecast(model, h = num_forecasts)
    test[, j] = as.numeric(fc$mean)
  }
  return(test)
}


shift <- function(test, threshold=1.1, shift=1){
  
  #s <- ts(rep(0,39), frequency=52, start=c(2012,44))
  idx <- week(test$Date) %in% 48:52
  #idx <- cycle(s) %in% 48:52
  holiday <- test[idx, 2:46]
  baseline <- mean(rowMeans(holiday[c(1, 5), ], na.rm=TRUE))
  surge <- mean(rowMeans(holiday[2:4, ], na.rm=TRUE))
  holiday[is.na(holiday)] <- 0
  if(is.finite(surge/baseline) & surge/baseline > threshold){
    shifted.sales <- ((7-shift)/7) * holiday
    shifted.sales[2:5, ] <- shifted.sales[2:5, ] + (shift/7) * holiday[1:4, ]
    shifted.sales[1, ] <- holiday[1, ]
    test[idx, 2:46] <- shifted.sales
  }
  return(test)
}

###################### End of  go to myMain.R #########################



###################### This is evaluation.R #########################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

train <- readr::read_csv('train_ini.csv')
test <- readr::read_csv('test.csv')



num_folds <- 10
wae <- rep(0, num_folds)


for (t in 1:num_folds) {

  test_pred <- mypredict()
  
  # load fold file 
  fold_file <- paste0('fold_', t, '.csv')
  new_train <- readr::read_csv(fold_file, 
                               col_types = cols())
  
  # extract predictions matching up to the current fold
  scoring_tbl <- new_train %>% 
    left_join(test_pred, by = c('Date', 'Store', 'Dept'))
  
  # compute WMAE
  actuals <- scoring_tbl$Weekly_Sales
  preds <- scoring_tbl$Weekly_Pred
  preds[is.na(preds)] <- 0
  weights <- if_else(scoring_tbl$IsHoliday, 5, 1)
  wae[t] <- sum(weights * abs(actuals - preds)) / sum(weights)
}

print(wae)
mean(wae)

###################### end of evaluation.R #########################


