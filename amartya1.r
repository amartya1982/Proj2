library(lubridate)
library(tidyverse)
library(forecast)

###################### This section will go to mymain.R #########################

mypredict = function(){
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  if (t>1){
    train <<- train %>% add_row(new_train)
  }
  
  start_last_year = min(test_current$Date) - 375
  end_last_year = max(test_current$Date) - 350
  tmp_train <- train %>%
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    rename(Weekly_Pred = Weekly_Sales) %>%
    select(-Date, -IsHoliday)
  
  test_current <- test_current %>%
    mutate(Wk = week(Date))
  
  test_pred <- test_current %>%
    left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) %>%
    select(-Wk)
  return(test_pred)
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

