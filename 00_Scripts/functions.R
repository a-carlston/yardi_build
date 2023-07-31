
# Misc----
Holidays_tbl <-function(year) {
  timetk::tk_get_holidays_by_year(year) %>%
    filter(holiday_name %>% stringr::str_detect("US_")) %>%
    filter(holiday_name %>% stringr::str_detect("US_CPulaskisBirthday|US_InaugurationDay|US_ElectionDay|US_DecorationMemorialDay", negate = T)) %>%
    dplyr::select(-locale)}


load_data <- function(path = "C:/Users/Andrew Carlston/OneDrive - Sitel/yardi_historical_data.xlsx", 
                      sheet = "Yard_forecast_dump", ...) {
  
  data <- readxl::read_excel(path = path, sheet = sheet, ...)
  
  return(data)
}

cleaned_tbl <- function(data, holiday, NCO) {
  data %>%
    tidyr::complete(Date = seq(min(Date), max(Date), by = "day"), fill = list(NCO = 0)) %>%
    dplyr::mutate(NCO_trans = log1p(NCO)) %>%
    dplyr::left_join(holiday, by = c("Date" = "date")) %>% 
    dplyr::mutate(NCO_trans_clean = timetk::ts_clean_vec(NCO_trans, period = 7)) %>% 
    dplyr::mutate(NCO_trans_clean_drop = case_when(
      (Date >= lubridate::floor_date(Date, "week")) ~ NCO_trans,
      (!is.na(holiday_name)) ~ NCO_trans,
      TRUE ~ NCO_trans_clean)) %>% 
    dplyr::select(-holiday_name)
}


prepared_tbl <- function(data,holiday) {
  
  data  <- data
  
  horizon    <- as.numeric(as.Date('2024-12-31')-as.Date(max(data$Date)))  
  lag_period <- as.numeric(as.Date('2024-12-31')-as.Date(max(data$Date)))
  max <- max(data$Date)
  min <- min(data$Date)
  
  data %>%
    
    tidyr::pivot_wider(names_from = name,values_from = value) %>% 
    
    # Add future window
    bind_rows(
      timetk::future_frame(.data = ., .date_var = Date, .length_out = horizon)
    ) %>% arrange(Date) %>% 
    
    # Add Events
    left_join(holiday, by = c("Date" = "date")) %>% 
    dplyr::mutate(holiday_name = ifelse(is.na(holiday_name), 0, holiday_name)) %>%
    
      # Add Untis/adds (Not used for Yardi)
    # left_join(customer, by = c("Date" = "Date")) %>%
    # dplyr::mutate(Customer_Count = log1p(`Customer_Count`)) %>%
    
    # Format Columns
    rename(holiday_name_event = holiday_name) %>% 
    dplyr::mutate(Date = as.Date(Date))
}

calibration_tbl <- function(workflow, splits) {
  modeltime::modeltime_table(
    workflow
  ) %>%
    modeltime::modeltime_calibrate(new_data = rsample::testing(splits))
}

accuracy_tbl <- function(workflow, splits) {
  calibration_tbl(workflow, splits) %>% 
    modeltime::modeltime_accuracy() 
}  
  
  
accuracy_chart <- function(workflow, splits, current_tbl){
    modeltime::modeltime_table(
      workflow
    ) %>%
      modeltime::modeltime_calibrate(new_data = rsample::testing(splits)) %>% 
      modeltime::modeltime_forecast(
        new_data = rsample::testing(splits),
        actual_data = current_tbl,
        keep_data = T
      ) %>% 
      modeltime::plot_modeltime_forecast(.conf_interval_show = F)
    
  }


refit_tbl <- function(workflow, splits, current_tbl){
  calibration_tbl(workflow, splits) %>% 
    modeltime::modeltime_refit(data = current_tbl)
}

refit_chart <- function(refit, current_tbl, forecast_tbl){
  modeltime::modeltime_forecast(object = refit,
                                new_data = forecast_tbl,
                                actual_data = current_tbl, 
                                keep_data = T) %>% 
    modeltime::plot_modeltime_forecast(.conf_interval_show = F)
}

Invert_forecast_tbl <- function(refit,current_tbl,forecast_tbl){
  
  refit %>%
    modeltime::modeltime_forecast(
      new_data = forecast_tbl,
      actual_data = current_tbl, keep_data = T
    ) %>% 
    # Invert Transformation
    dplyr::mutate(dplyr::across(.value:.conf_hi, .fns = ~ expm1(.))) %>% 
    ungroup()
  
}
