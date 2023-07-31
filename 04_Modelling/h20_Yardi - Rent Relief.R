library(dplyr)
library(h2o)
library(modeltime.h2o)
library(recipes)
library(timetk)


# Paramaters:
Q_filter <- "Yardi - Rent Relief"
Save_filter <- "Yardi - Rent Relief"
output_directory <- "00_Data/Yardi - Rent Relief/"

sys.source("00_Scripts/functions.R", envir = env <- new.env())


data <- env[["load_data"]](skip = 3) %>% 
  filter(`Date (Year)`> 2020) %>%
  select(-`Date (Month)`, -`Date (Year)`) %>% 
  tidyr::pivot_longer(-Date, names_to = "queue", values_to = "NCO")

Holidays_tbl <- env[["Holidays_tbl"]](2022:2024) 

cleaned_tbl <- data %>% 
  filter(queue == Q_filter) %>% 
  env[["cleaned_tbl"]](holiday = Holidays_tbl,NCO = NCO) %>% 
  select(-queue, -NCO) %>% 
  tidyr::pivot_longer(-Date)
  
cleaned_tbl %>% 
  timetk::plot_time_series(.date_var = Date, 
                           .value = value,
                           .color_var = name, 
                           .smooth = F)


# Prep Tables----
prepared <- env[["prepared_tbl"]](data = cleaned_tbl,
                                     holiday = Holidays_tbl)


current_tbl <- prepared %>%
  filter(!is.na(NCO_trans_clean_drop)) %>% 
  select(-NCO_trans,-NCO_trans_clean)

forecast_tbl <- prepared %>%
  filter(is.na(NCO_trans_clean_drop)) %>% 
  select(-NCO_trans,-NCO_trans_clean)


## Tbl Split for Training ----
Splits <- timetk::time_series_split(current_tbl, assess = "2 months", cumulative = TRUE)

## View Split
Splits %>%
  timetk::tk_time_series_cv_plan() %>%
  timetk::plot_time_series_cv_plan(Date, NCO_trans_clean_drop)


## Recipe for Forecast modeling ----- 

recipe <- recipes::recipe(NCO_trans_clean_drop ~ ., data = rsample::training(Splits)) %>%
  
  # Time Series Signature
  timetk::step_timeseries_signature(Date) %>%
  recipes::step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  
  # Standardization
  recipes::step_normalize(matches("(index.num)|(year)|(yday)")) %>% 
  step_dummy(all_nominal(), one_hot = TRUE)

recipe %>% recipes::prep() %>% recipes::juice() %>% glimpse()



# H2O Modeling ----

h2o.init()

model_spec_h2o <-  modeltime.h2o::automl_reg()%>% 
  parsnip::set_engine(
    engine            = 'h2o',
    max_runtime_secs  = 30,
    max_runtime_secs_per_model = 10,
    max_models =30,
    nfolds = 10,
    exclude_algos = c("DeepLearning"),
    verbosity = NULL,
    seed =   1234
  )

## Models ----
workflow <- workflows::workflow() %>%
  workflows::add_model(model_spec_h2o) %>%
  workflows::add_recipe(recipe) %>%
  parsnip::fit(rsample::training(Splits))

## Models results

accuarcy <- env[["accuracy_tbl"]](workflow, Splits) %>% 
  modeltime::table_modeltime_accuracy()

env[["accuracy_chart"]](workflow = workflow, splits = Splits, current_tbl = current_tbl)

## Refit Data ----

refit_tbl <- env[["refit_tbl"]](workflow = workflow,splits = Splits,current_tbl = current_tbl)

env[["refit_chart"]](refit = refit_tbl,current_tbl = current_tbl, forecast_tbl = forecast_tbl)

# Forecast Complete ----

forecast_complete <- env[["Invert_forecast_tbl"]](refit = refit_tbl,current_tbl = current_tbl,forecast_tbl = forecast_tbl)

# Export 
current_datetime <- format(Sys.time(), "%Y-%m-%d_%H_%M")
filename <- paste(Save_filter, "_h20_", current_datetime, ".csv", sep = "_")
file_path <- paste0(output_directory, filename)

write.csv(forecast_complete, file = file_path)


