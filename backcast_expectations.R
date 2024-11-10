library(tidyverse)
library(readxl)
library(fpp3)

# this file is confidential
inflation_expectations_raw <- read_excel(path = "data_input/exp1con_infPL_in_mBank.xlsx",
           sheet = "zestawienie")

#' data is taken from a file 'load_data.R'

#' Here :one must use object data_inflation_clean from 'load_data.R'

data_inflation_to_backcast <- data_inflation_clean %>% 
  # select begining of quarters
  filter(lubridate::month(TIME_PERIOD) %% 3 == 1) %>% 
  select('data' = TIME_PERIOD, 'HICP' = `All-items HICP`)  %>% 
  mutate(HICP = HICP/lag(HICP, n=4L)*100 - 100,
         data = yearquarter(data))

# change folder to work with fabletools
inflation_expectation_tsibble <- inflation_expectations_raw %>% 
  mutate(data = yearquarter(data)) %>% 
  as_tsibble(index = data) %>% 
  left_join(data_inflation_to_backcast, by = 'data')   

# make time series to be in reverse order

inflation_expectations_tsibble_reversed <- inflation_expectation_tsibble %>% 
  mutate(reverse_time = rev(row_number())) %>% 
  update_tsibble(index = reverse_time)

# based on ESI implied expectation

fit_esi <- inflation_expectations_tsibble_reversed %>% 
  filter(!is.na(NBP_long_term)) %>% 
  model(ARIMA(NBP_long_term ~ implied_esi))

fitted_data_esi <- fitted(fit_esi) %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .fitted)

backcast_data_esi <- inflation_expectations_tsibble_reversed %>% 
  filter(is.na(NBP_long_term)) %>% 
  # prognoza
  fabletools::forecast(
    object = fit_esi,
    new_data = .
  )  %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .mean)
  

data_model_fitted_esi <- rbind(fitted_data_esi, backcast_data_esi) 

#' Load into final file

left_join(inflation_expectations_tsibble_reversed, 
          rename(data_model_fitted_esi, esi_fitted = fitted),
          by = "reverse_time") %>% 
  ggplot(aes(x=data)) +
  geom_line(aes(y=esi_fitted, color = "long term inflation expectations - based on esi"), size = 2) +
  geom_line(aes(y=NBP_long_term, color = "long term inflation expectations - based on the NBP's survey"), size = 2) +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = 'inflation', x = NULL, color = "Type of expectations",
       title = "Backcasting - comparison") 


# Load into final file ------------------------------------------------------

left_join(inflation_expectations_tsibble_reversed, 
          rename(data_model_fitted_esi, esi_fitted = fitted),
          by = "reverse_time") %>% 
  as_tibble %>% 
  arrange(data) %>% 
  write_csv2(x=.,
             file = "data_input/data_inflation_expectations.csv")
