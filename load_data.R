library(tidyverse)
library(seasonal)
library(readxl)

# Inflation -------------------------------------------------------------

#' Inflation data (general, food, energy)

data_inflation_raw <-read_csv("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/PRC_HICP_MIDX/M.I15.CP00+CP01+NRG.PL/?format=SDMX-CSV&startPeriod=1996-01&lang=en&label=label_only")

#' Change data to wide format and format dates 

data_inflation_clean <- data_inflation_raw %>% 
  select(coicop, TIME_PERIOD, OBS_VALUE) %>% 
  pivot_wider(names_from = "coicop",
              values_from = "OBS_VALUE") %>% 
  mutate(TIME_PERIOD = as.Date(paste0(TIME_PERIOD, "-01")))

#' Check tail of dataset
tail(data_inflation_clean)

#' Seasonal adjustments
seasadj_CPI_general <- ts(data = data_inflation_clean$`All-items HICP`, frequency = 12,
   start = 1996) %>% 
  seasonal::seas(x = .,
                 arima.model = "(0 2 2)(0 1 1)") %>% 
  seasonal::final(.)

seasadj_CPI_food <- ts(data = data_inflation_clean$`Food and non-alcoholic beverages`, 
                               frequency = 12,
                              start = 1996) %>% 
  seasonal::seas(x = .,
                 arima.model = "(1 1 0)(0 1 1)") %>% 
  seasonal::final(.)

seasadj_CPI_energy <- ts(data = data_inflation_clean$`Energy`, 
                            frequency = 12,
                            start = 1996) %>% 
  seasonal::seas(x = .,
                 arima.model = "(1 1 2)") %>% 
  seasonal::final(.)

#' Prepare data to export to csv format

data_inflation_to_csv <- tibble(
  "TIME_PERIOD" = data_inflation_clean$TIME_PERIOD,
  "All-items HICP" = as.numeric(seasadj_CPI_general),
  "Food and non-alcoholic beverages" = as.numeric(seasadj_CPI_food),
  "Energy" = as.numeric(seasadj_CPI_energy)
) %>% 
  filter(lubridate::month(TIME_PERIOD) %% 3 == 0) %>% 
  mutate(across(.cols = 2:4, .fns = ~./lag(., n=1L)) * 100 - 100)

write_csv2(data_inflation_to_csv, file = "data_input/data_inflation.csv")

# Produktywność -----------------------------------------------------------

#' Labor productivity based on Eurostat
data_productivity_raw <- read_csv("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/NAMQ_10_LP_ULC/Q.I15+PCH_PRE+PCH_SM.NSA+SCA.RLPR_PER.PL/?format=SDMX-CSV&startPeriod=1975-Q1&lang=en&label=both")

#' Clean data for further work
data_productivity_clean <- data_productivity_raw %>% 
  filter(unit == "I15:Index, 2015=100") %>% 
  filter(s_adj == "SCA:Seasonally and calendar adjusted data") %>%
  select(TIME_PERIOD, OBS_VALUE) %>% 
  mutate(rolled_data = NA_integer_)

#' Make rolling average of productivity in the last eight months
for(ind in 8:nrow(data_productivity_clean)){
  data_productivity_clean$rolled_data[ind] <- mean(data_productivity_clean$OBS_VALUE[(ind-7):ind])
}

#' Send data about productivity to csv file
data_productivity_clean %>% 
  select(!OBS_VALUE) %>% 
  # compute growth
  mutate(rolled_data = rolled_data/lag(rolled_data, n=1L) * 100 - 100) %>% 
  write_csv2(., file = "data_input/data_productivity.csv")


# Salaries -----------------------------------------------------------

#' Fristly seasonally adjust the data 
data_salaries <- read_excel(
  path = "data_input/wynagrordzenia_brutto.xlsx",
  sheet= "raw_series",
  skip = 1,
  col_names = c("data", "value")
)

#' Seasonally adjust data
data_salaries_sadj <- ts(data_salaries$value,
   start = c(2001, 3),
   frequency = 4) %>% 
  seasonal::seas(x = .,
                 arima.model = "(0 1 0)(0 1 1)") %>%
  seasonal::final(.)

#' send it to adjusted data series
tibble(
  data = data_salaries$data,
  wynagrodzenia = as.numeric(data_salaries_sadj)
  ) %>% 
  mutate(dyn_wynagrodzenia = wynagrodzenia/lag(wynagrodzenia, n=1L)*100 - 100) %>% 
  write_csv2(., file = "data_input/data_salaries_seasadj.csv")

# VU seria ----------------------------------------------------------------

data_vu <- read_excel(
  path = "dane/wakaty.xlsx",
  sheet= "qtr_raw"
  )

data_vu_adj <- ts(data_vu$'v/u',
   start=c(1995, 1),
   frequency = 4) %>% 
  seasonal::seas(x=.,
                 arima.model="(0 1 0)(0 1 1)") %>% 
  seasonal::final(.)

tibble(
  data = data_vu$`Row Labels`,
  v_u = as.numeric(data_vu_adj)
) %>% 
  write_csv2(., file = "dane/dane_vu_odsezonowane.csv")

## odsezonowanie v ------

data_v_adj <- ts(data_vu$v,
   start=c(1995, 1),
   frequency = 4) %>% 
  seasonal::seas(x=.,
                 arima.model="(0 1 1)(0 1 1)"
                 ) %>% 
  seasonal::final(.)

tibble(
  data = data_vu$`Row Labels`,
  v = as.numeric(data_v_adj)
) %>% 
  write_csv2(., file = "dane/dane_wakaty_odsezonowane.csv")


## odsezonowanie u -----

data_u_adj <- ts(data_vu$u,
                 start=c(1995, 1),
                 frequency = 4) %>% 
  seasonal::seas(x=.,
                 #arima.model="(1 1 0)(1 1 0)"
  ) %>% 
  seasonal::final(.)

tibble(
  data = data_vu$`Row Labels`,
  u = as.numeric(data_u_adj)
) %>% 
  write_csv2(., file = "dane/dane_bezrobocie_odsezonowane.csv")


# Employment data -----------------------------------------------------

#' Time series is splitted between two places
E1 <- read_csv("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/LFSI_EMP_Q_H/1.0/Q.THS_PER.T.EMP_LFS.Y15-64.SA.PL?compress=false&format=csvdata&formatVersion=2.0&c[TIME_PERIOD]=ge:1989-Q1")
E2 <- read_csv("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/LFSI_EMP_Q/1.0/Q.EMP_LFS.SA.T.Y15-64.THS_PER.PL?compress=false&format=csvdata&formatVersion=2.0&c[TIME_PERIOD]=ge:2003-Q1")

E1 %>% 
  filter(as.double(str_extract(TIME_PERIOD, "^\\d{4}")) < 2009) %>% 
  rbind(., E2) %>%
  select("TIME_PERIOD","OBS_VALUE") %>% 
  write_csv2(., file = "data_input/employment_data_seasadj.csv")
