library(tidyverse)
library(seasonal)
library(readxl)
# Inflacja -------------------------------------------------------------

#' Dane o inflacji (ogólne, żywność, energia)

dane_inflacja_raw <-read_csv("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/PRC_HICP_MIDX/M.I15.CP00+CP01+NRG.PL/?format=SDMX-CSV&startPeriod=1996-01&endPeriod=2023-05&lang=en&label=label_only")

#' Przekształcenie danych do formatu wide, oraz formatuje daty
dane_inflacja_clean <- dane_inflacja_raw %>% 
  select(coicop, TIME_PERIOD, OBS_VALUE) %>% 
  pivot_wider(names_from = "coicop",
              values_from = "OBS_VALUE") %>% 
  mutate(TIME_PERIOD = as.Date(paste0(TIME_PERIOD, "-01")))

dane_inflacja_clean 

odsezonowane_CPI_ogolem <- ts(data = dane_inflacja_clean$`All-items HICP`, frequency = 12,
   start = 1996) %>% 
  seasonal::seas(x = .,
                 arima.model = "(0 2 2)(0 1 1)") %>% 
  seasonal::final(.)

odsezonowane_CPI_food <- ts(data = dane_inflacja_clean$`Food and non-alcoholic beverages`, 
                               frequency = 12,
                              start = 1996) %>% 
  seasonal::seas(x = .,
                 arima.model = "(1 1 0)(0 1 1)") %>% 
  seasonal::final(.)

odsezonowane_CPI_energy <- ts(data = dane_inflacja_clean$`Energy`, 
                            frequency = 12,
                            start = 1996) %>% 
  seasonal::seas(x = .,
                 arima.model = "(1 1 2)") %>% 
  seasonal::final(.)

dane_inflacja_to_csv <- tibble(
  "TIME_PERIOD" = dane_inflacja_clean$TIME_PERIOD,
  "All-items HICP" = as.numeric(odsezonowane_CPI_ogolem),
  "Food and non-alcoholic beverages" = as.numeric(odsezonowane_CPI_food),
  "Energy" = as.numeric(odsezonowane_CPI_energy)
) %>% 
  filter(lubridate::month(TIME_PERIOD) %% 3 == 0) %>% 
  mutate(across(.cols = 2:4, .fns = ~./lag(., n=1L)) * 100 - 100)

write_csv2(dane_inflacja_to_csv, file = "dane/dane_inflacja.csv")


# Produktywność -----------------------------------------------------------

#' Dane o produktywności pracy wyliczane przez Eurostat


dane_produktywnosc_pracy <- read_csv("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/NAMQ_10_LP_ULC/Q.I15+PCH_PRE+PCH_SM.NSA+SCA.RLPR_PER.PL/?format=SDMX-CSV&startPeriod=1975-Q1&endPeriod=2023-Q1&lang=en&label=both")

dane_produktywnosc_pracy_long <- dane_produktywnosc_pracy %>% 
  filter(unit == "I15:Index, 2015=100") %>% 
  filter(s_adj == "SCA:Seasonally and calendar adjusted data") %>%
  select(TIME_PERIOD, OBS_VALUE) 

for(ind in 8:nrow(dane_produktywnosc_pracy_long)){
  
  dane_produktywnosc_pracy_long$OBS_VALUE[ind] <- mean(dane_produktywnosc_pracy_long$OBS_VALUE[(ind-7):ind])

  }

dane_produktywnosc_pracy_long$OBS_VALUE[1:7] <- NA_integer_

dane_produktywnosc_pracy_long %>% 
  mutate(OBS_VALUE = OBS_VALUE/lag(OBS_VALUE, n=1L) * 100 - 100) %>% 
  write_csv2(., file = "dane/dane_produktywnosc.csv")


# Wynagrodzenia -----------------------------------------------------------

data_salaries <- read_excel(
  path = "dane/wynagrordzenia_brutto.xlsx",
  sheet= "raw_series",
  col_names = c("data", "value")
)

data_salaries_sadj <- ts(data_salaries$value,
   start = c(2001, 3),
   frequency = 4) %>% 
  seasonal::seas(x = .,
                 arima.model = "(0 1 0)(0 1 1)") %>%
  seasonal::final(.)

tibble(
  data = data_salaries$data,
  wynagrodzenia = as.numeric(data_salaries_sadj)
  ) %>% 
  mutate(dyn_wynagrodzenia = wynagrodzenia/lag(wynagrodzenia, n=1L)*100 - 100) %>% 
  write_csv2(., file = "dane/dane_wynagrodzenia_odsezonowane.csv")

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
                 # todo: nadpisać model arimy
                 #arima.model="(0 1 0)(0 1 1)"
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
                 # todo: nadpisać model arimy
                 #arima.model="(0 1 0)(0 1 1)"
  ) %>% 
  seasonal::final(.)

tibble(
  data = data_vu$`Row Labels`,
  u = as.numeric(data_u_adj)
) %>% 
  write_csv2(., file = "dane/dane_bezrobocie_odsezonowane.csv")

