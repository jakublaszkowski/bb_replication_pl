library(tidyverse)
library(readxl)
library(fpp3)


oczekiwania_inflacyjne <- read_excel(path = "dane/oczekiwania_inflacyjne.xlsx",
           sheet = "zestawienie")

# dane o inflacji biorę z pliku 'zaladowanie_danych.R'

dane_inflacja_to_backcast <- dane_inflacja_clean %>% 
  # wybieram początki kwartałóW
  filter(lubridate::month(TIME_PERIOD) %% 3 == 1) %>% 
  select('data' = TIME_PERIOD, 'HICP' = `All-items HICP`)  %>% 
  mutate(HICP = HICP/lag(HICP, n=4L)*100 - 100,
         data = yearquarter(data))


# zamiana na format do pracy z modelami fabletools
oczekiwania_inflacyjne_tsibble <- oczekiwania_inflacyjne %>% 
  mutate(data = yearquarter(data)) %>% 
  as_tsibble(index = data) %>% 
  left_join(dane_inflacja_to_backcast, by = 'data')   



# zamiana na format odwrotny tj. dane są do tyłu

oczekiwania_inflacyjne_tsibble_reversed <- oczekiwania_inflacyjne_tsibble %>% 
  mutate(reverse_time = rev(row_number())) %>% 
  update_tsibble(index = reverse_time)

#' # Wykres każdego szeregu czasowego

oczekiwania_inflacyjne_tsibble %>%
  as_tibble() %>% 
  mutate(data = as.Date(data)) %>% 
  pivot_longer(cols = 2:5, 
               names_to = "seria", 
               values_to = "oczekiwania",
               values_drop_na = T) %>% 
  ggplot(aes(x=data, y=oczekiwania, color=seria)) +
  geom_line()



#' # model oszacowany na samych danych NBP
  
fit_NBP_only <- oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(!is.na(NBP_4Q)) %>% 
  rename(short_term = NBP_4Q) %>% 
  model(ARIMA(NBP_long_term ~ short_term))

report(fit_NBP_only)

fitted(fit_NBP_only)


gg_tsresiduals(fit_NBP_only)

oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(is.na(NBP_long_term)) %>% 
  rename("short_term" = mBank)  %>% 
  # prognoza
  fabletools::forecast(
  object = fit_NBP_only,
  new_data = .
  ) 


#' # model oszacowany na danych mBanku
#' 

fit_mBank <- oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(!is.na(NBP_long_term)) %>% 
  model(ARIMA(NBP_long_term ~ mBank))

report(fit_mBank)

fitted_data_mbank <- fitted(fit_mBank) %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .fitted)

gg_tsresiduals(fit_mBank) 
  


backcast_data_mbank <- oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(is.na(NBP_long_term)) %>% 
  # prognoza
  fabletools::forecast(
    object = fit_mBank,
    new_data = .
  )  %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .mean)
  

data_model_fitted_mbank <- rbind(fitted_data_mbank, backcast_data_mbank) 

oczekiwania_inflacyjne_tsibble_reversed %>% 
  left_join(data_model_fitted_mbank, by = 'reverse_time') %>% 
  filter(!is.na(mBank)) %>% 
  ggplot(aes(x=data)) +
  geom_line(aes(y=mBank, color = "oczekiwania krótkoterminowe - dane mBanku"), size = 2) +
  geom_line(aes(y=fitted, color = "oczekiwania długoterminowe - dane dopasowane"), size = 2) +
  geom_line(aes(y=NBP_long_term, color = "oczekiwania długoterminowe - ankieta NBP"), size = 2) +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = 'inflacja', x = NULL, color = "Rodzaj danych",
       title = "Backcasting oczekiwań bazujących na oczekiwaniach konsumentów (mBank)") 


#' model oszacowany dla danych bloomberga
#' 

fit_bloomberg <- oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(!is.na(NBP_long_term)) %>% 
  model(ARIMA(NBP_long_term ~ bloomberg))

report(fit_bloomberg)

gg_tsresiduals(fit_bloomberg) 

fitted_data_bloomberg <- fitted(fit_bloomberg) %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .fitted)



backcast_data_bloomberg <- oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(is.na(NBP_long_term)) %>% 
  # prognoza
  fabletools::forecast(
    object = fit_bloomberg,
    new_data = .
  )  %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .mean)


data_model_fitted_bloomberg <- rbind(fitted_data_bloomberg, backcast_data_bloomberg) 

oczekiwania_inflacyjne_tsibble_reversed %>% 
  left_join(data_model_fitted_bloomberg, by = 'reverse_time') %>% 
  filter(!is.na(bloomberg)) %>% 
  ggplot(aes(x=data)) +
  geom_line(aes(y=bloomberg, color = "oczekiwania krótkoterminowe - dane bloomberga"), size = 2) +
  geom_line(aes(y=fitted, color = "oczekiwania długoterminowe - dane dopasowane"), size = 2) +
  geom_line(aes(y=NBP_long_term, color = "oczekiwania długoterminowe - ankieta NBP"), size = 2) +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = 'inflacja', x = NULL, color = "Rodzaj danych",
       title = "Backcasting oczekiwań bazujących na oczekiwaniach bloomberga") 


#' model oszacowany dla danych bloomberga i mbanku
#' 

fit_laczony <- oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(!is.na(NBP_long_term)) %>% 
  model(ARIMA(NBP_long_term ~ bloomberg + mBank))

report(fit_laczony)

gg_tsresiduals(fit_laczony) 

fitted_data_laczony <- fitted(fit_laczony) %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .fitted)



backcast_data_laczony <- oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(is.na(NBP_long_term)) %>% 
  # prognoza
  fabletools::forecast(
    object = fit_laczony,
    new_data = .
  )  %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .mean)


data_model_fitted_laczony <- rbind(fitted_data_laczony, backcast_data_laczony) 

oczekiwania_inflacyjne_tsibble_reversed %>% 
  left_join(data_model_fitted_laczony, by = 'reverse_time') %>% 
  filter(!is.na(mBank)) %>% 
  ggplot(aes(x=data)) +
  geom_line(aes(y=bloomberg, color = "oczekiwania krótkoterminowe - dane bloomberga"), size = 2) +
  geom_line(aes(y=mBank, color = "oczekiwania krótkoterminowe - dane mBanku"), size = 2) +
  geom_line(aes(y=fitted, color = "oczekiwania długoterminowe - dane dopasowane"), size = 2,
            linetype = "dashed") +
  geom_line(aes(y=NBP_long_term, color = "oczekiwania długoterminowe - ankieta NBP"), size = 2) +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = 'inflacja', x = NULL, color = "Rodzaj danych",
       title = "Backcasting oczekiwań bazujących na oczekiwaniach bloomberga i mBanku") 


#' # model oszacowany na danych mBanku, bloomberga oraz inflacji
#' 


fit_laczony_hicp <- oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(!is.na(NBP_long_term)) %>% 
  model(ARIMA(NBP_long_term ~ bloomberg + mBank + HICP))

report(fit_laczony_hicp)

gg_tsresiduals(fit_laczony_hicp) 

fitted_data_laczony_hicp <- fitted(fit_laczony_hicp) %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .fitted)



backcast_data_laczony_hicp <- oczekiwania_inflacyjne_tsibble_reversed %>% 
  filter(is.na(NBP_long_term)) %>% 
  # prognoza
  fabletools::forecast(
    object = fit_laczony_hicp,
    new_data = .
  )  %>% 
  as_tibble() %>% 
  select(reverse_time, "fitted" = .mean)


data_model_fitted_laczony_hicp <- rbind(fitted_data_laczony_hicp,
                                        backcast_data_laczony_hicp) 

oczekiwania_inflacyjne_tsibble_reversed %>% 
  left_join(data_model_fitted_laczony_hicp, by = 'reverse_time') %>% 
  filter(!is.na(mBank)) %>% 
  ggplot(aes(x=data)) +
  geom_line(aes(y=fitted, color = "oczekiwania długoterminowe - dane dopasowane"), size = 2,
            linetype = "solid") +
  geom_line(aes(y=NBP_long_term, color = "oczekiwania długoterminowe - ankieta NBP"), size = 2) +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = 'inflacja', x = NULL, color = "Rodzaj danych",
       title = "Backcasting oczekiwań bazujących na oczekiwaniach bloomberga, mBanku i inflacji HICP") 



#' # połączenie wszystkich prognoz
#' 

left_join(rename(data_model_fitted_mbank, mbank_fit = fitted),
          rename(data_model_fitted_bloomberg, bloomberg_fit = fitted),
          by = "reverse_time") %>% 
  left_join(.,
            rename(data_model_fitted_laczony, laczony_fit = fitted),
            by = "reverse_time") %>% 
  left_join(.,
            rename(data_model_fitted_laczony_hicp, laczony_fit_hicp = fitted),
            by = "reverse_time") %>% 
  left_join(oczekiwania_inflacyjne_tsibble_reversed, ., by = 'reverse_time') %>% 
  ggplot(aes(x=data)) +
  geom_line(aes(y=bloomberg_fit, color = "oczekiwania długoterminowe - dane dopasowane bloomberg"), size = 2) +
  geom_line(aes(y=mbank_fit, color = "oczekiwania długoterminowe - dane dopasowane mBank"), size = 2) +
  geom_line(aes(y=laczony_fit, color = "oczekiwania długoterminowe - dane dopasowane bloomberg+mBank"), size = 2) +
  geom_line(aes(y=laczony_fit, color = "oczekiwania długoterminowe - dane dopasowane bloomberg+mBank+hicp"), size = 2) +
  geom_line(aes(y=NBP_long_term, color = "oczekiwania długoterminowe - ankieta NBP"), size = 2) +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = 'inflacja', x = NULL, color = "Rodzaj danych",
       title = "Backcasting - porównanie") 


# Zapisanie prognozy ------------------------------------------------------

left_join(rename(data_model_fitted_mbank, mbank_fit = fitted),
          rename(data_model_fitted_bloomberg, bloomberg_fit = fitted),
          by = "reverse_time") %>% 
  left_join(.,
            rename(data_model_fitted_laczony, laczony_fit = fitted),
            by = "reverse_time") %>% 
  left_join(.,
            rename(data_model_fitted_laczony_hicp, laczony_fit_hicp = fitted),
            by = "reverse_time") %>% 
  left_join(oczekiwania_inflacyjne_tsibble_reversed, ., by = 'reverse_time') %>%
  as_tibble %>% 
  arrange(data) %>% 
  write_csv2(x=.,
             file = "dane/dane_oczekiwania_podsumowanie.csv")
