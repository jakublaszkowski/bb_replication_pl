# ładuje oszacowania parametrów równań
source("estymacja.R", echo = F)
# usuwa niepotrzebne zmienne
rm(list = setdiff(ls(), c("gpr", "gwr", "cf1r", "cf10r", "data_raw")))

# ładuje komendę, która dokonuje symulacji
source("simul_function.R", echo=F)
# długość opóźnień: musi być 4 lub większe inaczej wyrzuci błąd związany z wyliczaniem, catch-up effect
m_lag <- 4

# Przygowanie danych ---------------------------------------------------------------

data_to_simul_covid <- data_raw %>% 
  # wybieram dane od 2 kwartału 2019 roku 
  filter(data > "2018-09-30") %>% 
  # dodaję wartości 0 dla zmiennych endogenicznych od 2020 roku
  # dzięki temu będą one nadpisywane według symulacji
  mutate(across(.cols = c(gp, gw, cf1, cf10, catch_up),
                .fns = ~ifelse(data > "2019-12-31",
                               yes = 0,
                               no = .)))

# Wykonanie symulacji w danych

data_simulated <-simul_endo_values(data_to_simul_covid,
                  max_lag = m_lag,
                  simul_length = 12)




# Stworzenie wykresu -------------------------------------------------------------

# przeformatowanie danych do formatu long
data_simulated_long <- data_simulated %>%
  # wybranie zmiennych endogenicznych
  select(data, gp, gw, cf1, cf10) %>%
  # przeformatownie do formatu long
  pivot_longer(cols = 2:5, 
               names_to = "endo_value") %>% 
  mutate(type = "simulated")

# wybranie danych obserwowanych, aby dokleić je do wykresu
data_observed_covid <- data_raw %>%
  filter(data > "2019-01-01") %>% 
  # wybranie zmiennych
  select(data, gp, gw, cf1, cf10) %>% 
  # przeformatowanie do formatu long
  pivot_longer(cols = 2:5, 
               names_to = "endo_value") %>% 
  mutate(type = "observed")

# połączenie danych
rbind(
  data_simulated_long,
  data_observed_covid
) %>% 
  # stworzenie wykresu
  ggplot(aes(x=data,y=value, color=type)) +
  geom_line(size=1.5) +
  facet_wrap(~endo_value, scales="free_y") +
  labs(title = "Symulacja ze wszystkimi zmiennymi")
