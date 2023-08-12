# ładuje oszacowania parametrów równań
source("estymacja.R", echo = F)
# usuwa niepotrzebne zmienne
rm(list = setdiff(ls(), c("gpr", "gwr", "cf1r", "cf10r", "data_raw")))

# ładuje komendę, która dokonuje symulacji
source("simul_function.R", echo=F)
# długość IRF na jaką dokonywana jest symulacja
irf_length <- 10
# długość opóźnień: musi być 4 lub większe inaczej wyrzuci błąd związany z wyliczaniem, catch-up effect
m_lag <- 4

# Pirce inflation ---------------------------------------------------------

# Stworzenie pojemnika na dane
# długość każdego szeregu czasowego obejmuje `m_lag` opóźnień oraz `irf_length` na sam wykres 

irf_box <- tibble(
  time_index = c(-m_lag:0, 1:irf_length),
  gp = vector(mode = 'numeric', length = irf_length + m_lag + 1),
  gw = vector(mode = 'numeric', length = irf_length + m_lag + 1),
  cf1 = vector(mode = 'numeric', length = irf_length + m_lag + 1),
  cf10 = vector(mode = 'numeric', length = irf_length + m_lag + 1),
  catch_up = vector(mode = 'numeric', length = irf_length + m_lag + 1),
  grpe = vector(mode = 'numeric', length = irf_length + m_lag + 1),
  grpf = vector(mode = 'numeric', length = irf_length + m_lag + 1),
  v_u = vector(mode = 'numeric', length = irf_length + m_lag + 1),
  shortage = vector(mode = 'numeric', length = irf_length + m_lag + 1),
  gpty = vector(mode = 'numeric', length = irf_length + m_lag + 1)
)

# insert_shocks --------------------------

## shortage shock ------------------------

# obliczam odchylenie standardowe szoku shortage
shortage_sd <- data_raw %>% 
  filter(data < "2020-01-01") %>% 
  pull(shortage) %>% sd()

# nadpisanie kopi
irf_box_shortage <- irf_box 

# wpisanie w okresie 0 szoku
irf_box_shortage$shortage[m_lag+1] <- shortage_sd

# wykonanie symulacji
shortage_irf_raw <- simul_endo_values(irf_box_shortage, max_lag=m_lag, simul_length = irf_length)

# oczyszczenie danych
shortage_irf_clean <- shortage_irf_raw %>% 
  # śledzę tylko ścieżkę zmiennych endogenicznych
  select(time_index, gp,gw) %>% 
  # formatuje, do formatu long, który potem będzie strawny dla wygenerowania pliku
  pivot_longer(cols = 2:3, 
               names_to = "endo_var",
               values_to = "irf_val") %>% 
  mutate(irf_src = "shortage")

## energy price_shock ----------------------

# obliczam odchylenie standardowe szoku cen energii
grpe_sd <- data_raw %>% 
  filter(data < "2020-01-01") %>% 
  pull(grpe) %>% sd()

# nadpisanie kopi
irf_box_grpe <- irf_box 

# wpisanie w okresie 0 szoku
irf_box_grpe$grpe[m_lag+1] <- grpe_sd

# wykonanie symulacji
grpe_irf_raw <- simul_endo_values(irf_box_grpe, max_lag=m_lag, simul_length = irf_length)

# oczyszczenie danych
grpe_irf_clean <- grpe_irf_raw %>% 
  # śledzę tylko ścieżkę zmiennych endogenicznych
  select(time_index, gp,gw) %>% 
  # formatuje, do formatu long, który potem będzie strawny dla wygenerowania pliku
  pivot_longer(cols = 2:3, 
               names_to = "endo_var",
               values_to = "irf_val") %>% 
  mutate(irf_src = "energy")

## food price shock ------------------

# obliczam odchylenie standardowe szoku cen żywności
grpf_sd <- data_raw %>% 
  filter(data < "2020-01-01") %>% 
  pull(grpf) %>% sd()

# nadpisanie kopi
irf_box_grpf <- irf_box 

# wpisanie w okresie 0 szoku
irf_box_grpf$grpf[m_lag+1] <- grpf_sd

# wykonanie symulacji
grpf_irf_raw <- simul_endo_values(irf_box_grpf, max_lag=m_lag, simul_length = irf_length)

# oczyszczenie danych
grpf_irf_clean <- grpf_irf_raw %>%
  # śledzę tylko ścieżkę zmiennych endogenicznych
  select(time_index, gp,gw) %>% 
  # formatuje, do formatu long, który potem będzie strawny dla wygenerowania pliku
  pivot_longer(cols = 2:3, 
               names_to = "endo_var",
               values_to = "irf_val") %>% 
  mutate(irf_src = "food")


## plot irf  ---------------------------------------------------------------

# łącze dane
rbind(
  grpe_irf_clean,
  grpf_irf_clean,
  shortage_irf_clean
) %>% 
  filter(time_index >=0) %>% 
  # tworzę wykres z 3 szokami
  ggplot(aes(x=time_index, y=irf_val, color=irf_src)) +
  geom_line() +
  geom_point() +
  facet_wrap(~endo_var)



