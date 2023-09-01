source("simulation.R",echo = F)

placeholder_gwr <- gwr$coefficients
placeholder_gpr <- gpr$coefficients

simulated_gw_gp <- data_simulated[, 1:3]


# Funkcja do liczenia wpływu poszczególnego elementu ----------------------

element_to_decomp <- function(name_of_variable){
  
  # tworzę zmienną zapisaną z poszczególnymi elementami
  simul_element_to_decomp <- simul_endo_values(data_to_simul_covid,
                                  max_lag = m_lag,
                                  simul_length = 13)
  
  impact <- simulated_gw_gp[, 2:3] - simul_element_to_decomp[, 2:3]
  names(impact) <- paste0(name_of_variable, "_", names(impact))
  
  return(impact)
}

# Wpływ efektu wakatóW ----------------------------------------------------

# zamieniam zmienne, które wyłączam na zero


gwr$coefficients[['lag1_v']] <- 0 
gwr$coefficients[['lag2_v']] <- 0

impact_v <- element_to_decomp("v")

# przywrócenie poprzednich wartości do równania
gwr$coefficients[['lag1_v']] <- placeholder_gwr[['lag1_v']] 
gwr$coefficients[['lag2_v']] <- placeholder_gwr[['lag2_v']] 


# Wpływ efektu cen energii ------------------------------------------------

gpr$coefficients[['grpe']] <- 0
gpr$coefficients[['lag1_grpe']] <-0

impact_grpe <- element_to_decomp("grpe")

gpr$coefficients[['grpe']] <- placeholder_gpr[['grpe']] 
gpr$coefficients[['lag1_grpe']] <- placeholder_gpr[['lag1_grpe']] 


# Wpływ efektu cen żywności -----------------------------------------------


gpr$coefficients[['grpf']] <- 0
gpr$coefficients[['lag1_grpf']] <-0

impact_grpf <- element_to_decomp("grpf")

gpr$coefficients[['grpf']] <- placeholder_gpr[['grpf']] 
gpr$coefficients[['lag1_grpf']] <- placeholder_gpr[['lag1_grpf']] 


# Wpływ efektu niedoborów -------------------------------------------------

gpr$coefficients[['shortage']] <- 0
gpr$coefficients[['lag1_shortage']] <-0

impact_shortage <- element_to_decomp("shortage")

gpr$coefficients[['shortage']] <- placeholder_gpr[['shortage']] 
gpr$coefficients[['lag1_shortage']] <- placeholder_gpr[['lag1_shortage']] 


# Zebranie danych ---------------------------------------------------------

decomposition_long <- bind_cols(simulated_gw_gp, impact_v, impact_grpe, impact_grpf, impact_shortage) %>% 
  mutate(initial_gp = gp - v_gp - grpe_gp - grpf_gp - shortage_gp,
         initial_gw = gw - v_gw - grpe_gw - grpf_gw - shortage_gw) %>% 
  filter(data >= "2020-01-01") %>% 
  # usuwam dane z symulacji z tabeli
  select(!2:3) %>% 
  # formatuje do long
  pivot_longer(cols = 2:ncol(.)) %>% 
  separate(name, into = c("egzogeniczny", "zmienna"))

simulated_gw_gp_long <- simulated_gw_gp %>% 
  filter(data >= "2020-01-01") %>% 
  pivot_longer(cols = 2:3, names_to = "zmienna")

decomposition_long %>% 
  ggplot(aes(x=data, y=value)) +
  geom_col(aes(fill=egzogeniczny)) +
  geom_line(data = simulated_gw_gp_long) +
  facet_wrap(~zmienna, ncol=1)

