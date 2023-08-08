source("estymacja.R", echo = F)
rm(list = setdiff(ls(), c("gpr", "gwr", "cf1r", "cf10r", "data_raw")))

source("simul_function.R", echo=F)


irf_length <- 10
# this must be 3 or smaller
# otherwise it will throw error in computing catch-up effect
m_lag <- 4


# Data prep ---------------------------------------------------------------



data_to_simul_covid <- data_raw %>% 
  # select data from 2q 2019 
  filter(data > "2018-09-30") %>% 
  # create empty values for simulation
  mutate(across(.cols = c(gp, gw, cf1, cf10, catch_up),
                .fns = ~ifelse(data > "2019-12-31",
                               yes = 0,
                               no = .)))


data_simulated <-simul_endo_values(data_to_simul_covid,
                  max_lag = m_lag,
                  simul_length = 12)




# Create plot -------------------------------------------------------------

data_simulated_long <- data_simulated %>% 
  select(data, gp, gw, cf1, cf10) %>% 
  pivot_longer(cols = 2:5, 
               names_to = "endo_value") %>% 
  mutate(type = "simulated")

data_observed_covid <- data_raw %>% 
  # select data from 2q 2019 
  filter(data > "2019-01-01") %>% 
  # create empty values for simulation 
  select(data, gp, gw, cf1, cf10) %>% 
  pivot_longer(cols = 2:5, 
               names_to = "endo_value") %>% 
  mutate(type = "observed")


rbind(
  data_simulated_long,
  data_observed_covid
) %>% 
  ggplot(aes(x=data,y=value, color=type)) +
  geom_line(size=1.5) +
  facet_wrap(~endo_value, scales="free_y") +
  labs(title = "Symulacja ze wszystkimi zmiennymi")
