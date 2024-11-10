# loads parameter estimates for the equations
source("estimate.R", echo = F)
# removes unnecessary variables
rm(list = setdiff(ls(), c("gpr", "gwr", "cf1r", "cf10r", "data_raw")))

# loads the function that performs the simulation
source("simul_function.R", echo=F)
# lag length: must be 4 or larger; otherwise, it will throw an error related to calculating the catch-up effect
m_lag <- 4
# for how long data should be simulated
selected_simulation_lag <- 14

# Data Preparation ---------------------------------------------------------------

data_to_simul_covid <- data_raw %>% 
  # select data from the 2nd quarter of 2019 onward
  filter(date > "2018-09-30") %>% 
  # add 0 values for endogenous variables starting from 2020
  # this allows them to be overwritten according to the simulation
  mutate(across(.cols = c(gp, gw, cf1 
                          #, cf10
  ),
  .fns = ~ifelse(date > "2019-12-31",
                 yes = 0,
                 no = .)))

# Run Simulation on Data

data_simulated <- simul_endo_values(data_to_simul_covid,
                                    max_lag = m_lag,
                                    simul_length = selected_simulation_lag)

# Create Plot -------------------------------------------------------------

# reformat data to long format
data_simulated_long <- data_simulated %>%
  # select endogenous variables
  select(date, gp, gw, cf1) %>%
  # reformat to long format
  pivot_longer(cols = 2:4, 
               names_to = "endo_value") %>% 
  mutate(type = "simulated")

# select observed data to add to the plot
data_observed_covid <- data_raw %>%
  filter(date > "2019-01-01") %>% 
  # select variables
  select(date, gp, gw, cf1) %>% 
  # reformat to long format
  pivot_longer(cols = 2:4, 
               names_to = "endo_value") %>% 
  mutate(type = "observed")

# combine data
rbind(
  data_simulated_long,
  data_observed_covid
) %>% 
  # create the plot
  ggplot(aes(x=date, y=value, color=type)) +
  geom_line(size=1.5) +
  facet_wrap(~endo_value, scales="free_y") +
  labs(title = "Simulation with variable for vacancies")


# Save Data --------------------------------------------------------

rbind(
  data_simulated_long,
  data_observed_covid
) %>% 
  mutate(type = case_when(
    type == "simulated" ~ "simulation",
    type == "observed" ~ "observed data"
  )) %>% 
  write_csv2(file = "data_output/simul_data.csv")

