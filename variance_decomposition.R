# Load the simulation file
source("simulation.R", echo = F)

placeholder_gwr <- gwr$coefficients
placeholder_gpr <- gpr$coefficients

simulated_gw_gp <- data_simulated[, 1:3]

# Function to calculate the impact of each individual element ----------------------

element_to_decomp <- function(name_of_variable){
  
  # create a variable saved with individual elements
  simul_element_to_decomp <- simul_endo_values(data_to_simul_covid,
                                               max_lag = m_lag,
                                               simul_length = lenghth_simul)
  
  impact <- simulated_gw_gp[, 2:3] - simul_element_to_decomp[, 2:3]
  names(impact) <- paste0(name_of_variable, "_", names(impact))
  
  return(impact)
}

# Impact of the vacancies effect ---------------------------------------------------

# set variables to zero for deactivation

gwr$coefficients[['lag1_v']] <- 0 
gwr$coefficients[['lag2_v']] <- 0

impact_v <- element_to_decomp("v")

# restore previous values in the equation
gwr$coefficients[['lag1_v']] <- placeholder_gwr[['lag1_v']] 
gwr$coefficients[['lag2_v']] <- placeholder_gwr[['lag2_v']] 


# Impact of the catch up effect ---------------------------------------

gwr$coefficients[['lag1_catch_up']] <- 0
gwr$coefficients[['lag2_catch_up']] <- 0

impact_chatch_up <- element_to_decomp("catch_up")

gwr$coefficients[['lag1_catch_up']] <- placeholder_gwr[['lag1_catch_up']]
gwr$coefficients[['lag2_catch_up']] <- placeholder_gwr[['lag2_catch_up']]

# Impact of the energy prices effect -----------------------------------------------

gpr$coefficients[['grpe']] <- 0
gpr$coefficients[['lag1_grpe']] <-0

impact_grpe <- element_to_decomp("grpe")

gpr$coefficients[['grpe']] <- placeholder_gpr[['grpe']] 
gpr$coefficients[['lag1_grpe']] <- placeholder_gpr[['lag1_grpe']] 


# Impact of the food prices effect -------------------------------------------------

gpr$coefficients[['grpf']] <- 0
gpr$coefficients[['lag1_grpf']] <-0

impact_grpf <- element_to_decomp("grpf")

gpr$coefficients[['grpf']] <- placeholder_gpr[['grpf']] 
gpr$coefficients[['lag1_grpf']] <- placeholder_gpr[['lag1_grpf']] 


# Impact of shortages effect -------------------------------------------------------

gpr$coefficients[['shortage']] <- 0
gpr$coefficients[['lag1_shortage']] <-0

impact_shortage <- element_to_decomp("shortage")

gpr$coefficients[['shortage']] <- placeholder_gpr[['shortage']] 
gpr$coefficients[['lag1_shortage']] <- placeholder_gpr[['lag1_shortage']] 


# Collecting data ------------------------------------------------------------------

decomposition_long <- bind_cols(simulated_gw_gp, impact_v, impact_chatch_up, impact_grpe, impact_grpf, impact_shortage) %>% 
  mutate(initial_gp = gp - v_gp - catch_up_gp - grpe_gp - grpf_gp - shortage_gp,
         initial_gw = gw - v_gw - catch_up_gw - grpe_gw - grpf_gw - shortage_gw) %>% 
  filter(date >= "2020-01-01") %>% 
  # change name to dis
  # remove simulated data from the table
  select(!2:3) %>% 
  # format to long
  pivot_longer(cols = 2:ncol(.)) %>%
  separate(name, into = c("exogenous", "variable"), sep = -3) %>% 
  mutate(variable = str_remove(variable, "_"))

simulated_gw_gp_long <- simulated_gw_gp %>% 
  filter(date >= "2020-01-01") %>% 
  pivot_longer(cols = 2:3, names_to = "variable")

# Chart -----------------------

decomposition_long %>% 
  ggplot(aes(x=date, y=value)) +
  geom_col(aes(fill=exogenous)) +
  geom_line(data = simulated_gw_gp_long, size=2) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~variable, ncol=1) +
  theme_bw()

ggsave(filename = "var_decomp.png", path = "charts", device = "png",
       width = 1500, height = 800, units = "px", dpi = 150)

# Save data -----------------------------------------------------------------------

rbind(decomposition_long,
      mutate(simulated_gw_gp_long, exogenous = "total simulation sum")) %>% 
  mutate(exogenous = case_when(
    exogenous == "v" ~ "vacancies",
    exogenous == "grpe" ~ "energy prices",
    exogenous == "grpf" ~ "food prices",
    exogenous == "shortage" ~ "shortages",
    exogenous == "initial" ~ "initial conditions",
    TRUE ~ exogenous
  )) %>% 
  write_csv2(file = "data_output/var_decomp_data.csv")

