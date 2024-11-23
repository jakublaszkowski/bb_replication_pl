# loads parameter estimates for the equations
source("estimate.R", echo = F)
# removes unnecessary variables
rm(list = setdiff(ls(), c("gpr", "gwr", "cf1r", "cf10r", "data_raw")))

# loads the function that performs the simulation
source("simul_function.R", echo=F)
# lag length: must be 4 or larger; otherwise, it will throw an error related to calculating the catch-up effect
m_lag <- 4
# for how long data should be simulated
lenghth_simul <- 18

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
                                    simul_length = lenghth_simul)

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
  # make to annualised data
  mutate(value = (value/100 + 1)^4 - 1,
         endo_value = case_when(
           endo_value == "gp" ~ "Wzrost cen",
           endo_value == "gw" ~ "Wzrost wynagrodzeń",
           endo_value == "cf1" ~ "Roczne oczekiwania inflacyjne"
         ),
         type = case_when(
           type == "simulated" ~ "Symulacja",
           type == "observed"  ~ "Dane obserwowane"
         )) %>% 
  # create the plot
  ggplot(aes(x=date, y=value, color=type)) +
  geom_line(size=1.5) +
  facet_wrap(~endo_value, scales="free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("#B2182B", "#67A9CF")) +
  labs(title = "Wyniki symulacji kontra dane obserwowane",
       y = "Dynamika r/r", x = NULL, 
       caption = "Roczna dynamika została otrzymana przez zannualizowaie kwartalnych dynamik.\nObliczenia zostały wykonane na danych odsezonowanych",
       color = NULL) +
  theme_bw() +
  theme(
    strip.text = element_text(color = "white", size = 7),
    strip.background = element_rect(fill = "#00695F"),
    axis.text = element_text(color = "black", size = 6),
    axis.title = element_text(color = "black", size = 7),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    plot.title = element_text(size = 9),
    plot.title.position = "plot", 
    plot.caption = element_text(size = 7, color = "black"),
    plot.caption.position = "plot"
  ) +
  theme(
    legend.position = "bottom"
  )

ggsave(filename = "simulation.png", path = "charts", device = "png",
       width = 14*118, height = 7*118, units = "px", dpi = 300)

# Save Data --------------------------------------------------------

rbind(
  data_simulated_long,
  data_observed_covid
) %>%
  # make to annualised data
  mutate(value = (value/100 + 1)^4 - 1,
         endo_value = case_when(
           endo_value == "gp" ~ "Wzrost cen",
           endo_value == "gw" ~ "Wzrost wynagrodzeń",
           endo_value == "cf1" ~ "Roczne oczekiwania inflacyjne"
         ),
         type = case_when(
           type == "simulated" ~ "Symulacja",
           type == "observed"  ~ "Dane obserwowane"
         )) %>% 
  write_csv2(file = "data_output/simul_data.csv")

