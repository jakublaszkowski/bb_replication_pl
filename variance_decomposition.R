# Load the simulation file
source("simulation.R", echo = F, encoding = "UTF8")

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

simulated_gw_gp_long_to_chart <- simulated_gw_gp_long %>% 
  mutate(value = (value/100 + 1)^4 - 1,
         variable = case_when(
           variable == "gp" ~ "Wzrost cen",
           variable == "gw" ~ "Wzrost wynagrodzeń"
         ))

targets_data <- simulated_gw_gp_long_to_chart %>% 
  mutate(value = case_when(
    variable == "Wzrost cen" ~ 0.025,
    variable == "Wzrost wynagrodzeń" ~ 0.055,
  ))

decomposition_long_to_chart <-  decomposition_long %>% 
  mutate(value = (value/100 + 1)^4 - 1,
         exogenous = case_when(
                      exogenous == "catch_up" ~ "przeszła inflacja",
                      exogenous == "v" ~ "popyt na pracowników",
                      exogenous == "grpe" ~ "ceny energii",
                      exogenous == "grpf" ~ "ceny żywności",
                      exogenous == "shortage" ~ "zatory w globalnych łańcuchach wartości",
                      exogenous == "initial" ~ "warunki początkowe",
                      TRUE ~ exogenous
                      ),
         variable = case_when(
           variable == "gp" ~ "Wzrost cen",
           variable == "gw" ~ "Wzrost wynagrodzeń"
         ) 
         ) 
  
list_exogenous <- c("ceny energii", "ceny żywności", 
                    "zatory w globalnych łańcuchach wartości",
                    "przeszła inflacja",  "popyt na pracowników", "warunki początkowe") 

decomposition_long_to_chart$exogenous <- factor(
  x = decomposition_long_to_chart$exogenous,
  levels = c("warunki początkowe", "popyt na pracowników", "przeszła inflacja", 
             "zatory w globalnych łańcuchach wartości",
              "ceny żywności", "ceny energii"))

for(var_ch in c("cen", "wynagrodzeń")){
  
  targes_data_to_chart <- filter(targets_data ,str_detect(variable, var_ch))
  
  decomposition_long_to_chart %>% 
    filter(str_detect(variable, var_ch)) %>% 
    ggplot(aes(x=date, y=value)) +
    geom_col(aes(fill=exogenous)) +
    #geom_line(data = simulated_gw_gp_long_to_chart, size=1, color = 'black') +
    geom_line(data = targes_data_to_chart, size = 1, linetype = 'dashed', color = 'black') +
    scale_fill_brewer(palette = "RdBu") +
    facet_wrap(~variable, ncol=1) +
    scale_y_continuous(labels = scales::percent_format()) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    theme_bw() +
    theme(
      strip.text = element_text(color = "white", size = 8),
      strip.background = element_rect(fill = "#00695F"),
      axis.text = element_text(color = "black", size = 6),
      axis.title = element_text(color = "black", size = 7),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.position = "bottom",
      plot.title = element_text(size = 9),
      plot.title.position = "plot", 
      plot.caption = element_text(size = 7, color = "black"),
      plot.caption.position = "plot"
    ) +
    labs(
      #title = "Dekompozycja źródeł wariancji w symulowanych cenach energii i żywności", 
      y = "Dynamika r/r", x = NULL, 
      #caption = "Roczna dynamika została otrzymana przez zannualizowaie kwartalnych dynamik.\nObliczenia zostały wykonane na danych odsezonowanych\nPrzerywanymi liniami oznaczono cel inflacyjny oraz cel wzrostu wynagrodzeń",
      #fill = "Egzogeniczne źródła zmian:"
      fill = NULL
    )
  
  ggsave(filename = paste0("var_decomp_", var_ch ,".png"), path = "charts", device = "png",
         width = 14*118, height = 7*118, units = "px", dpi = 300)
}



# Create chart with each variable -----------------------------------------

## Get colors 
colors_palette <- rev(RColorBrewer::brewer.pal(n=6, name = "RdBu"))

decomposition_long_to_chart$exogenous %>% unique

for(i in 1:6){
  for(var_ch in c("cen", "wynagrodzeń")){
  # filter data
  decomposition_long_to_chart %>% 
    filter(str_detect(variable, var_ch)) %>% 
    filter(exogenous %in% list_exogenous[1:i]) %>% 
    ggplot(aes(x=date, y=value)) +
    geom_col(aes(fill=exogenous)) +
    scale_fill_manual(values = rev(colors_palette[1:i])) +
    facet_wrap(~variable, ncol=1) +
    scale_y_continuous(labels = scales::percent_format(), 
                       breaks = seq(-0.05, 0.15, 0.05),
                       limits = c(-0.05, 0.175),
                       expand = expansion(0)) +
      guides(fill=guide_legend(nrow=2, ncol = 3, byrow=TRUE)) +
    theme_bw() +
      theme(
        strip.text = element_text(color = "white", size = 8),
        strip.background = element_rect(fill = "#00695F"),
        axis.text = element_text(color = "black", size = 6),
        axis.title = element_text(color = "black", size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.position = "bottom",
        plot.title = element_text(size = 9),
        plot.title.position = "plot", 
        plot.caption = element_text(size = 7, color = "black"),
        plot.caption.position = "plot"
      ) +
    labs(
      #title = "Dekompozycja źródeł wariancji w symulowanych cenach energii i żywności", 
      y = "Dynamika r/r", x = NULL, 
      #caption = "Roczna dynamika została otrzymana przez zannualizowaie kwartalnych dynamik.\nObliczenia zostały wykonane na danych odsezonowanych",
      #fill = "Egzogeniczne źródła zmian:"
      fill = NULL
    )
  
  ggsave(filename = paste0("var_decomp_no",i,".png"), path = paste0("charts/var_decomp_", var_ch), device = "png",
         width = 14*118, height = 7*118, units = "px", dpi = 300)
  }
}


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

print("Done")
