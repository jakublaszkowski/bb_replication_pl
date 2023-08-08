source("estymacja.R", echo = F)
rm(list = setdiff(ls(), c("gpr", "gwr", "cf1r", "cf10r", "data_raw")))

source("simul_function.R", echo=F)


irf_length <- 10
# this must be 3 or smaller
# otherwise it will throw error in computing catch-up effect
m_lag <- 4

# Pirce inflation ---------------------------------------------------------
#' variable -> gp
#' 

# create empty box for variables

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

shortage_sd <- data_raw %>% 
  filter(data < "2020-01-01") %>% 
  pull(shortage) %>% sd()

irf_box_shortage <- irf_box 
irf_box_shortage$shortage[m_lag+1] <- shortage_sd

shortage_irf_raw <- simul_endo_values(irf_box_shortage, max_lag=m_lag, simul_length = irf_length)

shortage_irf_clean <- shortage_irf_raw %>% 
  select(time_index, gp,gw) %>% 
  pivot_longer(cols = 2:3, 
               names_to = "endo_var",
               values_to = "irf_val") %>% 
  mutate(irf_src = "shortage")

## energy price_shock ----------------------

grpe_sd <- data_raw %>% 
  filter(data < "2020-01-01") %>% 
  pull(grpe) %>% sd()

irf_box_grpe <- irf_box 
irf_box_grpe$grpe[m_lag+1] <- grpe_sd

grpe_irf_raw <- simul_endo_values(irf_box_grpe, max_lag=m_lag, simul_length = irf_length)

grpe_irf_clean <- grpe_irf_raw %>% 
  select(time_index, gp,gw) %>% 
  pivot_longer(cols = 2:3, 
               names_to = "endo_var",
               values_to = "irf_val") %>% 
  mutate(irf_src = "energy")

## food price shock ------------------

grpf_sd <- data_raw %>% 
  filter(data < "2020-01-01") %>% 
  pull(grpf) %>% sd()

irf_box_grpf <- irf_box 
irf_box_grpf$grpf[m_lag+1] <- grpf_sd

grpf_irf_raw <- simul_endo_values(irf_box_grpf, max_lag=m_lag, simul_length = irf_length)

grpf_irf_clean <- grpf_irf_raw %>% 
  select(time_index, gp,gw) %>% 
  pivot_longer(cols = 2:3, 
               names_to = "endo_var",
               values_to = "irf_val") %>% 
  mutate(irf_src = "food")


## plot irf  ---------------------------------------------------------------

rbind(
  grpe_irf_clean,
  grpf_irf_clean,
  shortage_irf_clean
) %>% 
  filter(time_index >=0) %>% 
  ggplot(aes(x=time_index, y=irf_val, color=irf_src)) +
  geom_line() +
  geom_point() +
  facet_wrap(~endo_var)



