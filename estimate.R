cat("\014")
rm(list = ls())

library(tidyverse)
library(readxl)
library(car)

# załadowanie danych ---------------------------------------------------------------

# load data stoared in time_series.xlsx
data_raw <- read_excel(path  = "time_series.xlsx",
                       sheet = "series_qq") %>%
  mutate(date = as.Date(date)) %>% 
  # Filter series where there are no observations
  filter(date >= "2002-04-01" & date <= "2023-04-01")

# filter data for pre-pandemic 

data_pre_covid <- data_raw %>% 
  filter(date < "2020-01-01") %>% 
  # add lagged data from 1 to 4
  mutate(across(.cols = 2:ncol(.),
                .fns = ~lag(.,n=1L),
                .names = "lag1_{.col}"),
         across(.cols = 2:ncol(.),
                .fns = ~lag(.,n=2L),
                .names = "lag2_{.col}"),
         across(.cols = 2:ncol(.),
                .fns = ~lag(.,n=3L),
                .names = "lag3_{.col}"),
         across(.cols = 2:ncol(.),
                .fns = ~lag(.,n=4L),
                .names = "lag4_{.col}")
         )

# Wage growth regression --------------------------------------------------

## version with v --------------------------------------------

gwr_2_v_gp <- lm(formula = gw ~
              lag1_gw + lag2_gw +
              lag1_v + lag2_v +
              lag1_catch_up + lag2_catch_up +
              lag1_cf1 + lag2_cf1 +
              lag1_gpty,
            data = data_pre_covid)

summary(gwr_2_v_gp)

# H0: restriction holds
# H1: restriction does not hold
car::linearHypothesis(gwr_2_v_gp,"lag1_gw+lag2_gw=0")
car::linearHypothesis(gwr_2_v_gp,"lag1_v+lag2_v=0")
car::linearHypothesis(gwr_2_v_gp,"lag1_catch_up + lag2_catch_up=0")
car::linearHypothesis(gwr_2_v_gp,"lag1_cf1 + lag2_cf1=0")

# it was chosen for simulation
gwr <- gwr_2_v_gp

# gwr_2_ct_binary <- lm(formula = gw ~
#      lag1_gw + lag2_gw +
#      lag1_v + lag2_v +
#      lag1_catch_up + lag2_catch_up +
#      lag1_catch_up_binary +
#      lag1_cf1 + lag2_cf1 +
#      lag1_gpty,
#    data = data_pre_covid)
# 
# summary(gwr_2_ct_binary)
# 
# gwr <- gwr_2_ct_binary

# Price equation ----------------------------------------------------------

# this version is used for simulation

gpr_1 <- lm(
  formula = gp ~
    lag1_gp +
    gw + lag1_gw +
    grpe + lag1_grpe +
    grpf + lag1_grpf +
    shortage + lag1_shortage +
    lag1_gpty,
  data = data_pre_covid
)

summary(gpr_1)

linearHypothesis(gpr_1, "gw + lag1_gw=1")

gpr <- gpr_1

car::linearHypothesis(gpr,"gw+lag1_gw=0")
car::linearHypothesis(gpr,"grpe + lag1_grpe=0")
car::linearHypothesis(gpr,"grpf + lag1_grpf=0")
car::linearHypothesis(gpr,"shortage + lag1_shortage=0")

# Short run inflation expectations ----------------------------------------

# ta version is included in simulation

cf1r_1 <- lm(
  formula = cf1 ~
    lag1_cf1 +
    gp + lag1_gp +
    I(gp^2),
  data = data_pre_covid)

summary(cf1r_1)

cf1r <- cf1r_1


# linearHypothesis(cf1r_1, "cf10 + lag1_cf10 + gp + lag1_gp= 1")
#' restriction model have a sense only for current relation
#' 
# linearHypothesis(cf1r_1, "cf10 + gp = 1")

# linearHypothesis(cf1r_1, "cf10 + lag1_cf10 = 0")
# linearHypothesis(cf1r_1, "gp + lag1_gp +I(gp^2) = 0")



