rm(list = ls())

library(tidyverse)
library(readxl)
library(car)
# import danych z stata
library(haven)
# paczka do nakładania restrykcji liniowych
library(restriktor)
# załadowanie danych ---------------------------------------------------------------

# dane ładuje tak jak w pliku empirical_eq_simulations.do
dane_raw_dta <- haven::read_dta(file = "replication_06.13.2023/data/intermediate_data/ImportData.dta")

vector_period <- seq.Date(from = as.Date("1947-01-01"),
         to = as.Date("2023-01-01"),
         by = "3 months")

dane_raw_dta$period <- vector_period


dane_clean_dta <- dane_raw_dta %>% 
  mutate(gp = 400 * (log(CPIAUCSL) - log(lag(CPIAUCSL))),
         gw = 400 * (log(ECIWAG) - log(lag(ECIWAG))),
         gpty = 400 * (log(OPHNFB) - log(lag(OPHNFB))),
         magpty = 0.125 * (gpty + lag(gpty, 1) + lag(gpty, 2) + lag(gpty, 3) +
                             lag(gpty, 4) + lag(gpty, 5) + lag(gpty, 6) +
                             lag(gpty, 7)),
         rpe = CPIENGSL / ECIWAG,
         rpf = CPIUFDSL / ECIWAG,
         grpe = 400 * (log(rpe) - log(lag(rpe))),
         grpf = 400 * (log(rpf) - log(lag(rpf))),
         v_u = VOVERU,
         cf1 = EXPINF1YR,
         cf10 = EXPINF10YR,
         shortage = ifelse(is.na(SHORTAGE), 5, SHORTAGE),
         catch_up = 0.25 * (gp + lag(gp, 1) + lag(gp, 2) + lag(gp, 3)) - lag(cf1, 4),
         dummygw = ifelse(period == as.Date("2001-01-01"), 1.0, 0.0)) %>% 
  # wybieram kolumny
  select(period, gp, v_u, gw,magpty, grpe, grpf, cf1, cf10, shortage, catch_up, CPIENGSL, CPIUFDSL) %>% 
  # wybieram daty 
  filter(period >= "1989-01-01" & period <= "2023-01-01") 


# zmieniam nazewnictwo danych, aby móc dokonywać testowania hipotez statystycznych

dane_do_regresji <- dane_clean_dta %>%  
  # usuwam dane sprzed pandemii
  #filter(period < "2020-01-01") %>% 
  # dodaje zmienne z opóźnieniami 
  # w kolumnach od 2 do 11 znajdują się zmienne używane dalej w modelu
  # wprowadzam opóźnienia od 1 do 4
  mutate(across(.cols = 2:11,
                .fns = ~lag(.,n=1L),
                .names = "lag1_{.col}"),
         across(.cols = 2:11,
                .fns = ~lag(.,n=2L),
                .names = "lag2_{.col}"),
         across(.cols = 2:11,
                .fns = ~lag(.,n=3L),
                .names = "lag3_{.col}"),
         across(.cols = 2:11,
                .fns = ~lag(.,n=4L),
                .names = "lag4_{.col}")
  )




# Wage growth regression --------------------------------------------------

# tak jak w oryginalnej pracy usuwam dane sprzed pandemii

gwr_4 <- lm(formula = gw ~
              lag1_gw + lag2_gw + lag3_gw + lag4_gw +
              lag1_v_u + lag2_v_u + lag3_v_u + lag4_v_u +
              lag1_catch_up + lag2_catch_up + lag3_catch_up + lag4_catch_up +
              lag1_cf1 + lag2_cf1 + lag3_cf1 + lag4_cf1 +
              lag1_magpty,
            data = filter(dane_do_regresji,period < "2020-01-01"))

constraints_gwr_4 <- c(0, 
                       1, 1, 1, 1, # gw
                       0, 0, 0, 0, # v_u
                       0, 0, 0, 0, # catch_up
                       1, 1, 1, 1, # cf1
                       0 # magpty
                       )

gwr_4_restr <-conLM.lm(gwr_4,
         constraints = constraints_gwr_4,
         rhs = 1,
         neq = 1)


summary(gwr_4_restr)

# suma gw
sum(coef(gwr_4_restr)[2:5])
# suma v_u
sum(coef(gwr_4_restr)[6:9])
# suma catch_up
sum(coef(gwr_4_restr)[10:13])
# suma cf1
sum(coef(gwr_4_restr)[14:17])

# Price equation ----------------------------------------------------------

# w kodzie w stacie jest magpty, bez lagu -> i tak dodaję tutaj, mimo że w paperze zapisany jest, 
# że jest bez lagu dodany

gpr_4 <- lm(
  formula = gp ~
    lag1_gp + lag2_gp + lag3_gp + lag4_gp +
    gw + lag1_gw + lag2_gw + lag3_gw + lag4_gw +
    grpe + lag1_grpe + lag2_grpe + lag3_grpe + lag4_grpe +
    grpf + lag1_grpf + lag2_grpf + lag3_grpf + lag4_grpf +
    shortage + lag1_shortage + lag2_shortage + lag3_shortage + lag4_shortage +
    magpty,
  data = dane_do_regresji
  )

summary(gpr_4)

constraints_gpr_4 <- c(0,
                       1, 1, 1, 1, # gp
                       1, 1, 1, 1, 1, # gw.
                       0, 0, 0, 0, 0, # grpe
                       0, 0, 0, 0, 0, # grpf
                       0, 0, 0, 0, 0, # shortage
                       0 # magpty
                       )

gpr_4_restr <-conLM.lm(gpr_4,
                       constraints = constraints_gpr_4,
                       rhs = 1,
                       neq = 1)

summary(gpr_4_restr)


# suma gp
sum(coef(gpr_4_restr)[2:5])
# suma gw
sum(coef(gpr_4_restr)[6:10])
# suma grpe
sum(coef(gpr_4_restr)[11:15])
# suma grpf
sum(coef(gpr_4_restr)[16:20])

                                  
# Short run inflation expectations ----------------------------------------

# usuwam stałą z równania jak w org w stacie
# ograniczam próbę do danych sprzed pandemii jak w org w stacie

cf1r_4 <- lm(
  formula = cf1 ~
    lag1_cf1 + lag2_cf1 + lag3_cf1 + lag4_cf1 +
    cf10 + lag1_cf10 + lag2_cf10 + lag3_cf10 + lag4_cf10 +
    gp + lag1_gp + lag2_gp + lag3_gp + lag4_gp
  -1,
  data = filter(dane_do_regresji,period < "2020-01-01")
)

summary(cf1r_4)

constraints_cf1r_4 <- c(1, 1, 1, 1, # cf1
                        1, 1, 1, 1, 1, # cf10
                        1, 1, 1, 1, 1 # gp
                        )

cf1r_4_restr <-conLM.lm(cf1r_4,
                       constraints = constraints_cf1r_4 ,
                       rhs = 1,
                       neq = 1)
summary(cf1r_4_restr)

# suma cf1
sum(coef(cf1r_4_restr)[1:4])
# suma cf10
sum(coef(cf1r_4_restr)[5:9])
# suma gp
sum(coef(cf1r_4_restr)[10:14])




# Long run inflation exp --------------------------------------------------


cf10r_4 <- lm(
  formula = cf10 ~
    lag1_cf10 + lag2_cf10 + lag3_cf10 + lag4_cf10 +
    gp + lag1_gp + lag2_gp + lag3_gp + lag4_gp - 1,
  data = filter(dane_do_regresji,period < "2020-01-01")
  )

summary(cf10r_4)

constraints_cf10r_4 <- rep(1, 9)

cf10r_4_restr <-conLM.lm(cf10r_4,
                        constraints = constraints_cf10r_4 ,
                        rhs = 1,
                        neq = 1)
summary(cf10r_4_restr)

# suma cf0
sum(coef(cf10r_4_restr)[1:4])
# suma gp
sum(coef(cf10r_4_restr)[5:9])

