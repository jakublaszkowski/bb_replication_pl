rm(list = ls())

library(tidyverse)
library(readxl)
library(car)

# załadowanie danych ---------------------------------------------------------------

# ładuje dane z pliku szeregi_czasowe.xlsx, na innych komputerach trzeba to zmieniać
data_raw <- read_excel(path  = "szeregi_czasowe.xlsx",
           sheet = "szeregi_qq") %>%
  mutate(data = as.Date(data)) %>% 
  # wyczyszczam próbę z okresów, gdzie nie ma żadnej obserwacji.
  filter(!if_any(.cols = everything(),
                .fns = is.na))

# odfiltrowuję dane, na próbę sprzed pandemii

data_pre_covid <- data_raw %>% 
  filter(data < "2020-01-01") %>% 
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

gwr_1 <- lm(formula = gw ~
              lag(gw, n=1L) +
              lag(v_u, n=1L)  +
              lag(catch_up, n=1L) + 
              lag(cf1, n=1L) +   
              lag(gpty, n=1L),
            data = data_pre_covid)

summary(gwr_1)

# to równanie wybrałem ostatecznie do symulacji

gwr_2 <- lm(formula = gw ~
              lag1_gw + lag2_gw +
              lag1_v_u + lag2_v_u +
              lag1_catch_up + lag2_catch_up +
              lag1_cf1 + lag2_cf1 +
              lag1_gpty,
            data = data_pre_covid)


summary(gwr_2)

gwr <- gwr_2

# sprawdzane nałożenie restrykcji ma uzasadnienie
# H0: prawdziwa jest przedstawiana w równaniu restrykcja
# H1: nieprawdziwa jest przedstawiana w równaniu restrykcja
car::linearHypothesis(gwr_2,"lag1_gw+lag2_gw=0")
car::linearHypothesis(gwr_2,"lag1_v_u+lag2_v_u=0")
car::linearHypothesis(gwr_2,"lag1_catch_up + lag2_catch_up=0")
car::linearHypothesis(gwr_2,"lag1_cf1 + lag2_cf1=0")

gwr_3 <- lm(formula = gw ~
              lag(gw, n=1L) + lag(gw, n=2L) + lag(gw, n=3L) + 
              lag(v_u, n=1L) + lag(v_u, n=2L) + lag(v_u, n=3L) + 
              lag(catch_up, n=1L) + lag(catch_up, n=2L) + lag(catch_up, n=3L) + 
              lag(cf1, n=1L) + lag(cf1, n=2L) + lag(cf1, n=3L) +    
              lag(gpty, n=1L),
            data = data_pre_covid)

summary(gwr_3)


gwr_4 <- lm(formula = gw ~
              lag(gw, n=1L) + lag(gw, n=2L) + lag(gw, n=3L) + lag(gw, n=4L) +
              lag(v_u, n=1L) + lag(v_u, n=2L) + lag(v_u, n=3L) + lag(v_u, n=4L) +
              lag(catch_up, n=1L) + lag(catch_up, n=2L) + lag(catch_up, n=3L) + lag(catch_up, n=4L) +
              lag(cf1, n=1L) + lag(cf1, n=2L) + lag(cf1, n=3L) + lag(cf1, n=4L) + 
              lag(gpty, n=1L),
            data = data_pre_covid)

summary(gwr_4)


# Price equation ----------------------------------------------------------


# ta wersja wchodzi do równania z symulacją

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

gpr_2 <- lm(
  formula = gp ~
    lag(gp, n=1L) + lag(gp, n=2L) +
    gw + lag(gw, n=1L) + lag(gw, n=2L) +
    grpe + lag(grpe, n=1L) + lag(grpe, n=2L) + 
    grpf + lag(grpf, n=1L) + lag(grpf, n=2L) + 
    shortage + lag(shortage, n=1L) + lag(shortage, n=2L) +
    lag(gpty, n=1L),
  data = data_pre_covid
)

summary(gpr_2)


gpr_3 <- lm(
  formula = gp ~
    lag(gp, n=1L) + lag(gp, n=2L) + lag(gp, n=3L) +
    gw + lag(gw, n=1L) + lag(gw, n=2L) + lag(gw, n=3L) +
    grpe + lag(grpe, n=1L) + lag(grpe, n=2L) + lag(grpe, n=3L) + 
    grpf + lag(grpf, n=1L) + lag(grpf, n=2L) + lag(grpf, n=3L) + 
    shortage + lag(shortage, n=1L) + lag(shortage, n=2L) +
    lag(shortage, n=3L) +
    lag(gpty, n=1L),
  data = data_pre_covid
)

summary(gpr_3)

gpr_4 <- lm(
  formula = gp ~
    lag(gp, n=1L) + lag(gp, n=2L) + lag(gp, n=3L) + lag(gp, n=4L) +
    gw + lag(gw, n=1L) + lag(gw, n=2L) + lag(gw, n=3L) + lag(gw, n=4L) +
    grpe + lag(grpe, n=1L) + lag(grpe, n=2L) + lag(grpe, n=3L) + lag(grpe, n=4L) +
    grpf + lag(grpf, n=1L) + lag(grpf, n=2L) + lag(grpf, n=3L) + lag(grpf, n=4L) +
    shortage + lag(shortage, n=1L) + lag(shortage, n=2L) +
    lag(shortage, n=3L) + lag(shortage, n=4L) +
    lag(gpty, n=1L),
  data = data_pre_covid
)

summary(gpr_4)


# Short run inflation expectations ----------------------------------------


# ta wersja wchodzi do równania z symulacją

cf1r_1 <- lm(
  formula = cf1 ~
    lag1_cf1 +
    cf10 + lag1_cf10 +
    gp + lag1_gp,
  data = data_pre_covid)

summary(cf1r_1)

cf1r <- cf1r_1


linearHypothesis(cf1r_1, "cf10 + lag1_cf10 + gp + lag1_gp= 1")
#' model z restrykcjami ma sens tylko dla obecnych relacji
#' 
linearHypothesis(cf1r_1, "cf10 + gp = 1")

linearHypothesis(cf1r_1, "cf10 + lag1_cf10 = 0")
linearHypothesis(cf1r_1, "gp + lag1_gp = 0")


summary(cf1r_1)

cf1r_2 <- lm(
  formula = cf1 ~
    lag(cf1, n=1L) + lag(cf1, n=2L) +
    cf10 + lag(cf10, n=1L) + lag(cf10, n=2L) +
    gp + lag(gp, n=1L) + lag(gp, n=2L),
  data = data_pre_covid
)

summary(cf1r_2)


cf1r_3 <- lm(
  formula = cf1 ~
    lag(cf1, n=1L) + lag(cf1, n=2L) + lag(cf1, n=3L) +
    cf10 + lag(cf10, n=1L) + lag(cf10, n=2L) + lag(cf10, n=3L) +
    gp + lag(gp, n=1L) + lag(gp, n=2L) + lag(gp, n=3L),
  data = data_pre_covid
)

summary(cf1r_3)


# Long run inflation exp --------------------------------------------------


cf10r_1 <- lm(
  formula = cf10 ~
    lag(cf10, n=1L) +
    gp + lag(gp, n=1L),
  data = data_pre_covid
)

summary(cf10r_1)

# ta wersja wchodzi do symulacji

cf10r_2 <- lm(
  formula = cf10 ~
    lag1_cf10 + lag2_cf10 + 
    gp + lag1_gp + lag2_gp,
  data = data_pre_covid
)

summary(cf10r_2)

cf10r <- cf10r_2

car::linearHypothesis(cf10r_2, "lag1_cf10 + lag2_cf10=0")
car::linearHypothesis(cf10r_2, "gp + lag1_gp + lag2_gp=0")


cf10r_3 <- lm(
  formula = cf10 ~
    lag1_cf10 + lag2_cf10 + lag3_cf10 + 
    gp + lag1_gp + lag2_gp + lag3_gp,
  data = data_pre_covid
)

cf10r_4 <- lm(
  formula = cf10 ~
    lag(cf10, n=1L) + lag(cf10, n=2L) + lag(cf10, n=3L) + lag(cf10, n=4L) +
    gp + lag(gp, n=1L) + lag(gp, n=2L) + lag(gp, n=3L) + lag(gp, n=4L),
  data = data_pre_covid
)

summary(cf10r_4)

