rm(list = ls())

library(tidyverse)
library(readxl)
library(car)

# załadowanie danych ---------------------------------------------------------------

# ładuje dane z pliku szeregi_czasowe.xlsx, na innych komputerach trzeba to zmieniać
data_raw <- read_excel(path  = "szeregi_czasowe.xlsx",
           # stare: sheet = "szeregi_qq"
                       sheet = "szeregi_qq_new") %>%
  mutate(data = as.Date(data)) %>% 
  # wyczyszczam próbę z okresów, gdzie nie ma żadnej obserwacji.
  filter(!if_any(.cols = everything(),
                .fns = is.na))

# odfiltrowuję dane, na próbę sprzed pandemii



data_pre_covid <- data_raw %>% 
  filter(data < "2020-01-01") %>% 
  # dodaje zmienne z opóźnieniami 
  # w kolumnach od 2 do 13 znajdują się zmienne używane dalej w modelu
  # wprowadzam opóźnienia od 1 do 4
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

## Wersja z v i u --------------------------------------------

gwr_2_v_gp <- lm(formula = gw ~
              lag1_gw + lag2_gw +
              lag1_v + lag2_v +
              lag1_catch_up + lag2_catch_up +
              lag1_cf1 + lag2_cf1 +
              lag1_gpty,
            data = data_pre_covid)

summary(gwr_2_v_gp)

# H0: prawdziwa jest przedstawiana w równaniu restrykcja
# H1: nieprawdziwa jest przedstawiana w równaniu restrykcja
car::linearHypothesis(gwr_2_v_gp,"lag1_gw+lag2_gw=0")
car::linearHypothesis(gwr_2_v_gp,"lag1_v+lag2_v=0")
car::linearHypothesis(gwr_2_v_gp,"lag1_catch_up + lag2_catch_up=0")
car::linearHypothesis(gwr_2_v_gp,"lag1_cf1 + lag2_cf1=0")

# zapisanie wersji do symulacji
gwr <- gwr_2_v_gp


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


car::linearHypothesis(gpr,"gw+lag1_gw=0")
car::linearHypothesis(gpr,"grpe + lag1_grpe=0")
car::linearHypothesis(gpr,"grpf + lag1_grpf=0")
car::linearHypothesis(gpr,"shortage + lag1_shortage=0")

# Short run inflation expectations ----------------------------------------


# ta wersja wchodzi do równania z symulacją

cf1r_1 <- lm(
  formula = cf1 ~
    lag1_cf1 +
    gp + lag1_gp +
    I(gp^2),
  data = data_pre_covid)

summary(cf1r_1)

cf1r <- cf1r_1


# linearHypothesis(cf1r_1, "cf10 + lag1_cf10 + gp + lag1_gp= 1")
#' model z restrykcjami ma sens tylko dla obecnych relacji
#' 
# linearHypothesis(cf1r_1, "cf10 + gp = 1")

# linearHypothesis(cf1r_1, "cf10 + lag1_cf10 = 0")
# linearHypothesis(cf1r_1, "gp + lag1_gp +I(gp^2) = 0")



