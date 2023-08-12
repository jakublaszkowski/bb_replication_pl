# plik zawiera funckję, która dokonuje symulacji, użytej zarówno do scenariusza 
# kontrfaktycznego jak i do generowania IRF. 
# Aby uruchomić plik niezbędne jest wcześniejsze wczytanie zmiennych z estymacja.R

simul_endo_values <- function(data_box, max_lag, simul_length){
  
  # nadpisuję zmienną do zwrotu
  data_box_to_return <- data_box
  
  # maksymalne opóźnienie musi być równe większe 4, ponieważ wymaga tego 
  # catch up
  if(max_lag<4){
    stop("max_lag must be at least 4 or larger")
  }
  # simul_length 
  # - długość symulacji 
  
  # part that compute simulation of endogenous values
  for(ind in (max_lag+1):(simul_length+max_lag+1)){
    
    # gw equation
    data_box_to_return$gw[ind] <- 
      # wages 
      gwr$coefficients[['lag1_gw']] * data_box_to_return$gw[ind-1] +
      gwr$coefficients[['lag2_gw']] * data_box_to_return$gw[ind-2] +
      # V/U
      gwr$coefficients[['lag1_v_u']] * data_box_to_return$v_u[[ind-1]] +
      gwr$coefficients[['lag2_v_u']] * data_box_to_return$v_u[[ind-2]] +
      # catch_up
      gwr$coefficients[['lag1_catch_up']] * data_box_to_return$catch_up[[ind-1]] +
      gwr$coefficients[['lag2_catch_up']] * data_box_to_return$catch_up[[ind-2]] +
      # cf_1
      gwr$coefficients[['lag1_cf1']] * data_box_to_return$cf1[[ind-1]] +
      gwr$coefficients[['lag2_cf1']] * data_box_to_return$cf1[[ind-2]] +
      # gpty
      gwr$coefficients[['lag1_gpty']] * data_box_to_return$gpty[[ind-1]] 
    
    # gp equation
     
    data_box_to_return$gp[ind] <-
      # prices
      gpr$coefficients[['lag1_gp']] * data_box_to_return$gp[ind-1] +
      # wages
      gpr$coefficients[['gw']] * data_box_to_return$gw[ind] +
      gpr$coefficients[['lag1_gw']] * data_box_to_return$gw[ind-1] +
      # grpe
      gpr$coefficients[['grpe']] * data_box_to_return$grpe[ind] +
      gpr$coefficients[['lag1_grpe']] * data_box_to_return$grpe[ind-1] +
      # grpf
      gpr$coefficients[['grpf']] * data_box_to_return$grpf[ind] +
      gpr$coefficients[['lag1_grpf']] * data_box_to_return$grpf[ind-1] +
      # shortage
      gpr$coefficients[['shortage']] * data_box_to_return$shortage[ind] +
      gpr$coefficients[['lag1_shortage']] * data_box_to_return$shortage[ind-1] +
      # gpty
      gpr$coefficients[['lag1_gpty']] * data_box_to_return$gpty[[ind-1]] 
    
    # cf1 equation
    
    data_box_to_return$cf1[ind] <-
      # cf
      cf1r$coefficients[['lag1_cf1']] * data_box_to_return$cf1[[ind-1]] +
      # cf10
      cf1r$coefficients[['cf10']] * data_box_to_return$cf10[[ind-1]] +
      cf1r$coefficients[['lag1_cf10']] * data_box_to_return$cf10[[ind-1]] + 
      # gp 
      cf1r$coefficients[['gp']] * data_box_to_return$gp[ind] +
      cf1r$coefficients[['lag1_gp']] * data_box_to_return$gp[ind-1] 
    
    # cf10 equation
    
    data_box_to_return$cf10[ind] <-
      # cf 10
      cf10r$coefficients[['lag1_cf10']] * data_box_to_return$cf10[[ind-1]] + 
      cf10r$coefficients[['lag2_cf10']] * data_box_to_return$cf10[[ind-2]] + 
      # gp 
      cf10r$coefficients[['gp']] * data_box_to_return$gp[ind] +
      cf10r$coefficients[['lag1_gp']] * data_box_to_return$gp[ind-1] +
      cf10r$coefficients[['lag2_gp']] * data_box_to_return$gp[ind-2] 
     
    # catch_up equation
    data_box_to_return$catch_up[ind] <- sum(data_box_to_return$gp[(ind-3):ind])/4 - data_box_to_return$cf1[ind-4]
  }
  
  # zwraca dane z symulacji
  return(data_box_to_return)
}
