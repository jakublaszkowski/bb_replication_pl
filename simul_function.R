# The file contains a function that performs a simulation, used both for the 
# counterfactual scenario and for generating IRF.
# To run the file, it is necessary to first load the variables from estymacja.R

simul_endo_values <- function(data_box, max_lag, simul_length){
  
  # overwrite the variable to return
  data_box_to_return <- data_box
  
  # maximum lag must be at least 4, as required by 
  # catch up
  if(max_lag<4){
    stop("max_lag must be at least 4 or larger")
  }
  # simul_length 
  # - length of the simulation
  
  # part that computes the simulation of endogenous values
  for(ind in (max_lag+1):(simul_length+max_lag+1)){
    
    # gw equation
    data_box_to_return$gw[ind] <- 
      # wages 
      gwr$coefficients[['lag1_gw']] * data_box_to_return$gw[ind-1] +
      gwr$coefficients[['lag2_gw']] * data_box_to_return$gw[ind-2] +
      # V
      gwr$coefficients[['lag1_v']] * data_box_to_return$v[[ind-1]] +
      gwr$coefficients[['lag2_v']] * data_box_to_return$v[[ind-2]] +
      # catch_up
      gwr$coefficients[['lag1_catch_up']] * data_box_to_return$catch_up[[ind-1]] +
      gwr$coefficients[['lag2_catch_up']] * data_box_to_return$catch_up[[ind-2]] +
      # add binary
      # gwr$coefficients[['lag1_catch_up_binary']] * data_box_to_return$catch_up_binary[[ind-1]] +
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
      # gp
      cf1r$coefficients[['gp']] * data_box_to_return$gp[ind] +
      cf1r$coefficients[['lag1_gp']] * data_box_to_return$gp[ind-1] +
      cf1r$coefficients[['I(gp^2)']] * (data_box_to_return$gp[ind])^2
    
  }
  
  # returns simulation data
  return(data_box_to_return)
}
