# Imporatant informations

# Source Data files

In the case of .xlsx files the source and description of used data is given within the file, in metadata sheet


# Files: 

## 1. load_data.R

This file is preparing data to be used in further 

After using this script one must copy data from csv to excel file *time_series.xlsx* in *data_input* folder. The file is not provided due to confidentiality of expectations series.

## 1. backcast_expectations.R

This file performs a backcast of long-term expectations based on short-term expectations.

## estimate.R

This file contains commands that estimate individual equations. Data is retrieved from a file, and then a relevant sample is selected. *The equations later used in the simulation are saved in a comment after the summarizing command.* Each equation is estimated in four forms, with different numbers of lags, to determine the best option.

## simul_function.R

This file contains a function that performs a simulation based on input data and structural parameter estimates from the *estimate.R* file.

## IRFs.R

This file generates impulse response functions (IRFs) using the function from *simul_function.R* and based on variable estimates from the *estimate.R* file. The output is a chart with IRFs for energy price shocks, food prices, and shortages. All shocks are introduced at the level of 1 standard deviation within the sample. The output also includes a chart of the response functions.

## simulation

This file generates a simulation using the function from simul_function and based on variable estimates from the estymacja file. The output is a chart with IRFs.

Assumptions are made for exogenous variable values as in the data, and the path of endogenous variables is determined. The output is a chart comparing actual and simulated data.

## org_equations.R

Replication of estimations from the original study. Used to check if sums are consistent.

## Variance_decomposition.R

This file performs variance decomposition, based on *simul_function.R*. Variance decomposition is carried out by excluding individual variables and comparing the results from the simulation with and without each variable.
