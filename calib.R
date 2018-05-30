
cost_function <- function( par ){
  cost = system( paste0("echo ", settings$simsuite, " ", sprintf( "%f", par ), " | ./run", settings$model, " | tail -n 1"), intern = TRUE)
  return(as.numeric(cost))
}

## This corresponds to Case 3 in Hartig et al. Calibrating Dynamic Vegetation Models (see p. 85)
run_sofun <- function( par, settings, return_data="cost" ){
  
  if (return_data=="cost"){
    
    ## SOFUN returns the cost from a pre-defined function implemented in the 'runcalib' setup
    systemCall <- paste0("echo ", sprintf( "%f", par ), " | ./runcalib | tail -n 1")
    cost = system( systemCall, intern = TRUE )
    return(as.numeric(cost))
    
  } else if (return_data=="predicted"){
    
    ## Compile and run model corresponding to this setup
    if (settings$do_compile) system( paste0( "make ", settings$model ) )
    systemCall <- paste0("echo ", settings$simsuite, " ", sprintf( "%f", par ), " | ./run", settings$model )
    system( systemCall )
    
    ## SOFUN writes predicted values to file, to be read into R here
    predicted <- read_sofun()
    return( predicted )
  }
  
}


## initial parameter guess
par = c(1.0)

# limits to the parameter space
lower = c(0.001)
upper = c(100.00)

## Prepare simulation and site parameter files, and link directories (forcing files are prepared independently)
setup_sofun( simsuite="fcover" )

## Define which types of simulations (simulation set and compilation type) to be done
settings <- list( model="cmodel_simsuite", do_compile=FALSE, simsuite="fcover" )

## Basic run (output is the cost now)
output <- run_sofun( par, return_data="predicted", settings=settings )

# optimize the model parameters
library(GenSA)
here <- getwd()
setwd("/alphadata01/bstocker/sofun/trunk/")
optim_par = GenSA(
                  par = par,
                  fn = cost_function,
                  lower = lower,
                  upper = upper,
                  control=list( temperature=4000, max.call=100 )
                  )
setwd(here)


  #  user  system elapsed 
  # 7.326   2.573   9.506 



library( BayesianTools )

setup    <- createBayesianSetup( cost_function, lower = lower, upper = upper )
settings <- list( iterations = 1000,  message = FALSE )
out      <- runMCMC( bayesianSetup = setup, sampler = "Metropolis", settings = settings )

 #   user  system elapsed 
 # 10.688   3.806  14.099 