
# cost_function <- function( par, minimize=TRUE ){
#   cost = system( paste0("echo ", settings$simsuite, " ", sprintf( "%f", par ), " | ./run", settings$model, " | tail -n 1"), intern = TRUE)
#   return(as.numeric(-cost))
# }

cost_function <- function( par ){
  cost = system( paste0("echo fcover ", sprintf( "%f", par ), " | ./runcmodel_simsuite | tail -n 1"), intern = TRUE)
  return((as.numeric(cost)))
}

negcost_function <- function( par ){
  ## this executes the model and reads the cost that is returned through the standard output (very last line)
  cost = system( paste0("echo fcover ", sprintf( "%f", par ), " | ./runcmodel_simsuite | tail -n 1"), intern = TRUE)
  
  ## return the negative of the cost!
  return(-(as.numeric(cost)))
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

## Prepare simulation and site parameter files, and link directories (forcing files are prepared independently)
setup_sofun( simsuite="fcover" )

## Define which types of simulations (simulation set and compilation type) to be done
settings <- list( model="cmodel_simsuite", do_compile=FALSE, simsuite="fcover" )

## example: get cost for a single parameter value (4.0)
here <- getwd()
setwd("/Users/benjaminstocker/sofun/trunk")
cost <- cost_function(4.0)
setwd(here)
print(cost)

# ## Basic run (output is the cost now)
# output <- run_sofun( par, return_data="predicted", settings=settings )

# limits to the parameter space
lower = c(1)
upper = c(20)
maxit = 300
here <- "/Users/benjaminstocker/fcover"

##----------------------------------------------------------------
## Simple visualisation of the cost
##----------------------------------------------------------------
setwd("/Users/benjaminstocker/sofun/trunk")
par_list <- as.list(seq(lower,upper,by=0.3))
cost_list <- sapply( par_list, cost_function )
plot( unlist(par_list), cost_list )
print(par_list[which.min((cost_list))])
setwd(here)

##----------------------------------------------------------------
## calibrate the model parameters using GenSA (simulated annealing)
##----------------------------------------------------------------
library(GenSA)
# setwd("/alphadata01/bstocker/sofun/trunk/")
setwd("/Users/benjaminstocker/sofun/trunk")
ptm <- proc.time()
optim_par_gensa = GenSA(
    par = par,
    fn = cost_function,
    lower = lower,
    upper = upper,
    control=list( temperature=4000, max.call=maxit )
  )
proc.time() - ptm
setwd(here)
print(optim_par_gensa$par)

# user  system elapsed 
# 8.521   1.158   9.425 


##----------------------------------------------------------------
## calibrate model parameters using rgenoud
##----------------------------------------------------------------
library( rgenoud )
setwd("/Users/benjaminstocker/sofun/trunk")
ptm <- proc.time()
optim_par_rgenoud = rgenoud::genoud(fn = cost_function,
                            nvars = length(par),
                            max.generations = 3,
                            Domains = cbind(lower,upper),
                            boundary.enforcement = 2,
                            data.type.int = FALSE
                            )
proc.time() - ptm
setwd(here)
print(optim_par_rgenoud$par)

##----------------------------------------------------------------
## calibrate model parameters using BayesianTools and Metropolis MCMC
##----------------------------------------------------------------
library( BayesianTools )

# setwd("/alphadata01/bstocker/sofun/trunk/")
setwd("/Users/benjaminstocker/sofun/trunk")
ptm <- proc.time()
setup    <- createBayesianSetup( negcost_function, lower = lower, upper = upper )
settings <- list( iterations = maxit,  message = FALSE )
optim_par_bayesiantools <- runMCMC( bayesianSetup = setup, sampler = "DEzs", settings = settings )
proc.time() - ptm
setwd(here)

 #   user  system elapsed 
 # 10.688   3.806  14.099 