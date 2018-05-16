
cost_function <- function( par ){
  cost = system( paste0("echo ", sprintf( "%f", par ), " | ./runcalib | tail -n 1"), intern = TRUE)
  return(as.numeric(cost))
}

## initial parameter guess
par = c(1.0)

# limits to the parameter space
lower = c(0.001)
upper = c(100.00)

# # optimize the model parameters
# library(GenSA)
# # system.time(
#             optim_par = GenSA(
#                             par = par,
#                             fn = cost_function,
#                             lower = lower,
#                             upper = upper,
#                             control=list( temperature=4000,
#                                           max.call=100 
#                                           )
#                             )
#             # )

  #  user  system elapsed 
  # 7.326   2.573   9.506 



library( BayesianTools )

setup    <- createBayesianSetup( cost_function, lower = lower, upper = upper )
settings <- list( iterations = 1000,  message = FALSE )
out      <- runMCMC( bayesianSetup = setup, sampler = "Metropolis", settings = settings )

 #   user  system elapsed 
 # 10.688   3.806  14.099 