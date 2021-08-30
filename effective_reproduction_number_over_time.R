library(deSolve)
library(reshape2)
library(ggplot2)


initial_state_values <- c(S = 1000000-1,I = 1,R = 0)

parameters <- c(beta = 0.4, gamma = 0.1)

times <- seq(from = 0, to = 100, by = 1) 

sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), { 
    
    N <- S+I+R
    
    lambda <- beta * I/N
    
    dS <- -lambda * S               
    dI <- lambda * S - gamma * I    
    
    dR <- gamma * I         

    return(list(c(dS, dI, dR))) 
  })
  
}


output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))

output_long <- melt(as.data.frame(output), id = "time")                 

output_long$proportion <- output_long$value/sum(initial_state_values)

ggplot(data = output_long,                                              
       aes(x = time, y = proportion, colour = variable, group = variable)) + 
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Proportion of the population") +                                 
  labs(colour = "Compartment",                                             
       title = "Proportion susceptible, infected and recovered over time") +                                                               
  theme(legend.position = "bottom")                                      

output$reff <- parameters["beta"]/parameters["gamma"] *                 
  output$S/(output$S+output$I+output$R)                    # multiply R0 by the proportion susceptible
# at each timestep/for each row
# In this calculation, the total population size (output$S+output$I+output$R) is calculated for each timestep
# so this approach would also be appropriate if the population size varies over time

# Plot Reff
ggplot(data = output,                                                    
       aes(x = time, y = reff)) +                                        
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Reff") +                                                         
  labs(title = "Effective reproduction number over time")                