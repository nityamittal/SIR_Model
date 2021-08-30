library(deSolve)
library(reshape2)
library(ggplot2)

initial_state_values <- c(S = 999999,I = 1,R = 0)

parameters <- c(beta = 0.5, gamma = 0.25)

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
  labs(colour = "Compartment")                                           