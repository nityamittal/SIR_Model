library(deSolve)
library(reshape2)
library(ggplot2)


initial_state_values <- c(S = 1000000-1,  
                          I = 1,         
                          R = 0)          

parameters <- c(beta = 0.4,gamma = 0.2,mu = 1/28, b = 1/28)  

times <- seq(from = 0, to = 365, by = 1)

sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {  
    
    N <- S+I+R
    
    lambda <- beta * I/N
    
    dS <- -lambda * S - mu * S + b * N            
    dI <- lambda * S - gamma * I  - mu * I           
    dR <- gamma * I - mu * R               
    
    return(list(c(dS, dI, dR))) 
  })
  
}

output_pigs <- as.data.frame(ode(y = initial_state_values, 
                                 times = times, 
                                 func = sir_model,
                                 parms = parameters))

output_pigs_long <- melt(as.data.frame(output_pigs), id = "time")    

output_pigs_long$prevalence <- output_pigs_long$value/sum(initial_state_values)

ggplot(data = output_pigs_long,                                               
       aes(x = time, y = prevalence, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Prevalence (proportion)") +                                      
  labs(colour = "Compartment",                                           
       title = "Prevalence of infection, susceptibility and recovery over time") +  
  theme(legend.position = "bottom")