library(deSolve)
library(reshape2)
library(ggplot2)


initial_state_values <- c(S = 1000000-1, I = 1, R = 0)

parameters <- c(beta = 0.4*365,  gamma = 0.2*365, mu = 1/70,  b = 1/70)

times <- seq(from = 0, to = 400, by = 2/365)

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


output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))


ggplot(data = output,                                               
       aes(x = time, y = I)) +                                           
  geom_line() +                                                          
  xlab("Time (years)")+                                                  
  ylab("Number of infected people") +                                   
  labs(title = "Epidemic curve in the first year after introduction of an infected case") +    
  xlim(c(0,1))                                                           


output_long <- melt(as.data.frame(output), id = "time")                  # turn output dataset into long format

# Plot of the epidemic curve over time
ggplot(data = output,                                               
       aes(x = time, y = I)) +                                           # only plotting the I column from our output
  geom_line() +                                                          # represent data as lines
  xlab("Time (years)")+                                                  # add label for x axis
  ylab("Number of infected people") +                                    # add label for y axis
  labs(title = "Epidemic curve over 4 generations")                      # add plot title   

output_long <- melt(as.data.frame(output), id = "time")                  # turn output dataset into long format

# Calculating the proportion in each compartment as a column in the long-format output
output_long$proportion <- output_long$value/sum(initial_state_values)

# Plot the number of people in the S, I and R compartments over time
ggplot(data = output_long,                                               # specify object containing data to plot
       aes(x = time, y = proportion, colour = variable, group = variable)) +  # assign columns to axes and groups
  geom_line() +                                                          # represent data as lines
  xlab("Time (years)")+                                                  # add label for x axis
  ylab("Prevalence (proportion)") +                                      # add label for y axis
  labs(colour = "Compartment",                                           # add legend title  
       title = "Prevalence of susceptible, infected and recovered people over time")