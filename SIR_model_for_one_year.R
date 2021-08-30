#LOAD THE PACKAGES:
library(deSolve)
library(reshape2)
library(ggplot2)

# MODEL INPUTS:

# Vector storing the initial number of people in each compartment (at timestep 0)
initial_state_values <- c(S = 1000000-1,   # the whole population we are modelling is susceptible to infection
                          I = 1,           # the epidemic starts with a single infected person
                          R = 0)           # there is no prior immunity in the population

# Vector storing the parameters describing the transition rates in units of years^-1
parameters <- c(beta = 0.4*365,      # the infection rate, which acts on susceptibles
                gamma = 0.2*365,     # the rate of recovery, which acts on those infected
                mu = 1/70,           # the background mortality rate, which acts on every compartment
                b = 1/70)            # the birth rate

# TIMESTEPS:

# Vector storing the sequence of timesteps to solve the model at
times <- seq(from = 0, to = 400, by = 2/365)   # from 0 to 400 YEARS in intervals of every 2 days

# SIR MODEL FUNCTION: 

# The model function takes as input arguments (in the following order): time, state and parameters
sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {  # tell R to look for variable names within the state and parameters objects    
    
    # Calculating the total population size N (the sum of the number of people in each compartment)
    N <- S+I+R
    
    # Defining lambda as a function of beta and I:
    lambda <- beta * I/N
    
    # The differential equations with addition of births and deaths
    dS <- -lambda * S - mu * S + b * N            
    dI <- lambda * S - gamma * I  - mu * I           
    dR <- gamma * I - mu * R               
    
    # Return the number of people in the S, I and R compartments at each timestep 
    # (in the same order as the input state variables)
    return(list(c(dS, dI, dR))) 
  })
  
}

# MODEL OUTPUT (solving the differential equations):

# Solving the differential equations using the ode integration algorithm
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))

# PLOT
output_long <- melt(as.data.frame(output), id = "time")     


# Plot of the epidemic curve for the first year
ggplot(data = output_long,aes(x = time,y = value,colour = variable, group = variable)) +                                           # only plotting the I column from our output dataframe
  geom_line() +                                                          # represent data as lines
  xlab("Time (years)")+                                                  # add label for x axis
  ylab("Number of infected people") +                                    # add label for y axis
  labs(title = "Epidemic curve in the first year after introduction of an infected case") +  # add plot title   
  xlim(c(0,1))                                                           # limit the time axis to 1 year

# ggplot gives us a warning that we removed all the rows beyond the first year in the plot - that's what we wanted
# so it's fine