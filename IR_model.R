library(deSolve)
library(reshape2)
library(ggplot2)

initial_number_infected <- 1000000
initial_number_recovered <- 1000

recovery_rate <- 0.1

follow_up_duration <- 100

initial_state_value <- c(I=initial_number_infected, R=initial_number_recovered)

parameters <- c(gamma = recovery_rate)

times <- seq(from=0, to=follow_up_duration, by=1)

cohort_model<- function(time, state, parameters){
  with(as.list(c(state,parameters)),{
    dI<- -I*gamma
    dR<- I*gamma
    return(list(c(dI,dR)))
  })
}

output <- as.data.frame(ode(y= initial_state_value, times=times, func=cohort_model, parms= parameters))

output_long <- melt(as.data.frame(output), id="time")


ggplot(data = output_long,         
       aes(x = time, y = value, colour = variable, group = variable)) +  


  geom_line() +                                                          

  xlab("Time (in days)")+                    
  ylab("Number of people") +              
  labs(title = paste("Number of people Infected(I) and Recovered(R) wrt to time when gamma =",
                     parameters["gamma"],"days^-1"))

parameters <- c(gamma = 0.5)
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = cohort_model,
                            parms = parameters))
# Plotting the output
output_long <- melt(as.data.frame(output), id = "time")   
# turn output dataset into long format

ggplot(data = output_long,       
       aes(x = time, 
           y = value, 
           colour = variable, 
           group = variable)) +  
  geom_line() +                                 
  xlab("Time (days)")+                          
  ylab("Number of people") +                    
  labs(title = paste("Number infected and recovered over time when gamma =",
                     parameters["gamma"],"days^-1")) + # add title
  scale_color_brewer(palette = "Set1")