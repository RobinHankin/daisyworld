library("deSolve")

source("usefulfuncs.R")


initial_state <- c(W=0.1, B=0.1)   # initial value
times <- seq(0, 100, by = 10)      # times for output
out <- ode(y = initial_state, times = times, func = watson, parms = parameters)

plot(out)
