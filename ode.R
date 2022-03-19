source("usefulfuncs.R")


initial_state <- c(W=0.1, B=0.1)   # initial value
times <- seq(0, 10, by = 1)      # times for output
out <- ode(y = initial_state, times = times, func = watson, parms = parameters)

plot(out)
