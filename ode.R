library("deSolve")
source("usefulfuncs.R")


## modified from "Chaos in the atmosphere"
watson <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
        x <- bare_fertile(W,B)
        A <- albedo(W,B,parameters)
        T_e <- T_effective(A,L,parameters)

        T_W <- T_white(T_e,A)
        T_B <- T_black(T_e,A)
        
        beta_W <- growth_rate(T_W,parameters)
        beta_B <- growth_rate(T_B,parameters)
        
        dW <-  W*(x*beta_W-gamma)
        dB <-  B*(x*beta_B-gamma)
        list(c(dW,dB),T_e=T_e,bare=x)
    })
}

initial_state <- c(W=0.1, B=0.1)   # initial value
times <- seq(0, 100, by = 10)      # times for output
out <- ode(y = initial_state, times = times, func = watson, parms = parameters)

plot(out)
