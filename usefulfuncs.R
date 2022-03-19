## Supporting functions for daisyworld

parameters <- list(
    L  = 1,  # solar luminosity
    gamma = 0.3, # death rate
    parabolic=0.003265,
    T_opt = 22.5,
    S     = 917, # SI!  In fig 1, W&L use ergs/cm^2s FFS
    sigma = 5.670374419e-8, # stefan's constant: SI
    triple_point = 273, # actually 273.15
    Ag = 0.50, # albedo of ground
    Aw = 0.75, # albedo of white daisies
    Ab = 0.25, # albedo of black daisies
    q = NA, 
    qdash = 20,    # caption in figure 1
    p = 1.0  # proportion of fertile ground, fig 1
    
)

`bare_fertile` <- function(W,B){with(parameters,p-W-B)} #eq 2
`growth_rate` <- function(temp,parameters){with(parameters, 1-parabolic*(T_opt-temp)^2)} #eq 3
`T_effective` <- function(A,L,parameters){with(parameters, (S*L*(1-A)/sigma)^(0.25) - triple_point)} #eq 4
`albedo` <- function(W,B,parameters){with(parameters, (1-W-B)*Ag + W*Aw + B*Ab)} #eq 5
`T_white` <- function(T_e, A){with(parameters, qdash*(A-Aw) + T_e ) } #eq 7 (white)
`T_black` <- function(T_e, A){with(parameters, qdash*(A-Ab) + T_e ) } #eq 7 (black)


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

