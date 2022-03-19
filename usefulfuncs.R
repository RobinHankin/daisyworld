## Supporting functions for daisyworld

params <- list(
    gamma = 0.3, # death rate
    parabolic=0.003265,
    T_opt = 22.5,
    S_cgs = 9.17e-4,specified in caption to figure 1 in ergs/cm^2s FFS
    sigma = 5.670374419e-8, # SI
    triple_point = 273 # actually 273.15
    Ag = 0.50, # albedo of ground
    Aw = 0.75, # albedo of white daisies
    Ab = 0.25, # albedo of black daisies
    q = NA, 
    qdash = 20,    # caption in figure 1
    p = 1.0  # proportion of fertile ground, fig 1
    
)

`bare_fertile` <- function(aw,ab){with(params,p-aw-ab)} #eq 2
`specific_growth_rate` <- function(temp,params){with(params, 1-parabolic*(T_opt-temp)^2)} #eq 3
`T_effective` <- function(A,L,params){with(params, (S*L*(1-A)/sigma)^(0.25) - triple_point)} #eq 4
`albedo` <- function(aw,ab,params){with(params, (1-aw-aw)*Ag + aw*Aw + ab*Ab)} #eq 5
`T_white` <- function(T_e, A){with(params, qdash*(A-Aw) + Te ) } #eq 7 (white)
`T_black` <- function(T_e, A){with(params, qdash*(A-Ab) + Te ) } #eq 7 (black)


