copy_input = function(input) {
  pars = c()
  for (name in names(input))
    pars[[name]] = input[[name]]
  pars
}

par_defaults = list(
  tau_tilde=0.3, 
  sigma_b=0.5, 
  sigma_w=1,
  sigma_n=5, 
  sigma_tau_tilde=0,
  rho_w=0,
  rho_b=0,
  sigma_l=2, 
  A=25,
  N=10000, 
  Y=5,
  P_CO2=40, 
  z_deduct=-0.43, 
  c_T=25, 
  c_C=25,
  c_1s=400, 
  c_2s=15, 
  c_l=20, 
  r_b=0.05,
  esm=TRUE
)

design_defaults = list(
  p_1C=0.05, 
  d_2C=1, 
  n_3C=3, 
  p_1T=0.05, 
  d_2T=1, 
  n_3T=3
)

get_design_defaults = function(N) {
  list(
    p_1C=max(0.05, 3/N), 
    d_2C=1, 
    n_3C=3, 
    p_1T=max(0.05, 3/N), 
    d_2T=1, 
    n_3T=3
  )
}

get_design_names = function(design) {
  if(design == "cluster") {
    c("p_1C", "d_2C", "n_3C", "p_1T", "d_2T", "n_3T")
  } else if (design == "stratified" | design == "simple") {
    c("p_1C", "d_2C", "n_3C", "d_2T", "n_3T")
  }
}

par_names = names(par_defaults)
design_names = names(design_defaults)

cost_benefit <- function(N, # total fields
                         N_C, # number control fields
                         N_T, # number treatment fields
                         A, 
                         tau_tildeY, tau_sd, tau_hat_se, mu_C_hat_se, mu_T_hat_se,
                         n_fields, n_locations, n_labs, 
                         c_1s, c_2s, c_l, c_T,  c_C, Y, r_b, P_CO2, z_deduct, esm) {
  R_credit = (16*2+12)/12*A*N_T*tau_tildeY*P_CO2
  R_credit_sd = (16*2+12)/12*A*N_T*P_CO2*sqrt(tau_sd**2 + tau_hat_se**2)
  C_deduct = (16*2+12)/12*A*N_T*(-z_deduct*tau_hat_se)*P_CO2
  
  C_sample1 = n_fields*c_1s + n_locations*c_2s
  C_lab1 = n_labs*c_l
  
  # Implementation cost
  C_implement1 = N_T*c_T*A + N_C*c_C*A
  C_implement = C_implement1*Y
  
  C_measure1 = C_sample1 + C_lab1
  C_measure2 = C_sample1 + (1+esm)*C_lab1
  C_measure = C_measure1 + C_measure2
  
  Cost = C_measure + C_implement
  Profit = R_credit - C_deduct - Cost
  Profit_sd = R_credit_sd
  
  ROI = Profit/Cost
  ROI_sd = Profit_sd/Cost
  
  R_b = C_measure1*((1+r_b)^Y) + C_implement1*(1+r_b)*(1-(1+r_b)^Y)/(1-(1+r_b)) +
    (C_measure2)*0
  C_b = C_implement + C_measure1 + C_measure2*0
  ROI_b = (R_b - C_b)/C_b
  Sharpe = (ROI - ROI_b)/ROI_sd
  haY = Y*A*N
  
 list(
   R_credit=R_credit,
   R_credit_sd=R_credit_sd,
   C_deduct=C_deduct,
   C_measure=C_measure,
   C_implement=C_implement,
   Cost=Cost,
   Profit=Profit,
   Profit_sd = Profit_sd,
   ROI=ROI,
   ROI_sd=ROI_sd,
   ROI_b=ROI_b,
   Sharpe=Sharpe,
   haY=haY,
   N_C=N_C,
   N_T=N_T,
   tau_tildeY=tau_tildeY,
   n_fields=n_fields,
   n_labs=n_labs,
   n_locations=n_locations,
   tau_hat_se=tau_hat_se,
   mu_C_hat_se=mu_C_hat_se,
   mu_T_hat_se=mu_T_hat_se,
   tau_sd=tau_sd)
}

get_project_stats = function(tau_tilde, N, Y, sigma_b, sigma_w, sigma_tau_tilde, rho_w, rho_b) {
  tau_tildeY = tau_tilde*Y
  sigma_wY = sqrt((1 + rho_w*(Y-1))*Y)*sigma_w
  sigma_bY = sqrt((1 + rho_b*(Y-1))*Y)*sigma_b
  
  tau_sd = sqrt(Y^2*sigma_tau_tilde**2 + 2*sigma_bY**2/N)

  list(tau_tildeY=tau_tildeY, sigma_wY=sigma_wY, sigma_bY=sigma_bY, tau_sd=tau_sd)
}

analytic_soc_cluster = function(
    tau_tilde, sigma_b, sigma_w, sigma_tau_tilde, rho_w, rho_b, 
    sigma_n, sigma_l, A, # SOC
    N, Y, # project size
    p_1C, d_2C, n_3C, p_1T, d_2T, n_3T, # design
    P_CO2, z_deduct, c_T, c_C, c_1s, c_2s, c_l, r_b, # economics
    esm,
    ...) {
  
  ps = get_project_stats(tau_tilde, N, Y, sigma_b, sigma_w, sigma_tau_tilde, rho_w, rho_b)

  n_1C = p_1C*N             # TODO: check greater than 2 less than N-2
  En_1C = n_1C #N*(1-(1-1/N)^n_1C) # expected number of (sampled) control fields
  
  N_T = N-En_1C # expected number of treatment fields
  n_1T = p_1T*N
  En_1T = n_1T #N_T*(1-(1-1/N_T)^n_1T) # expected number of sampled treatment fields
  
  mu_C_hat_se_sq = ps$sigma_bY**2/n_1C + 2*sigma_l**2/(n_1C*d_2C*A/n_3C) + (ps$sigma_wY**2 + 2*sigma_n**2)/(n_1C*d_2C*A)
  mu_T_hat_se_sq = ps$sigma_bY**2/n_1T + 2*sigma_l**2/(n_1T*d_2T*A/n_3T) + (ps$sigma_wY**2 + 2*sigma_n**2)/(n_1T*d_2T*A)
  tau_hat_se = sqrt(mu_C_hat_se_sq + mu_T_hat_se_sq)

  # Sampling cost
  n_fields = En_1C + En_1T
  n_locations = A*(n_1C*d_2C + n_1T*d_2T)
  n_labs = A*(n_1C*d_2C/n_3C + n_1T*d_2T/n_3T)
  
  cost_benefit(N, N_C=En_1C, N_T, A, 
               ps$tau_tildeY, 
               ps$tau_sd,
               tau_hat_se, sqrt(mu_C_hat_se_sq), sqrt(mu_T_hat_se_sq),
               n_fields, n_locations, n_labs, 
               c_1s, c_2s, c_l, c_T,  c_C, Y, r_b, P_CO2, z_deduct,
               esm)
}

analytic_soc_simple = function(
    tau_tilde, sigma_b, sigma_w, sigma_tau_tilde, rho_w, rho_b, sigma_n, sigma_l, A, # SOC
    N, Y, # project size
    p_1C, d_2C, n_3C, d_2T, n_3T, # design
    P_CO2, z_deduct, c_T, c_C, c_1s, c_2s, c_l, r_b, # economics
    esm,
    ...) {
  ps = get_project_stats(tau_tilde, N, Y, sigma_b, sigma_w, sigma_tau_tilde, rho_w, rho_b)
  
  n_1C = p_1C*N             # TODO: check greater than 2 less than N-2
  En_1C = n_1C #n_1C*(1-(1-1/n_1C)^(n_1C*A*d_2C))
  
  N_T = N-n_1C
  n_1T = N_T
  En_1T = n_1T*(1-(1-1/n_1T)^(n_1T*A*d_2T)) # expected number of sampled treatment fields
  
  mu_C_hat_se_sq = ps$sigma_bY**2/n_1C + 2*sigma_l**2/(n_1C*d_2C*A/n_3C) + (ps$sigma_wY**2 + 2*sigma_n**2)/(n_1C*d_2C*A)
  mu_T_hat_se_sq = ps$sigma_bY**2/n_1T + (ps$sigma_bY**2 + ps$sigma_wY**2 + 2*sigma_n**2)/(n_1T*d_2T*A) + 2*sigma_l**2/(n_1T*d_2T*A/n_3T)
  tau_hat_se = sqrt(mu_C_hat_se_sq + mu_T_hat_se_sq)
  
  # Sampling cost
  n_fields = En_1C + En_1T
  n_locations = A*(n_1C*d_2C + n_1T*d_2T)
  n_labs = A*(n_1C*d_2C/n_3C + n_1T*d_2T/n_3T)
  
  cost_benefit(N, En_1C, N_T, A, 
               ps$tau_tildeY, ps$tau_sd, tau_hat_se, sqrt(mu_C_hat_se_sq), sqrt(mu_T_hat_se_sq),
               n_fields, n_locations, n_labs, 
               c_1s, c_2s, c_l, c_T,  c_C, Y, r_b, P_CO2, z_deduct,
               esm)
}

analytic_soc_stratified = function(
    tau_tilde, sigma_b, sigma_w, sigma_tau_tilde, rho_w, rho_b, sigma_n, sigma_l, A, # SOC
    N, Y, # project size
    p_1C, d_2C, n_3C, d_2T, n_3T, # design
    P_CO2, z_deduct, c_T, c_C, c_1s, c_2s, c_l, r_b, # economics
    esm,
    ...) {
  ps = get_project_stats(tau_tilde, N, Y, sigma_b, sigma_w, sigma_tau_tilde, rho_w, rho_b)
  
  n_1C = p_1C*N             # TODO: check greater than 2 less than N-2
  En_1C = n_1C#n_1C*(1-(1-1/n_1C)^(n_1C*A*d_2C))
  
  N_T = N-n_1C
  n_1T = N_T
  En_1T = N_T # all treatment fields are sampled
  
  mu_C_hat_se_sq = ps$sigma_bY**2/n_1C + (ps$sigma_wY**2 + 2*sigma_n**2)/(n_1C*d_2C*A) + 2*sigma_l**2/(n_1C*d_2C*A/n_3C)
  mu_T_hat_se_sq = ps$sigma_bY**2/n_1T + (ps$sigma_wY**2 + 2*sigma_n**2)/(n_1T*d_2T*A) + 2*sigma_l**2/(n_1T*d_2T*A/n_3T)
  tau_hat_se = sqrt(mu_C_hat_se_sq + mu_T_hat_se_sq)
  
  # Sampling cost
  n_fields = En_1C + En_1T
  n_locations = A*(n_1C*d_2C + n_1T*d_2T)
  n_labs = A*(n_1C*d_2C/n_3C + n_1T*d_2T/n_3T)
  
  cost_benefit(N, En_1C, N_T, A, 
               ps$tau_tildeY, ps$tau_sd, tau_hat_se, sqrt(mu_C_hat_se_sq), sqrt(mu_T_hat_se_sq),
               n_fields, n_locations, n_labs, 
               c_1s, c_2s, c_l, c_T,  c_C, Y, r_b, P_CO2, z_deduct,
               esm)
}

cb_outcomes = list(
  R_credit="Carbon credit earnings (Expected)",
  R_credit_sd="Carbon credit earnings (SD)",
  C_deduct="Uncertainty deduction",
  C_measure="Measurement costs",
  C_implement="Farmer payments",
  Cost="Total cost",
  Profit="Profit (Expected)",
  Profit_sd="Profit (SD)"
)

summary_outcomes = list(
  ROI="Return on Investment (Expected)",
  ROI_sd="Return on investment (SD)",
  ROI_b="Risk-free return on investment",
  Sharpe="Sharpe ratio"
)

design_outcomes = list(
  n_fields="Fields visited",
  n_locations="Locations sampled",
  n_labs="Lab analyses (composites)",
  N_T="Treatment fields",
  N_C="Control fields"
)

est_outcomes = list(
  tau_tildeY="Treatment effect Mean",
  tau_hat_se="Treatment effect SE",
  mu_C_hat_se="Control change SE",
  mu_T_hat_se="Treatment change SE",
  tau_sd="Treatment effect SD",
  power="Power",
  s_error="Sign error rate",
  m_error="Exaggeration ratio"
)

convertCB = function(outcomes, unit) {
  if(unit == "Dollars ($)") {
    outcomes = outcomes
  } else if (unit == "Dollars per hectare-year ($ ha<sup>-1</sup> y<sup>-1</sup>)") {
    outcomes = outcomes %>%
      mutate(across(names(cb_outcomes), ~ .x / outcomes$haY))
  } else if (unit == "Dollars per tonne CO<sub>2</sub> ($ t<sup>-1</sup>)") {
    
  }
  return(outcomes)
}

# design is "tscs", "srs", or "ss"
# specifies treatment sampling design only
# control sampling design is always tscs
# tscs needs at least two primary units and at least one secondary per primary
# srs needs at least two samples
# stratified sampling needs at least 
get_bounds = function(pars, side, design) {
  N = pars$N
  A = pars$A
  if(side == "lower") {
    p_1C=2/N # ensure at least two control fields in all cases
    d_2C=1/A # cluster needs at least one secondary sample
    
    # need two fields for cluster, one field for simple or stratified
    if (design == "cluster")
      p_1T = 2/N
    # need one sample per field for cluster, two samples total for simple, two samples per field for stratified
    d_2T = switch(design,
                  cluster=1/A,
                  simple=2/(N*A), # this should be N_T*2/A, but we don't know N_T here...
                  stratified=2/A)
    # always need at least one sample per composite
    n_3C=1
    n_3T=1
  } else if (side == "upper") {
    p_1C = switch(design,
                  cluster=(N-2)/N,
                  simple=(N-1)/N,
                  stratified=(N-1)/N)
    
    # there isn't really an upper bound here, since greater than N-2 just means you resample fields
    if (design == "cluster")
      p_1T = (N-2)/N
    
    # no real upper bounds here but these are reasonable
    d_2C = 100
    d_2T = 100
    n_3C = 100
    n_3T = 100
  }
  if(design=="cluster") {
    c(p_1C, d_2C, n_3C, p_1T, d_2T, n_3T)
  } else if (design == "stratified" | design == "simple") {
    c(p_1C, d_2C, n_3C, d_2T, n_3T)
  }
}

eval_design = function(x, pars, design_names, soc_function) {
  design_pars = as.list(x)
  names(design_pars) = design_names
  do.call(soc_function, c(pars, design_pars))
}

get_and_transform = function(l, name, a, b) {
  l[[name]]*a + b
}

scale_props = function(x, n) {
  x[1] = x[1]*n
  x[4] = x[4]*n
  x
}

scale_props_inv = function(x, n) {
  x[1] = x[1]/n
  x[4] = x[4]/n
  x
}

scale_props2 = function(x, n) {
  x[1] = (2 + (n-4)*x[1])/n
  x[4] = (2 + (n - x[1]*n - 2)*x[4])/n
  
  x
}

scale_props2_inv = function(x, n) {
  x[4] = (x[4]*n - 2)/(n-x[1]*n-2)
  x[1] = (n*x[1] - 2)/(n-4)
  x
}

scale_props3 = function(x, N,A) {
  x[1] = x[1]*N
  x[3] = x[3]/(x[2]*A)
  x[5] = x[5]/(x[4]*A)
  x
}

scale_props3_inv = function(x, N,A) {
  x[1] = x[1]/N
  x[3] = x[3]*x[2]*A
  x[5] = x[5]*x[4]*A
  x
}

exp_last = function(x) {
  x[length(x)] = exp(x[length(x)])
  x
}

exp_last_inv = function(x) {
  x[length(x)] = log(x[length(x)])
  x
}

# power, type s, and type m errors
# note that the type m and type s errors are defined in terms of the project treatment effect not population
analyze_design = function(tau_tildeY, tau_sd, tau_hat_se, n_sims=10000) {
  tau = rnorm(n_sims, mean=tau_tildeY, sd=tau_sd)
  tau_hat = rnorm(n_sims, mean=tau, sd=tau_hat_se)
  
  sig_index = abs(tau_hat) > 1.96*tau_hat_se
  power = mean(sig_index)
  s_error = mean(tau_hat[sig_index]/tau[sig_index] < 0)
  m_error = mean(tau_hat[sig_index]/tau[sig_index])
  
  list(power=power, s_error=s_error, m_error=m_error)
}

add_design_analysis = function(r) {
  c(r, analyze_design(tau_tildeY=r$tau_tildeY, 
                      tau_sd=r$tau_sd,
                      tau_hat_se=r$tau_hat_se))
}



# intercept the first (x) argument to various functions and symmetrize them
# for stabilizing certain optimizations of simple and stratified random sampling
# where we know a priori that the optimum is symmetric in treatment and control
symmetrize = function(x) {
  c(0.5, x, x)
}
