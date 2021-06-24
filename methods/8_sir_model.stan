functions {
  real switch_eta(real t, real t1, real eta, real beta, real nu, real xi) {
    real weight = 1/(1+exp(-xi * (t - t1 - nu)));
    return( beta * (1-weight) + eta * weight );
  }
  real[] sir(real t, real[] y, real[] theta, 
             real[] x_r, int[] x_i) {

      int N = x_i[1];
      real tswitch = x_r[1];
      real tswitch_two = x_r[2];
      
      real beta = theta[1];
      real gamma = theta[2];
      real a = theta[3];
      real eta = theta[4]; // increase/reduction in transmission rate after quarantine
      real eta_two = theta[5]; // increase/reduction in transmission rate after end of quarantine
      real nu = theta[6]; // shift of quarantine implementation
      real nu_two = theta[7]; // shift of quarantine implementation
      real xi = theta[8]; // slope of quarantine implementation
      real i0 = theta[9];
      real e0 = theta[10];
      real beta_eff = switch_eta(t,tswitch, eta, beta, nu, xi);
      real beta_eff_two = switch_eta(t,tswitch_two, eta_two, eta, nu_two, xi);
      real beta_final = beta_eff_two * (t > tswitch_two) + beta_eff * (t <= tswitch_two);
      
      real init[4] = {N - i0 - e0, e0, i0, 0}; // initial values

      real S = y[1] + init[1];
      real E = y[2] + init[2];
      real I = y[3] + init[3];
      real R = y[4] + init[4];
      
      real dS_dt = -beta_final * I * S / N;
      real dE_dt =  beta_final * I * S / N - a * E;
      real dI_dt = a * E - gamma * I;
      real dR_dt =  gamma * I;
      
      return {dS_dt, dE_dt, dI_dt, dR_dt};
  }
}
data {
  int<lower=1> n_days;
  int<lower=1> max_death_day;
  real t0;
  real tswitch; // date of introduction of control measures
  real tswitch_two; // date of reduction in control measures
  real ts[n_days+max_death_day];
  int N; // population size
  int deaths[n_days];
  real death_distribution[max_death_day+1];
  real p_death; // infection fatality rate
}
transformed data {
  int x_i[1] = { N };
  real x_r[2] = {tswitch, tswitch_two};
}
parameters {
  real<lower=0> gamma;
  real<lower=0> beta;
  real<lower=0> a;
  real<lower=0> phi_inv;
  real<lower=0> eta; // reduction/increase in transmission due to control measures (in proportion of beta)
  real<lower=0> eta_two; // reduction/increase in transmission due to control measures (in proportion of beta)
  real<lower=0> nu; // shift of quarantine implementation (strictly positive as it can only occur after tswitch)
  real<lower=0> nu_two; // shift of quarantine implementation (strictly positive as it can only occur after tswitch)
  real<lower=0,upper=1> xi_raw; // slope of quarantine implementation (strictly positive as the logistic must be downward)
  real<lower=0> i0; // number of infected
  real<lower=0> e0;
}
transformed parameters{
  real y[n_days+max_death_day, 4];
  real incidence[n_days+max_death_day - 1];
  real death_incidence[n_days - 1];
  real phi = 1. / phi_inv;
  real xi = xi_raw + 0.5;
  real theta[10];
  theta = {beta, gamma, a, eta, eta_two, nu, nu_two, xi, i0, e0};
  y = integrate_ode_rk45(sir, rep_array(0.0, 4), t0, ts, theta, x_r, x_i);
  for (i in 1:(n_days+max_death_day)-1){
    incidence[i] = -(y[i+1, 2] - y[i, 2] + y[i+1, 1] - y[i, 1]); //-(E(t+1) - E(t) + S(t+1) - S(t))
  }
  // print("Incidence =", incidence);
  // print("P_death =", p_death);
  for (i in 1:n_days -1) {
    death_incidence[i] = 0;
    // print("Time is ", i);
    // if (i > tswitch_two) {
    //   print("Beta is ", switch_eta(i,tswitch_two, eta_two, eta, nu, xi));
    // } else {
    //   print("Beta is ", switch_eta(i,tswitch, eta, beta, nu, xi));
    // }
    for (j in 0:max_death_day) {
      death_incidence[i] = death_incidence[i] + incidence[(max_death_day - j)+i]*p_death * death_distribution[j+1]; 
    }
  }
  // print("Death Incidence =", death_incidence);
}
model {
  //priors
  beta ~ normal(1.5, 1) T[0,];
  gamma ~ normal(0.3, 0.5) T[0,];
  a ~ normal(0.4, 0.5) T[0,];
  phi_inv ~ exponential(5);
  i0 ~ normal(0, 10);
  e0 ~ normal(0, 10);
  eta ~ normal(1, 1) T[0,];
  eta_two ~ normal(1, 1) T[0,];
  nu ~ exponential(1./5);
  nu_two ~ exponential(1./5);
  xi_raw ~ beta(1, 1);
  p_death ~ beta(100, 99000);
  
  //sampling distribution
  //col(matrix x, int n) - The n-th column of matrix x. Here the number of infected people 
  deaths[1:(n_days-1)] ~ neg_binomial_2(death_incidence, phi);
}
generated quantities {
  real R0 = beta / gamma;
  real Reff[n_days];
  real recovery_time = 1 / gamma;
  real incubation_time = 1 / a;
  real pred_cases[n_days+max_death_day-1];
  real pred_deaths[n_days-1];
  pred_cases = neg_binomial_2_rng(incidence, phi);
  pred_deaths = neg_binomial_2_rng(death_incidence, phi);
  for (i in 1:n_days)
    if (i > tswitch_two) {
      Reff[i] = switch_eta(i, tswitch_two, eta_two, eta, nu, xi) / gamma;
    } else {
      Reff[i] = switch_eta(i, tswitch, eta, beta, nu_two, xi) / gamma;
    }
}
