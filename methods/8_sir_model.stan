functions {
  real switch_eta(real t, real t1, real eta, real nu, real xi) {
    return(eta + (1 - eta) / (1 + exp(xi * (t - t1 - nu))));
  }
  real[] sir(real t, real[] y, real[] theta, 
             real[] x_r, int[] x_i) {

      int N = x_i[1];
      real tswitch = x_r[1];
      
      real beta = theta[1];
      real gamma = theta[2];
      real a = theta[3];
      real gamma_two = theta[4]; 
      real eta = theta[5]; // reduction in transmission rate after quarantine
      real nu = theta[6]; // shift of quarantine implementation
      real xi = theta[7]; // slope of quarantine implementation
      real i0 = theta[8];
      real e0 = theta[9];
      real i20  = theta[10];
      real forcing_function = switch_eta(t,tswitch,eta,nu,xi); // switch function
      real beta_eff = beta * forcing_function; // beta decreased to take control measures into account
      real init[5] = {N - i0 - e0, e0, i0, i20, 0}; // initial values

      real S = y[1] + init[1];
      real E = y[2] + init[2];
      real I = y[3] + init[3];
      real I2 = y[3] + init[4];
      real R = y[4] + init[5];
      
      real dS_dt = -beta_eff * I * S / N;
      real dE_dt =  beta_eff * I * S / N - a * E;
      real dI_dt = a * E - gamma * I;
      real dI2_dt = gamma * I - gamma_two * I2;
      real dR_dt =  gamma_two * I2;
      
      return {dS_dt, dE_dt, dI_dt, dI2_dt, dR_dt};
  }
}
data {
  int<lower=1> n_days;
  int<lower=1> max_death_day;
  real t0;
  real tswitch; // date of introduction of control measures
  real ts[n_days+max_death_day];
  int N; // population size
  int deaths[n_days];
  real death_distribution[max_death_day+1];
  real p_death; // infection fatality rate
}
transformed data {
  int x_i[1] = { N };
  real x_r[1] = {tswitch};
}
parameters {
  real<lower=0> gamma;
  real<lower=0> gamma_two;
  real<lower=0> beta;
  real<lower=0> a;
  real<lower=0> phi_inv;
  real<lower=0,upper=1> eta; // reduction in transmission due to control measures (in proportion of beta)
  real<lower=0> nu; // shift of quarantine implementation (strictly positive as it can only occur after tswitch)
  real<lower=0,upper=1> xi_raw; // slope of quarantine implementation (strictly positive as the logistic must be downward)
  real<lower=0> i0; // number of infected, transmitting people inititally
  real<lower=0> i02; // number of infected, non-transmitting people inititally
  real<lower=0> e0;
}
transformed parameters{
  real y[n_days+max_death_day, 4];
  real incidence[n_days+max_death_day - 1];
  real death_incidence[n_days - 1];
  real phi = 1. / phi_inv;
  real xi = xi_raw + 0.5;
  real theta[10];
  theta = {beta, gamma, a, gamma_two, eta, nu, xi, i0, e0, i02};
  y = integrate_ode_rk45(sir, rep_array(0.0, 5), t0, ts, theta, x_r, x_i);
  for (i in 1:(n_days+max_death_day)-1){
    incidence[i] = -(y[i+1, 2] - y[i, 2] + y[i+1, 1] - y[i, 1]); //-(E(t+1) - E(t) + S(t+1) - S(t))
  }
  // print("Incidence =", incidence);
  // print("P_death =", p_death);
  for (i in 1:n_days -1) {
    death_incidence[i] = 0;
    for (j in 0:max_death_day) {
      death_incidence[i] = death_incidence[i] + incidence[(max_death_day - j)+i]*p_death * death_distribution[j+1]; 
    }
  }
  // print("Death Incidence =", death_incidence);
}
model {
  //priors
  beta ~ normal(2, 0.5);
  gamma ~ normal(0.4, 0.5);
  a ~ normal(0.4, 0.5);
  phi_inv ~ exponential(5);
  i02 ~ normal(0, 10);
  i0 ~ normal(0, 10);
  e0 ~ normal(0, 10);
  eta ~ beta(2.5, 4);
  nu ~ exponential(1./5);
  xi_raw ~ beta(1, 1);
  
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
    Reff[i] = switch_eta(i, tswitch, eta, nu, xi) * beta / gamma;
}

