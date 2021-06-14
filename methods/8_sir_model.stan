functions {
  real switch_eta(real t, real t1, real exp_eta) {
    real adj;
    if( t < t1 ) {
      adj = 1.0;
    }
    else {
      adj = exp_eta;
    }
    return(adj);
  }
  real[] sir(real t, real[] y, real[] theta, 
             real[] x_r, int[] x_i) {

      int N = x_i[1];
      real tswitch = x_r[1];
      
      real beta = theta[1];
      real gamma = theta[2];
      real a = theta[3];
      real exp_eta = theta[4]; // reduction in transmission rate after quarantine
      real i0 = theta[5];
      real e0 = theta[6];
      real forcing_function = switch_eta(t,tswitch,exp_eta); // switch function
      real beta_eff = beta * forcing_function; // beta increased/decreased to take control measures into account
      real init[4] = {N - i0 - e0, e0, i0, 0}; // initial values

      real S = y[1] + init[1];
      real E = y[2] + init[2];
      real I = y[3] + init[3];
      real R = y[4] + init[4];
      
      real dS_dt = -beta_eff * I * S / N;
      real dE_dt =  beta_eff * I * S / N - a * E;
      real dI_dt = a * E - gamma * I;
      real dR_dt =  gamma * I;
      
      print(t);
      print(forcing_function);
      
      return {dS_dt, dE_dt, dI_dt, dR_dt};
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
  real<lower=0> beta;
  real<lower=0> a;
  real<lower=0> phi_inv;
  real<lower=0> eta; // increase/reduction in transmission due to control measures (in proportion of beta)
  real<lower=0> i0; // number of infected
  real<lower=0> e0;
}
transformed parameters{
  real y[n_days+max_death_day, 4];
  real incidence[n_days+max_death_day - 1];
  real death_incidence[n_days - 1];
  real phi = 1. / phi_inv;
  real exp_eta = exp(eta);
  real theta[6];
  theta = {beta, gamma, a, exp_eta, i0, e0};
  y = integrate_ode_rk45(sir, rep_array(0.0, 4), t0, ts, theta, x_r, x_i);
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
  gamma ~ normal(0.5, 0.5);
  a ~ normal(0.4, 0.5);
  phi_inv ~ exponential(5);
  i0 ~ normal(0, 10);
  e0 ~ normal(0, 10);
  eta ~ normal(0, 0.5);
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
    Reff[i] = switch_eta(i, tswitch, exp_eta) * beta / gamma;
}


