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
      real tswitch_three = x_r[3];
      
      real beta = theta[1];
      real gamma = theta[2];
      real a = theta[3];
      real eta = theta[4]; // increase/reduction in transmission rate after quarantine
      real eta_two = theta[5]; // increase/reduction in transmission rate after end of quarantine
      real eta_three = theta[6]; // increase/reduction in transmission rate after end of quarantine
      real nu = theta[7]; // shift of quarantine implementation
      real nu_two = theta[8]; // shift of quarantine implementation
      real nu_three = theta[9]; // shift of quarantine implementation
      real xi = theta[10]; // slope of quarantine implementation
      real i0 = theta[11];
      real e0 = theta[12];
      real beta_eff = switch_eta(t,tswitch, eta, beta, nu, xi);
      real beta_eff_two = switch_eta(t,tswitch_two, eta_two, eta, nu_two, xi);
      real beta_eff_three = switch_eta(t,tswitch_three, eta_three, eta_two, nu_three, xi);
      real beta_final = beta_eff_three * (t > tswitch_three) + beta_eff_two * (t > tswitch_two) * (t <= tswitch_three) + beta_eff * (t <= tswitch_two);
      
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
  real tswitch_three; // date of reduction in control measures
  real ts[n_days+max_death_day];
  int N; // population size
  real death_distribution[max_death_day+1];
  int num_ages; // number of age groups
  int deaths[num_ages, n_days]; // deaths by age group
  real p_death[num_ages]; // infection fatality rate per age group
  vector<lower=0>[num_ages] alpha;
}
transformed data {
  int x_i[1] = { N };
  real x_r[3] = {tswitch, tswitch_two, tswitch_three};
}
parameters {
  real<lower=0> gamma;
  real<lower=0> beta;
  real<lower=0> a;
  real<lower=0> phi_inv;
  real<lower=0> eta; // reduction/increase in transmission due to control measures
  real<lower=0> eta_two; // reduction/increase in transmission due to control measures 
  real<lower=0> eta_three; // reduction/increase in transmission due to control measures 
  real<lower=0> nu; // shift of quarantine implementation (strictly positive as it can only occur after tswitch)
  real<lower=0> nu_two; // shift of quarantine implementation (strictly positive as it can only occur after tswitch)
  real<lower=0> nu_three; // shift of quarantine implementation (strictly positive as it can only occur after tswitch)
  real<lower=0,upper=1> xi_raw; // slope of quarantine implementation (strictly positive as the logistic must be downward)
  real<lower=0> i0; // number of infected
  real<lower=0> e0;
  simplex[num_ages] varphi[1];
}
transformed parameters{
  real y[n_days+max_death_day, 4];
  real incidence[n_days+max_death_day - 1];
  real infected[n_days+max_death_day - 1];
  real recovered[n_days+max_death_day - 1];
  real new_recovered[n_days+max_death_day - 1];
  real incidence_by_age[num_ages, n_days+max_death_day-1];
  real death_incidence[num_ages, n_days - 1];
  real phi = 1. / phi_inv;
  real xi = xi_raw + 0.5;
  real theta[12];
  theta = {beta, gamma, a, eta, eta_two, eta_three, nu, nu_two, nu_three, xi, i0, e0};
  y = integrate_ode_rk45(sir, rep_array(0.0, 4), t0, ts, theta, x_r, x_i);
  for (i in 1:(n_days+max_death_day)-1){
    incidence[i] = -(y[i+1, 2] - y[i, 2] + y[i+1, 1] - y[i, 1]); //-(E(t+1) - E(t) + S(t+1) - S(t))
    infected[i] = y[i, 3];
    recovered[i] = y[i, 4];
  }
  for (k in 1:num_ages) {
    for (i in 1:(n_days+max_death_day)-1){
      incidence_by_age[k,i] = incidence[i] * varphi[1][k];
    }
  }
  for (k in 1:num_ages) {
    for (i in 1:n_days -1) {
      death_incidence[k,i] = 0;
      for (j in 0:max_death_day) {
        death_incidence[k,i] = death_incidence[k,i] + incidence_by_age[k,(max_death_day - j)+i]*p_death[k] * death_distribution[j+1]; 
      }
    }
  }
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
  eta_three ~ normal(1, 1) T[0,];
  nu ~ exponential(1./5);
  nu_two ~ exponential(1./5);
  nu_three ~ exponential(1./5);
  xi_raw ~ beta(1, 1);
  //sampling distribution
  //col(matrix x, int n) - The n-th column of matrix x. Here the number of infected people 
  for (k in 1:num_ages) {
    for (i in 1:n_days-1) {
      deaths[k,i] ~ neg_binomial_2(death_incidence[k,i], phi);
    }
  }
  
  // This is the Dirichlet prior on the fraction to each age group
  varphi[1] ~ dirichlet(alpha);
  
}
generated quantities {
  real R0 = beta / gamma;
  real Reff[n_days];
  real recovery_time = 1 / gamma;
  real incubation_time = 1 / a;
  real pred_cases[n_days+max_death_day-1];
  real pred_active_infections[n_days+max_death_day-1];
  real pred_recovered[n_days+max_death_day-1];
  real pred_cases_per_agegroup[num_ages, n_days+max_death_day-1];
  real pred_deaths[num_ages, n_days-1];
  pred_cases = neg_binomial_2_rng(incidence, phi);
  pred_active_infections = infected;
  pred_recovered = recovered;
  for (k in 1:num_ages) {
    for (i in 1:n_days-1) {
      pred_deaths[k,i] = neg_binomial_2_rng(death_incidence[k,i], phi);
    }
    for (i in 1:(n_days+max_death_day-1)) {
      pred_cases_per_agegroup[k,i] = neg_binomial_2_rng(incidence_by_age[k,i], phi);
    }
  }
  for (i in 1:n_days)
    if (i > tswitch_two) {
      if (i > tswitch_three) {
        Reff[i] = switch_eta(i, tswitch_two, eta_three, eta_two, nu_three, xi) / gamma;
      } else {
        Reff[i] = switch_eta(i, tswitch_two, eta_two, eta, nu_two, xi) / gamma; 
      }
    } else {
      Reff[i] = switch_eta(i, tswitch, eta, beta, nu, xi) / gamma;
    }
}
