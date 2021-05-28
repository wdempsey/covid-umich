functions {
  real[] sir(real t, real[] y, real[] theta, 
             real[] x_r, int[] x_i) {

      real S = y[1];
      real I = y[2];
      real R = y[3];
      int N = x_i[1];
      
      real beta = theta[1];
      real gamma = theta[2];
      
      real dS_dt = -beta * I * S / N;
      real dI_dt = beta * I * S / N - gamma * I;
      real dR_dt =  gamma * I;
      
      return {dS_dt, dI_dt, dR_dt};
  }
}
data {
  int<lower=1> n_days;
  real t0;
  real y0[3];
  real ts[n_days];
  int N;
  int cases[n_days];
  real death_rate[28];
}
transformed data {
  real x_r[0];
  int x_i[1] = { N };
}
parameters {
  real<lower=0> gamma;
  real<lower=0> beta;
  real<lower=0> phi_inv;
  real<lower=0, upper=1> p_reported; // proportion of infected (symptomatic) people reported
}
transformed parameters{
  real y[n_days, 3];
  real incidence[n_days - 1];
  real phi = 1. / phi_inv;
  real deaths[n_days - 1];
  // initial compartement values
  real theta[2] = {beta, gamma};
  y = integrate_ode_rk45(sir, y0, t0, ts, theta, x_r, x_i);
  for (i in 1:n_days-1){
    incidence[i] =  (y[i, 1] - y[i+1, 1]) * p_reported;
  }
}
model {
  //priors
  beta ~ normal(2, 1);
  gamma ~ normal(0.4, 0.5);
  phi_inv ~ exponential(5);
  //p_reported ~ beta(2, 3);//normal(0, 0.5);
  p_reported ~ beta(1, 2);
  //sampling distribution
  //col(matrix x, int n) - The n-th column of matrix x. Here the number of infected people 
  cases[1:(n_days-1)] ~ neg_binomial_2(incidence, phi);
}
generated quantities {
  real R0 = beta / gamma;
  real recovery_time = 1 / gamma;
  //real incubation_time = 1 / a;
  real pred_cases[n_days-1];
  pred_cases = neg_binomial_2_rng(incidence, phi);
}