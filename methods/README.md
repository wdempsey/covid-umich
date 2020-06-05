## Simulation and data analysis scripts

| <img height=0 width=1000> File Name <img height=0 width=1000> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`us_testing.ipynb`](https://github.com/wdempsey/covid-umich/new/master/methods/us_testing.ipynb) | Plots of State and Country Level testing and data quality calculation|
| [`simple-models-sr.py`](https://github.com/wdempsey/sense2stop-lvm/blob/master/methods/simple-models-sr.py) | Using self-report (SR) data only, the following log-linear models of mean counts (`mu`) were considered: (1) `log(mu) = beta` (2) `mu = posquit_mu * is_post_quit + prequit_mu*(1 - is_post_quit)` where `log(postquit_mu) = beta_postq` and `log(prequit_mu) = beta_preq`|
| [`randeff-models-sr.py`](https://github.com/wdempsey/sense2stop-lvm/blob/master/methods/randeff-models-sr.py) | Using self-report (SR) data only, the following log-linear models of mean counts for the `i`th participant (`mu_i`) were estimated: (1) `log(mu_i) = beta + gamma_i` (2) `mu_i = posquit_mu_i * is_post_quit + prequit_mu_i*(1 - is_post_quit)` where `log(postquit_mu_i) = gamma_postq_i + beta_postq` and `log(prequit_mu_i) = gamma_preq_i + beta_preq`|

