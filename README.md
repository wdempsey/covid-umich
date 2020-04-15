# The Hypothesis of Testing: Paradoxes arising out of reported coronavirus case-counts  #

Many statisticians, epidemiologists, economists and data scientists have registered their serious reservations regarding the reported coronavirus case-counts. Comparing countries and states using those case-counts seem inappropriate when every nation/state have adopted different testing strategies and protocols. Estimating prevalence of COVID-19 based on these data is a hopeless exercise and several groups have recently argued for estimating the number of truly infected cases by using mortality rates. In this project, we aim to (a)	posit a conceptual mathematical framework to characterize sampling bias, misclassification/imperfection of the test, and heterogeneity  in the reproductive number simultaneously on the estimation of the prevalence rate, (b)	review current testing strategies in some of the countries where we have testing data, and (c) provide guidelines for testing strategy/disease surveillance that may help track the pulse of the epidemic, to identify disease free areas and identify disease outbreaks.

## Project Description ##
This project includes the code needed to reproduce results.  This includes (A) sourcing both US and World testing  (B) algorithmic development, and (C) application of models to the cleaned datasets. If using this code please cite the paper using the following bibtex: 

```tex
@article{dempsey:2020,
author = {Du, Jiacong and Dempsey, Walter and Mukherjee, Bhramar},
title = {The Hypothesis of Testing: Paradoxes arising out of reported coronavirus case-counts},
booktitle = {arXiv},
year = {2020}}
```
## Code Description ##

If there are steps to run the code list them as follows: 

0. Dependencies: all code is developed in Python using [Anaconda](https://anaconda.org/about).
* The Anaconda environment can be installed using [covid.yml](./bayesian.yml). See [here](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#creating-an-environment-from-an-environment-yml-file) for instructions on creating the environment.  Simply open Anaconda shell, open to github repo and run:
```
conda env create -f bayesian.yml
```
1. Datasets and exploratory data analysis
* World testing data is accessed [here](https://ourworldindata.org/covid-testing) and country population totals is accessed [here](https://www.worldometers.info/world-population/population-by-country/)
* US testing data is accessed [here](https://covidtracking.com/) and US population totals is accessed [here](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html).  For AS, GU, MP, and VI are extracted from [here](https://www.google.com/publicdata/explore?ds=d5bncppjof8f9_&met_y=sp_pop_totl&idim=country:ASM:VIR:MNP&hl=en&dl=en)
* [Exploratory data analysis](/exploratory_data_analysis) is presented as a set of ipython notebooks. Descriptive statistics are used to inform the prior on the measurement-error models using in the analysis phase
2. The [methods directory](/methods) contains all algorithms for estimation under selection bias, measurement-error, and heterogeneity.  Algorithms are developed within the [pymc3](https://docs.pymc.io/).  
3. All evaluation functions can be found in the [the evaluation directory](/evaluation).  In particular, we perform posterior predictive checks to confirm model fit to the data.
4. Final report can be found in [the write-up directory](/write-up)
