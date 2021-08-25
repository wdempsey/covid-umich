# The Hypothesis of Testing: Paradoxes arising out of reported coronavirus case-counts  #

Coronavirus case-count data has influenced government policies and drives most epidemiological forecasts. Limited testing is cited as the key driver behind minimal information on the COVID-19 pandemic. While expanded testing is laudable, measurement error and selection bias are the two greatest problems limiting our understanding of the COVID-19 pandemic; neither can be fully addressed by increased testing capacity. In this paper, we demonstrate their impact on estimation of point prevalence and the effective reproduction number. We show that estimates based on the millions of molecular tests in the US has the same mean square error as a small simple random sample.  To address this, a procedure is presented that combines case-count data and random samples over time to estimate selection propensities based on key covariate information. We then combine these selection propensities with epidemiological forecast models to construct a \emph{doubly robust} estimation method that accounts for both measurement-error and selection bias.  This method is then applied to estimate Indiana's prevalence using case-count, hospitalization, and death data with cumulative demographic information, the United States's only statewide random molecular sample collected from April 25--29th, and Facebook's COVID-19 symptom survey.  We end with a series of recommendations based on the proposed methodology.

## Project Description ##
This project includes the code needed to reproduce results.  This includes (A) sourcing both US and World testing  (B) algorithmic development, and (C) application of models to the cleaned datasets. If using this code please cite the paper using the following bibtex: 

```tex
@article{dempsey:2020,
author = {Dempsey, Walter},
title = {The Hypothesis of Testing: Paradoxes arising out of reported coronavirus case-counts},
booktitle = {arXiv},
year = {2020}}
```
## Code Description ##

If there are steps to run the code list them as follows: 

0. Dependencies: all code is developed in R.
1. Datasets and exploratory data analysis
* [Indiana case count data](https://hub.mph.in.gov/dataset?q=COVID)
* [Facebook Symptom Survey](https://dataforgood.fb.com/docs/covid-19-symptom-survey-request-for-data-access/) 
* [Random Sample from Indiana](https://www.cdc.gov/mmwr/volumes/69/wr/mm6929e1.htm)
2. The [methods directory](/methods) contains all relevant code to this project.
4. Final reports can be found in [the write-up directory](/write-up)
