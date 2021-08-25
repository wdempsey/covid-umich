# The Hypothesis of Testing: Paradoxes arising out of reported coronavirus case-counts  #

Many statisticians, epidemiologists, economists and data scientists have registered serious reservations regarding the reported coronavirus case-counts. Limited testing capacity across the country has been  identified as a key driver of suppressed coronavirus case-counts.  The calls to increase testing capacity are well-justified as they become a more frequent point of discussion in the public sphere.  While expanded testing is a laudable goal, selection bias will impact estimates of disease prevalence and the effective reproduction number until the entire population is sampled.  Moreover, tests are imperfect as false positive/negative rates interact in complex ways with selection bias.  In this paper, we attempt to clarify this interaction.  Through simple calculations, we demonstrate pitfalls and paradoxes that can arise when considering case-count data in the presence of selection bias and measurement error.

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
