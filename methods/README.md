# Step-by-step instructions

This folder contains all methods code.  Each step has inputs from prior `pre-processing` and therefore a step-by-step set of instructions is listed below concerning sequence of fitting the models and cleaning the data.

## Model 1: imputation + estimation

We first build the imputation models for contact and then fever status given demographics and testing.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`1_contact_propensity_estimation.R`](./1_contact_propensity_estimation.R) | Estimates the likelihood of contact given demographics and COVID-19 testing outcome (positive or negative) for individuals who reported getting tested |
| [`1_contact_propensity_estimation.R`](./1_contact_propensity_estimation.R) | Estimates the likelihood of contact given demographics and COVID-19 testing outcome (positive or negative) for individuals who reported getting tested |

After running these two imputation models, return the preprocessing stage [`4_propensity_preproc.R`](../preprocessing/4_propensity_preproc.R) to clean data for the propensity estimation phase.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`1_contact_propensity_estimation.R`](./1_contact_propensity_estimation.R) | Estimates the likelihood of contact given demographics and COVID-19 testing outcome (positive or negative) for individuals who reported getting tested |
| [`1_contact_propensity_estimation.R`](./1_contact_propensity_estimation.R) | Estimates the likelihood of contact given demographics and COVID-19 testing outcome (positive or negative) for individuals who reported getting tested |

After running these two imputation models, return the preprocessing stage [`4_propensity_preproc.R`](../preprocessing/4_propensity_preproc.R) to clean data for the propensity estimation phase.



