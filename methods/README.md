# Step-by-step instructions

This folder contains all methods code.  Each step has inputs from prior `pre-processing` and therefore a step-by-step set of instructions is listed below concerning sequence of fitting the models and cleaning the data.

## Model 1: imputation + estimation

We first build the imputation models for contact and then fever status given demographics and testing.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`1_contact_propensity_estimation.R`](./1_contact_propensity_estimation.R) | Estimates the likelihood of contact given demographics and COVID-19 testing outcome (positive or negative) for individuals who reported getting tested |
| [`1_contact_propensity_estimation.R`](./1_contact_propensity_estimation.R) | Estimates the likelihood of contact given demographics and COVID-19 testing outcome (positive or negative) for individuals who reported getting tested |
| [`2_propensity_estimation.R`](./2_propensity_estimation.R) | Estimates the likelihood of testing given demographics, contact, and symptoms |
| [`3_invweight_estimation.R`](./3_invweight_estimation.R) | Builds IPW estimates of the AIR per week |

After running these two imputation models, return the preprocessing stage [`4_propensity_preproc.R`](../preprocessing/4_propensity_preproc.R) to clean data for the propensity estimation phase.

## Model 2: imputation + estimation

We first build the imputation models for contact and then fever status given demographics and testing.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`4_symptom_neg_propensity_estimation_alt.R`](./4_symptom_neg_propensity_estimation_alt.R) | Estimates the likelihood of contact given demographics and COVID-19 testing outcome (negative) and hospitalization indicator for individuals who reported getting tested |
| [`4_symptom_pos_propensity_estimation_alt.R`](./4_symptom_pos_propensity_estimation_alt.R) | Estimates the likelihood of symptoms given demographics and COVID-19 testing outcome (positive or negative) for individuals who reported getting tested |
| [`5_propensity_estimation_alt.R`](./5_propensity_estimation_alt.R) | Estimates the likelihood of testing given demographics, contact, and symptoms |
| [`6_invweight_estimation_alt.R`](./6_invweight_estimation_alt.R) | Builds IPW estimates of the AIR per week |

After running these two imputation models, return the preprocessing stage [`4_propensity_preproc.R`](../preprocessing/4_propensity_preproc.R) to clean data for the propensity estimation phase.

## Model 2: imputation + estimation

We first build the imputation models for contact and then fever status given demographics and testing.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`4_symptom_neg_propensity_estimation_alt.R`](./4_symptom_neg_propensity_estimation_alt.R) | Estimates the likelihood of contact given demographics and COVID-19 testing outcome (negative) and hospitalization indicator for individuals who reported getting tested |
| [`4_symptom_pos_propensity_estimation_alt.R`](./4_symptom_pos_propensity_estimation_alt.R) | Estimates the likelihood of symptoms given demographics and COVID-19 testing outcome (positive or negative) for individuals who reported getting tested |
| [`5_propensity_estimation_alt.R`](./5_propensity_estimation_alt.R) | Estimates the likelihood of testing given demographics, contact, and symptoms |
| [`6_invweight_estimation_alt.R`](./6_invweight_estimation_alt.R) | Builds IPW estimates of the AIR per week |

After running these two imputation models, return the preprocessing stage using [`5_in_hops_cleaning.R`](./5_in_hops_cleaning.R) and [`5_in_hops_cleaning--preproc.R`](./5_in_hops_cleaning--preproc.R) to clean data for the propensity estimation phase.

## Model 3: imputation + estimation

We first build the imputation models for contact and then fever status given demographics and testing.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`4_symptom_neg_propensity_estimation_alt.R`](./4_symptom_neg_propensity_estimation_alt.R) | Estimates the likelihood of contact given demographics and COVID-19 testing outcome (negative) and hospitalization indicator for individuals who reported getting tested |
| [`4_symptom_pos_propensity_estimation_alt.R`](./4_symptom_pos_propensity_estimation_alt.R) | Estimates the likelihood of symptoms given demographics and COVID-19 testing outcome (positive or negative) for individuals who reported getting tested |
| [`5_propensity_estimation_alt.R`](./5_propensity_estimation_alt.R) | Estimates the likelihood of testing given demographics, contact, and symptoms |
| [`6_invweight_estimation_alt.R`](./6_invweight_estimation_alt.R) | Builds IPW estimates of the AIR per week |

After running these two imputation models, return the preprocessing stage using [`5_in_hops_cleaning.R`](./5_in_hops_cleaning.R) and [`5_in_hops_cleaning--preproc.R`](./5_in_hops_cleaning--preproc.R) to clean data for the propensity estimation phase.

## Other analysis files

We first build the imputation models for contact and then fever status given demographics and testing.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`7_indiana_randomsample_analysis.R`](./7_indiana_randomsample_analysis.R) | Detailed analysis of the Indiana random sample from Yiannoutsos et al. (2021)  |
| [`11_effss_calcs.R`](./11_effss_calcs.R) | Estimates effective sample size for choices of prevalence and relative sampling rates given in Appendix B.7  |

