# Step-by-step instructions

This folder contains all pre-processing code.  Each step may have inputs from prior `methods` and therefore a step-by-step set of instructions is listed below concerning sequence of fitting the models and cleaning the data.

## Prepare Facebook symptom survey and Indiana case count data
| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`1_fb_cleaning.R`](./1_fb_clearning) | Extracts individuals who state they live in Indiana from FB dataset |
| [`1_indiana_cleaning.R`](./1_indiana_cleaning.R) | Cleans the Indiana data to calculate weekly tests and counts per age, gender, ethnicity, and race. |
| [`2_fb_in_merge_code.R`](./2_fb_in_merge_code.R) | Generates weekly FB weights per age, gender, FB fever, and FB COVID-19 positive contact |

## Model 1: preprocessing

Cleaning here is used in the first imputation method which calculates probability of contact given demographics and COVID test status as well as probability of symptoms given same and contact.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`3_feverfb_cleaning.R`](./3_feverfb_cleaning.R) | Extracts individuals who state they live in Indiana from FB dataset |
| [`3_feverfb_merge.R`](./3_feverfb_merge.R) | Generates weekly FB weights x contact by demographic and test outcome as well as FB weights x symptoms by same + contact. |
| [`3_feverfb--preproc.R`](./3_feverfb--preproc.R) | Incorporates ethnicity into the weights x outcome files |
| [`4_propensity_preproc.R`](./4_propensity_preproc.R) | Imputes the missing symptom and contact values |


After running pre-processing, fit the imputation models using [`1_contact_propensity_estimation.R`](../methods/1_contact_propensity_estimation.R) and [`1_symptom_propensity_estimation.R`](../methods/1_symptom_propensity_estimation.R) respectively.  Then return to impute symptom and COVID-19 using [`4_propensity_preproc.R`](./4_propensity_preproc.R).

## Model 2: preprocessing

Cleaning here is used in the second imputation method which calculates probability of symptoms given demographics and COVID test status.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`5_hospfb_merge.R`](./5_hospfb_merge.R) | Generates weekly FB weights x symptoms by demographic and test outcome |
| [`5_hospfb--preproc.R`](./5_hospfb--preproc.R) | Incorporates ethnicity into the weights x outcome files |
| [`5_in_hops_cleaning.R`](./5_in_hops_cleaning.R) | Cleans hospital data to construct weekly hospital counts |
| [`5_in_hops_cleaning--preproc.R`](./5_in_hops_cleaning--preproc.R) | Imputes COVID-19 with symptom + demographics |

After running pre-processing, fit the imputation models using [`4_symptom_neg_propensity_estimation_alt.R`](../methods/4_symptom_neg_propensity_estimation_alt.R) and [`4_symptom_pos_propensity_estimation_alt.R`](../methods/4_symptom_pos_propensity_estimation_alt.R) respectively.  Then return to impute symptom status using [`5_in_hops_cleaning.R`](./5_in_hops_cleaning.R) and [`5_in_hops_cleaning--preproc.R`](./5_in_hops_cleaning--preproc.R).



## Model 3: Death modeling pre-processing

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`6_in_death_cleaning.R`](./6_in_death_cleaning.R) | Generates weekly FB weights x symptoms by demographic and test outcome |
| [`7_ifr.R`](./7_ifr.R) | Generates age strata-specific IFRs based on published rates in the literature.  |
| [`8_modelbased_preproc.R`](./8_modelbased_preproc.R) | Constructs active infection rate based on the SEIR model posterior predictive checks. |