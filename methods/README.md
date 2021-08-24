# Step-by-step instructions

This folder contains all methods code.  Each step has inputs from prior `pre-processing` and therefore a step-by-step set of instructions is listed below concerning sequence of fitting the models and cleaning the data.

## Model 1: preprocessing

We first build the imputation models for contact and then fever status given demographics and testing.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`3_feverfb_cleaning.R`](./3_feverfb_cleaning.R) | Extracts individuals who state they live in Indiana from FB dataset |
| [`3_feverfb_merge.R`](./3_feverfb_merge.R) | Generates weekly FB weights x contact by demographic and test outcome as well as FB weights x symptoms by same + contact. |
| [`3_feverfb--preproc.R`](./1_fb_clearning) | Incorporates ethnicity into the weights x outcome files |

After running pre-processing, fit the imputation models using [`1_contact_propensity_estimation.R`](../methods/1_contact_propensity_estimation.R) and [`1_symptom_propensity_estimation.R`](../methods/1_symptom_propensity_estimation.R) respectively.


