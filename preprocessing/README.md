# Step-by-step instructions

This folder contains all pre-processing code.  Each step may have inputs from prior `methods` and therefore a step-by-step set of instructions is listed below concerning sequence of fitting the models and cleaning the data.

## Prepare Facebook symptom survey and Indiana case count data
| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`1_fb_cleaning.R`](./1_fb_clearning) | Extracts individuals who state they live in Indiana from FB dataset |
| [`1_indiana_cleaning.R`](./1_indiana_cleaning.R) | Cleans the Indiana data to calculate weekly tests and counts per age, gender, ethnicity, and race. |
| [`2_fb_in_merge_code.R`](./2_fb_in_merge_code.R) | Generates weekly FB weights per age, gender, FB fever, and FB COVID-19 positive contact |

## Model 1: preprocessing

Cleaning here is used in the first imputation method which calculates probability of contact given demographics and COVID test status as well as probability of fever given same and contact.

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`3_feverfb_cleaning.R`](./3_feverfb_cleaning.R) | Extracts individuals who state they live in Indiana from FB dataset |
| [`3_feverfb_merge.R`](./3_feverfb_merge.R) | Generates weekly FB weights x contact by demographic and test outcome as well as FB weights x fever by same + contact. |
| [`3_feverfb--preproc.R`](./1_fb_clearning) | Incorporates ethnicity into the weights x outcome files |


## Model 2: preprocessing
| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`1_fb_cleaning.R`](./preprocessing/1_fb_clearning) | Builds single  the FB data . |
| [`./preprocessing/1_indiana_cleaning.R`](./preprocessing/1_fb_clearning) | Cleans the FB data . |

To run this set of code, simply run `bash clean.sh` in the pre-processing folder.  This will generate all the cleaned files and prepare 


