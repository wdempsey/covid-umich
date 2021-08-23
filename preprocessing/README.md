# Step-by-step instructions

This folder contains all pre-processing code.  Each step may have inputs from prior `methods` and therefore a step-by-step set of instructions is listed below concerning sequence of fitting the models and cleaning the data.

## Prepare Facebook symptom survey and Indiana case count data
| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------------------------------------------------|
| [`1_fb_cleaning.R`](./1_fb_clearning) | Builds Indiana only Facebook dataset |
| [`1_indiana_cleaning.R`](./1_fb_clearning) | Cleans the Indiana data to calculate weekly tests and counts per age, gender, ethnicity, and race. |
| [`./preprocessing/1_indiana_cleaning.R`](./1_fb_clearning) | Cleans the Indiana data to calculate weekly tests and counts per age, gender, ethnicity, and race. |

To run this set of code, simply run `bash clean.sh` in the pre-processing folder.  This will generate all the cleaned files and prepare 

## Model 1: preprocessing

- Prerequisites include
    + Cleaning

| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------:|:-----------------------------------------------------------------------|
| [`1_fb_cleaning.R`](./preprocessing/1_fb_clearning) | Builds single  the FB data . |
| [`./preprocessing/1_indiana_cleaning.R`](./preprocessing/1_fb_clearning) | Cleans the FB data . |

To run this set of code, simply run `bash clean.sh` in the pre-processing folder.  This will generate all the cleaned files and prepare 

## Model 2: preprocessing
| <img height=0 width=800> File Name <img height=0 width=800> | <img height=0 width=1000> Brief Description <img height=0 width=1000> |
|:-----------------------------:|:-----------------------------:|:-----------------------------------------------------------------------|
| [`1_fb_cleaning.R`](./preprocessing/1_fb_clearning) | Builds single  the FB data . |
| [`./preprocessing/1_indiana_cleaning.R`](./preprocessing/1_fb_clearning) | Cleans the FB data . |

To run this set of code, simply run `bash clean.sh` in the pre-processing folder.  This will generate all the cleaned files and prepare 


