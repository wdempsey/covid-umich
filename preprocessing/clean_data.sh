#!/bin/bash

# Make Weekly Complete Indiana Data
Rscript in_cleaning.R

# Make Weekly Complete Facebook Data
Rscript fb_cleaning.R

# Collapse Facebook Data for ease of use.
Rscript fb-in-merge-code.R

# Collapse Facebook Data for ease of use.
Rscript propensity_prepoc.R

