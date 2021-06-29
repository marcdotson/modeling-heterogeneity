Code
================

Scripts with prefixes (e.g., `01_import-data.R`, `02_clean-data.R`) and
functions in `/Source`.

## R Scripts

  - `00_hmnl-simulate-data.R` Simulate data according to a hierarchical
    multinomial logit.
  - `01_experimental-design.R` Generate an experimental design for the
    conjoint study.
  - `02_exploratory-data-analysis.R` Perform exploratory data analysis.
  - `03_model-calibration.R` Calibrate the model.
  - `04_model-checking.R` Perform model checking.
  - `05_model-free-evidence.R` Explore model-free evidence.
  - `06_model-validation.R` Validating the model results using ownership
    and the recontact survey data.

## Source

  - `hmnl_centered.stan` Centered parameterization of the hierarchical
    multinomial logit in Stan.
  - `hier_mnl.R` Hierarchical multinomial logit using random-walk
    Metropolis.
  - `model_fit.R` Compute model fit.
