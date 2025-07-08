README
================

# HepatitisVaccine Project

This repository documents the code and results for the
**HepatitisVaccine** project, which models and visualizes the
spatial-temporal patterns of hepatitis B vaccination coverage (HEP1 and
HEP3) across Africa.

## Environment

The code was developed and tested on the following system:

- OS: Ubuntu 18.04 LTS  
- Kernel: Linux version 4.15.0-136-generic  
- R Version: 3.6.3 (2020-02-29)  
- IDE: RStudio Server 2021.09.0 Build 351

## Folder Structure

### 1. `Code/` folder

This folder contains all scripts used for data processing, modeling, and
results generation.

#### Main Scripts

- `hepB_Main.R`: Main modeling script for HEP3 (third dose).
- `hepB_Main_d1.R`: Main modeling script for HEP1 (first dose).

#### Support Scripts

##### Part 1: Preprocessing and Modeling

- `RegularizeProcess.R`: Handles raw data preprocessing including
  logistic transformation and normalization.
- `makePolygon.R`: Generates mesh polygons for spatial modeling.
- `ml_function.R`: Implements machine learning models and outputs
  sub-model predictions.
- `stack_synthesizer_newmesh.R`: Constructs the projection matrix (A
  matrix) and generates the INLA stack object.
- `output_calc.R`: Defines `output_calc_function`, the core function
  calling INLA for posterior inference.

##### Part 2: Post-processing and Result Extraction

- `pred_hyperparameter.R`: Performs posterior prediction and
  hyperparameter sampling based on INLA and spatial models.
- `parallel_extract_function_WGS4326.R`: Aggregates and summarizes
  prediction results (mean, SD, 95% UI) in parallel.
- `model_linkage_WGS4326.R`: Links posterior samples with spatial
  attributes and produces spatial predictions.
- `spatial_extraction.R`: Calls `model_linkage()` for each year and
  compiles a list of yearly spatial predictions.
- `ml_predcsv.R`:
  - `cv_predict()`: Cross-validation for ML models (e.g., `glmnet`,
    `gam`, `ranger`).  
  - `ml_predcsv()`: Predicts on new data using trained ML model
    ensemble.
- `all_af.R`: Generates country-level HEP3 estimates and uncertainty
  intervals for all years using raked results.
- `all_af_d1.R`: Same as above, for HEP1.
- `cross_validation.R` / `cross_validation_dose1.R`: Scripts for
  cross-validation of HEP3 and HEP1 models.

##### CV-related Utilities

- `RegularizeProcessCV.R`: Preprocessing pipeline for CV.
- `stack_synthesizer_cv.R`: Builds A matrix and stack object for CV.
- `cv_data_extratcor.R`: Extracts posterior predictions and summary
  statistics for CV points.

##### Difference and Trend Analysis

- `diff_rake_rdata.R`: Calculates spatial differences and relative
  changes between HEP1 and HEP3 after raking.
- `diffrdiff.R`: Performs year-by-year calibration and computes
  absolute/relative differences at multiple admin levels.
- `post_allyear.R`: Trains ML model on HEP3 and generates yearly 5km
  predictions (Shapefiles).
- `post_d3_20202021.R` / `post_d1_20202021.R`: Predicts HEP3 and HEP1 at
  high resolution for recent years.
- `post_d3_region.R` / `post_d1_region.R`: Produces population-weighted
  regional predictions with 1000 simulation means.
- `post_diff_rake.R`: Compares spatial estimates across years for HEP3
  and calculates raked differences.
- `post_different_years.R`: Deprecated version of `post_diff_rake.R` (no
  raking).
- `post_p1p3_2010to2021.R`: Summarizes and compares HEP1 and HEP3 across
  years and admin levels.
- `rake&rltv_d1.R`: Computes relative change between 2010 and 2022 for
  HEP1 at 5km resolution.
- `rake_functions.R`: Iterative logit-based raking function
  `SimpleFindK` to match target coverage.
- `rake_poly_function.R`: Population-weighted aggregation of raked
  predictions to admin units (parallel enabled).
- `rakeProcess.R` / `rakeProcess1.R`: Applies raking to HEP3 and HEP1
  yearly predictions and generates summarized outputs.

------------------------------------------------------------------------

### 2. `estimations/` folder

This folder contains the supplementary output datasets:

- **Supplementary Dataset 1: `hep_estimations.xlsx`**  
  Contains ADM0(Nation), ADM1(Province/State), and ADM2(City/County)-level estimates of HEP1 and HEP3
  coverage for 2010–2022.

- **Supplementary Dataset 2: `dropout_estimations.xlsx`**  
  Contains dropout rate estimates absolute dropout rate (HEP1-HEP3) and relative dropout rate(HEP1-HEP3)/HEP1 at national, ADM1, and
  ADM2 levels from 2010–2022.

- **Supplementary Dataset 3: `predict_to_2030.xlsx`**  
  Contains national, ADM1, and ADM2-level predictions of HEP3 coverage
  for the year 2030.

------------------------------------------------------------------------

## Citation

Please cite appropriately if you use or refer to this project. For any
questions or collaboration interests, feel free to open an issue or
contact the maintainer/writer.
