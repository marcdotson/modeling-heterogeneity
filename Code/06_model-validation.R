# Load Packages -----------------------------------------------------------
library(tidyverse)
# library(bayesplot)
# library(tidybayes)
library(bayesm)
library(mvtnorm)
# library(ggridges)


# Load final data.
final_data <- read_csv(here::here("Data", "218329_Final_Excel_050619_with_recontact.csv")) %>% 
  filter(REC_Q1 == 1)

# - Filter based on screening.
# - Append row number ID.
# - Select ID and ownership data, filter.
# - Select ID and purchase data, filter.
# - Investigate amount of overlap.
# - Combine data base on brand and year.
# - Look at spelling corrections as needed.
# - Recode into a truncated validation task with an outside good.
# - Append IDs to the original data and hold-out.
# - Draw the subset of respondents and the subset of parameters in computing predictive fit.

# c("brand", "year", "miles", "warranty", "seller", "mpg", "safety", "price")
# Q2_1 MAKE, Q2_3 YEAR





intercept <- 1 # Intercept-only.
geo_locat <- 0 # Geolocation covariates.
demo_vars <- 0 # Demographic covariates.
geo_demos <- 0 # Geolocation and demographic covariates.
bnd_demos <- 0 # Brand covariates.
all_three <- 0 # Geolocation, demographic, and brand covariates.

# Load model output.
if (intercept == 1) run <- read_rds(here::here("Output", "hmnl_intercept-100k_ho.RDS"))
if (geo_locat == 1) run <- read_rds(here::here("Output", "hmnl_more-geo-locat-100k_ho.RDS"))
if (demo_vars == 1) run <- read_rds(here::here("Output", "hmnl_demo-vars-100k_ho.RDS"))
if (geo_demos == 1) run <- read_rds(here::here("Output", "hmnl_more-geo-demos-100k_ho.RDS"))
if (bnd_demos == 1) run <- read_rds(here::here("Output", "hmnl_bnd-demos-100k_ho.RDS"))
if (all_three == 1) run <- read_rds(here::here("Output", "hmnl_all-three-100k_ho.RDS"))

# Extract Data, Prior, Mcmc, and fit objects.
Data <- run$Data
Prior <- run$Prior
Mcmc <- run$Mcmc
fit <- run$fit

