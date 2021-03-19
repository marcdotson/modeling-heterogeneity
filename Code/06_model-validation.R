# Preamble ----------------------------------------------------------------
# Load packages and functions.
library(tidyverse)
library(bayesm)
library(mvtnorm)
source(here::here("Code", "Source", "validation_fit.R"))

# Transform and Combine Recontact and Ownership Data ----------------------
# Extract make, model, and year from the recontact survey.
validate_recontact <- read_csv(here::here("Data", "218329_Final_Excel_050619_with_recontact.csv")) %>% 
  filter(REC_Q1 == 1) %>% 
  mutate(vehicle = 1) %>% 
  select(record, vehicle, REC_Q2_1, REC_Q2_2, REC_Q2_3) %>% 
  rename(
    recontact_make = REC_Q2_1,
    recontact_model = REC_Q2_2,
    recontact_year = REC_Q2_3
  )

# Extract make, model, and year from the ownership data.
validate_ownership <- read_csv(here::here("Data", "218329_Final_Excel_050619_with_recontact.csv")) %>% 
  select(record, `Vehicle 1 Make`:`Vehicle 8 Year`) %>% 
  mutate(across(contains("Year"), as.character)) %>% 
  pivot_longer(
    `Vehicle 1 Make`:`Vehicle 8 Year`,
    names_to = "vehicle_temp",
    values_to = "vehicle_info"
  ) %>% 
  separate(
    vehicle_temp,
    into = c("temp", "vehicle", "info"),
    sep = " "
  ) %>% 
  drop_na(vehicle_info) %>% 
  select(-temp) %>% 
  pivot_wider(
    names_from = info,
    values_from = vehicle_info
  ) %>% 
  rename(
    ownership_make = Make,
    ownership_model = Model,
    ownership_year = Year
  ) %>% 
  mutate(vehicle = as.numeric(vehicle))

# # Investigate overlap between ownership and recontact data.
# overlap <- validate_ownership %>% 
#   inner_join(validate_recontact)
# 
# overlap
# 
# # There are only 18 respondents who overlap between recontact and ownership.
# # In 17/18 cases (see record 239) the recontact survey indicates a newer purchase
# # than the ownership data, so we will use the overlap from recontact instead.
# 
# # Investigate ownership data for more than one car.
# validate_ownership %>% 
#   group_by(record) %>% 
#   count(vehicle) %>%
#   ungroup() %>% 
#   count(vehicle)
#   
# # Most respondents in the ownership data have purchased one or two cars. We
# # don't have any reason to give special credence to any one of the vehicles,
# # so let's just stick with the first of the vehicles listed.
# 
# # Remove respondents from ownership data that overlap in the recontact data,
# # keeping just the first car listed in the ownership data.
# validate_ownership <- validate_ownership %>%
#   filter(vehicle == 1) %>%
#   # anti_join(overlap, by = "record") %>% # Or just use the ownership data ONLY.
#   select(record, contains("ownership")) %>%
#   rename(
#     make = ownership_make,
#     model = ownership_model,
#     year = ownership_year
#   )
# 
# # Remove respondents from recontact data that overlap in the ownership data.
# # Combine recontact and ownership data into validate_data.
# validate_data <- bind_rows(
#   validate_recontact %>% # Or just use the ownership data ONLY.
#     anti_join(overlap, by = "record") %>% 
#     select(record, contains("recontact")) %>%
#     rename(
#       make = recontact_make,
#       model = recontact_model,
#       year = recontact_year
#     ),
#   validate_ownership # Or just use the recontact data ONLY.
# )

# Combine all recontact and all ownership data.
validate_data <- bind_rows(
  validate_recontact %>%
    select(record, contains("recontact")) %>%
    rename(
      make = recontact_make,
      model = recontact_model,
      year = recontact_year
    ),
  validate_ownership %>%
    select(record, contains("ownership")) %>%
    rename(
      make = ownership_make,
      model = ownership_model,
      year = ownership_year
    )
)

# Clean up validate_data to provide a brand and year consistent with the conjoint.
# Filter based on brand and recode year to match the attribute levels.
conjoint_brands = c(
  "jeep", "toyota", "ford", "chevrolet", "honda", "nissan", "subaru", "hyundai", 
  "gmc", "kia", "lexus", "mazda", "buick", "mercedes-benz", "volkswagen", "bmw"
)

validate_data <- validate_data %>% 
  mutate(
    make = str_replace(make, pattern = "Jepp", replacement = "Jeep"),
    make = str_replace(make, pattern = "Chevy", replacement = "Chevrolet"),
    make = str_replace(make, pattern = "2018", replacement =  "Mazda"),
    make = str_to_lower(make),
    year = str_replace(year, pattern = "Cx -9", replacement =  "2018")
  ) %>% 
  filter(make %in% conjoint_brands) %>% 
  mutate(
    year = case_when(
      year >= 2019 ~ "2019 or newer",
      year >= 2016 & year <= 2018 ~ "2016-2018",
      year >= 2013 & year <= 2015 ~ "2013-2015",
      year >= 2010 & year <= 2012 ~ "2010-2012",
      year >= 2007 & year <= 2009 ~ "2007-2009",
      year < 2007 ~ "Older than 2007"
    )
  ) %>% 
  select(-model)

# Load Output and Restructure Data ----------------------------------------
intercept <- 0 # Intercept-only.
geo_locat <- 0 # Geolocation covariates.
demo_vars <- 0 # Demographic covariates.
geo_demos <- 0 # Geolocation and demographic covariates.
bnd_demos <- 0 # Brand covariates.
all_three <- 1 # Geolocation, demographic, and brand covariates.

# Load model output.
if (intercept == 1) run <- read_rds(here::here("Output", "hmnl_intercept-100k_ho.RDS"))
if (geo_locat == 1) run <- read_rds(here::here("Output", "hmnl_more-geo-locat-100k_ho.RDS"))
if (demo_vars == 1) run <- read_rds(here::here("Output", "hmnl_demo-vars-100k_ho.RDS"))
if (geo_demos == 1) run <- read_rds(here::here("Output", "hmnl_more-geo-demos-100k_ho.RDS"))
if (bnd_demos == 1) run <- read_rds(here::here("Output", "hmnl_bnd-demos-100k_ho.RDS"))
if (all_three == 1) run <- read_rds(here::here("Output", "hmnl_all-three-100k_ho.RDS"))

# Model names.
if (geo_locat == 1) model_name <- "Geolocation"
if (demo_vars == 1) model_name <- "Demographics"
if (geo_demos == 1) model_name <- "Geolocation and Demographics"
if (bnd_demos == 1) model_name <- "Brands"
if (all_three == 1) model_name <- "Geolocation, Brands, and Demographics"

# Extract Data, Prior, Mcmc, and fit objects.
Data <- run$Data
Prior <- run$Prior
Mcmc <- run$Mcmc
fit <- run$fit

# Use Data$ho_ind to recover record (IDs).
final_record <- read_csv(here::here("Data", "218329_Final_Excel_050619.csv")) %>% 
  select(record) %>% 
  bind_cols(tibble(ho_ind = as.vector(Data$ho_ind)))

in_sample_record <- final_record %>% 
  filter(ho_ind == 0) %>% 
  select(record) %>% 
  pull()

ho_record <- final_record %>% 
  filter(ho_ind == 1) %>% 
  select(record) %>% 
  pull()

# Investigate how many in-sample and hold-out respondents we have in validate_data.
validate_data_in_sample <- validate_data %>% 
  filter(record %in% in_sample_record)

validate_data_ho <- validate_data %>% 
  filter(record %in% ho_record)

validate_data_in_sample
validate_data_ho

# ONE VEHICLE
# Just 16 hold-out respondents in the validation data vs. 156 respondents in-sample.
# While just 4 hold-out respondents in recontact-only validation data vs. 40 in-sample.
# While just 12 hold-out respondents in ownership-only validation data vs. 128 in-sample (no removing overlap).

# ALL VEHICLES
# We have 36 hold-out observations and 302 in-sample observations (repeat respondents and no removing overlap).

# Create a validation task with all possible combinations of brands and years.
brands <- c(
  "jeep", "toyota", "ford", "chevrolet", "honda", "nissan", "subaru", "hyundai", 
  "gmc", "kia", "lexus", "mazda", "buick", "mercedes-benz", "volkswagen", "bmw"
)
years <- c(
  "2019 or newer", "2016-2018", "2013-2015", "2010-2012", "2007-2009", "Older than 2007"
)
temp <- NULL
for (i in 1:length(years)) temp <- c(temp, rep(years[i], length(brands)))
X_named <- tibble(
  brands = rep(brands, length(years)),
  years = temp
)
X_full <- bind_cols(
  # Account for the first level for the brand intercept.
  tibble(brands1 = rep(c(1, rep(0, length(brands) - 1)), length(years))),
  # All possible combinations of brands and years.
  as_tibble(
    model.matrix(
      ~ brands + years, 
      data.frame(
        brands = gl(length(brands), 1, length(brands) * length(years)), 
        years = gl(length(years), length(brands))
      )
    )[,-1]
  )
)

# Populate the design matrices X and X_ho and choice data Y and Y_ho.
X <- array(
  data = NA,
  dim = c(
    nrow(validate_data_in_sample), # Number of respondents.
    1,                             # Number of choice tasks per respondent.
    (nrow(X_full) + 1),            # Number of alternatives per choice task.
    ncol(X_full)                   # Number of (estimable) attribute levels.
  )
)
Y <- rep(NA, nrow(validate_data_in_sample)) %>% as.matrix()
X_ho <- array(
  data = NA,
  dim = c(
    nrow(validate_data_ho),        # Number of respondents.
    1,                             # Number of choice tasks per respondent.
    (nrow(X_full) + 1),            # Number of product alternatives per choice task.
    ncol(X_full)                   # Number of (estimable) attribute levels.
  )
)
Y_ho <- rep(NA, nrow(validate_data_ho)) %>% as.matrix()
for (n in 1:nrow(Y)) {
  # Identify the choice.
  Y[n] <- which(
    pull(validate_data_in_sample[n,2]) == X_named[,1] & 
      pull(validate_data_in_sample[n,3]) == X_named[,2]
  )
  # Add an outside option.
  X[n,1,,] <- as.matrix(rbind(X_full, rep(0, dim(X)[4])))
}
for (n in 1:nrow(Y_ho)) {
  # Identify the choice.
  Y_ho[n] <- which(
    pull(validate_data_ho[n,2]) == X_named[,1] & 
      pull(validate_data_ho[n,3]) == X_named[,2]
  )
  # Add an outside option.
  X_ho[n,1,,] <- as.matrix(rbind(X_full, rep(0, dim(X_ho)[4])))
}

# # Create validation choice data Y and Y_ho.
# Y <- rep(1, nrow(validate_data_in_sample)) %>%
#   as.matrix()
# Y_ho <- rep(1, nrow(validate_data_ho)) %>%
#   as.matrix()
# 
# # Create the design X and X_ho.
# X <- array(
#   data = NA,
#   dim = c(
#     nrow(Y),    # Number of respondents.
#     1,          # Number of choice tasks per respondent.
#     (1 + 1),    # Number of product alternatives per choice task.
#     (16 + 5)    # Number of (estimable) attribute levels.
#   )
# )
# X_ho <- array(
#   data = NA,
#   dim = c(
#     nrow(Y_ho), # Number of respondents.
#     1,          # Number of choice tasks per respondent.
#     (1 + 1),    # Number of product alternatives per choice task.
#     (16 + 5)    # Number of (estimable) attribute levels.
#   )
# )
# 
# # Specify the brand intercept levels and year levels.
# brands <- c(
#   # Brand intercept, so no baseline level.
#   "jeep", "toyota", "ford", "chevrolet", "honda", "nissan", "subaru", "hyundai", 
#   "gmc", "kia", "lexus", "mazda", "buick", "mercedes-benz", "volkswagen", "bmw"
# )
# years <- c(
#   # "2019 or newer" baseline level
#   "2016-2018", "2013-2015", "2010-2012", "2007-2009", "Older than 2007"
# )
# 
# # Recode the validation_data into X and X_ho and add the outside option coded as all zeros.
# for (n in 1:dim(X)[1]) {
#   # Construct the two-attribute validation task.
#   for (brand in 1:16) {
#     X[n,,1,brand] <- ifelse(validate_data_in_sample[n,]$make == brands[brand], 1, 0)
#   }
#   for (year in 1:5) {
#     X[n,,1,year + 16] <- ifelse(validate_data_in_sample[n,]$year == years[year], 1, 0)
#   }
#   # Include an outside option.
#   X[n,,2,] <- rep(0, dim(X)[4])
# }
# for (n in 1:dim(X_ho)[1]) {
#   # Construct the two-attribute validation task.
#   for (brand in 1:16) {
#     X_ho[n,,1,brand] <- ifelse(validate_data_ho[n,]$make == brands[brand], 1, 0)
#   }
#   for (year in 1:5) {
#     X_ho[n,,1,year + 16] <- ifelse(validate_data_ho[n,]$year == years[year], 1, 0)
#   }
#   # Include an outside option.
#   X_ho[n,,2,] <- rep(0, dim(X_ho)[4])
# }

# Use record to get the appropriate covariates Z and Z_ho (in the right order).
Z <- validate_data_in_sample %>% 
  select(record) %>% 
  left_join(
    bind_cols(
      tibble(record = in_sample_record),
      as_tibble(Data$Z)
    )
  ) %>% 
  select(-record) %>% 
  as.matrix()
Z_ho <- validate_data_ho %>% 
  select(record) %>% 
  left_join(
    bind_cols(
      tibble(record = ho_record),
      as_tibble(Data$Z_ho)
    )
  ) %>% 
  select(-record) %>% 
  as.matrix()

# Use record to get the appropriate covariates betadraws (in the right order).
betadraw <- array(
  data = NA,
  dim = c(
    nrow(Y),    # Number of respondents.
    dim(X)[4],  # Number of (estimable) attribute levels.
    1000        # Number of draws.
  )
)
for (n in 1:dim(betadraw)[1]) {
  # Identify which betadraws to extract.
  validate_record_temp <- validate_data_in_sample %>% 
    select(record) %>% 
    pull() %>% 
    .[n]
  record_temp <- which(in_sample_record == validate_record_temp)
  # Extract.
  betadraw[n,,] <- fit$betadraw[record_temp,1:dim(betadraw)[2],]
}

# Create list-form validation Data and fit for computing the validation fit.
Y_new <- vector(mode = "list", length = nrow(Y))
Y_new_ho <- vector(mode = "list", length = nrow(Y_ho))
X_new <- vector(mode = "list", length = nrow(Y))
X_new_ho <- vector(mode = "list", length = nrow(Y_ho))
for (resp in 1:nrow(Y)) {
  Y_new[[resp]] <- matrix(Y[resp, ])
  X_new[[resp]] <- rbind(X_new[[resp]], X[resp,1,,])
}
for (resp in 1:nrow(Y_ho)) {
  Y_new_ho[[resp]] <- matrix(Y_ho[resp, ])
  X_new_ho[[resp]] <- rbind(X_new_ho[[resp]], X_ho[resp,1,,])
}

Data_validate <- list(
  y = Y_new,
  X = X_new,
  Z = Z,
  y_ho = Y_new_ho,
  X_ho = X_new_ho,
  Z_ho = Z_ho
)
fit_validate <- list(
  betadraw = betadraw,
  llikedraw = fit$llikedraw,
  Gammadraw = fit$Gammadraw[,1:(21 * ncol(Z))],
  # Vbetadraw = fit$Vbetadraw[,1:(21 * 21)] Not symmetric without using the entire matrix.
  Vbetadraw = matrix(rep(as.vector(diag(21)), 1000), nrow = 1000, byrow = TRUE)
)

# Compute Validation Fit --------------------------------------------------
# Load model validation table.
if (intercept != 1) model_validation_table <- read_rds(here::here("Figures", "model_validation_table_final.rds"))

if (intercept == 1) {
  temp <- validation_fit(fit = fit_validate, n_warmup = 500, Data = Data_validate)
  model_validation_table <- tibble(
    model = "Intercept 100k w/HO and All-Vehicle, All-Combos Validation",
    train_hr = temp[1],
    train_hp = temp[2],
    test_hr = temp[3],
    test_hp = temp[4]
  )
}
if (intercept != 1) {
  temp <- validation_fit(fit = fit_validate, n_warmup = 500, Data = Data_validate)
  model_validation_table <- model_validation_table %>% 
    bind_rows(
      tibble(
        model = str_c(model_name, " 100k w/HO and All-Vehicle, All-Combos Validation"),
        train_hr = temp[1],
        train_hp = temp[2],
        test_hr = temp[3],
        test_hp = temp[4]
      )
    )
}

model_validation_table

# Write the model validation table.
write_rds(model_validation_table, here::here("Figures", "model_validation_table_final.rds"))

