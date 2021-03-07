# Preamble ----------------------------------------------------------------
# Load packages and functions.
library(tidyverse)
source(here::here("Code", "Source", "model_fit.R"))

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

# Investigate overlap between ownership and recontact data.
overlap <- validate_ownership %>% 
  inner_join(validate_recontact)

overlap

# There are only 18 respondents who overlap between recontact and ownership.
# In 17/18 cases (see record 239) the recontact survey indicates a newer purchase
# than the ownership data, so we will use the overlap from recontact instead.

# Investigate ownership data for more than one car.
validate_ownership %>% 
  group_by(record) %>% 
  count(record) %>% 
  ggplot(aes(x = n)) +
  geom_bar()

# Most respondents in the ownership data have purchased one or two cars. We
# don't have any reason to give special credence to any one of the vehicles,
# so let's just stick with the first of the vehicles listed.

# Remove respondents from ownership data that overlap in the recontact data,
# keeping just the first car listed in the ownership data.
validate_ownership <- validate_ownership %>% 
  filter(vehicle == 1) %>% 
  anti_join(overlap, by = "record") %>% 
  select(record, contains("ownership")) %>% 
  rename(
    make = ownership_make,
    model = ownership_model,
    year = ownership_year
  )

# Combine recontact and ownership data into validate_data.
validate_data <- bind_rows(
  validate_recontact %>% 
    select(record, contains("recontact")) %>% 
    rename(
      make = recontact_make,
      model = recontact_model,
      year = recontact_year
    ),
  validate_ownership
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

# Just 16 hold-out respondents in the validation data vs. 156 respondents in-sample.




# - Recode into a truncated validation task with an outside good.
# - Draw the subset of respondents and the subset of parameters in computing predictive fit.

# c("brand", "year", "miles", "warranty", "seller", "mpg", "safety", "price")
# Q2_1 MAKE, Q2_3 YEAR


# Create validation choice data Y and Y_ho.
Y <- rep(1, nrow(validate_data_in_sample)) %>%
  as.matrix()
Y_ho <- rep(1, nrow(validate_data_ho)) %>%
  as.matrix()

# Create the design X and X_ho.
X <- array(
  data = NA,
  dim = c(
    nrow(Y),  # Number of respondents.
    1,        # Number of choice tasks per respondent.
    (1 + 1),  # Number of product alternatives per choice task.
    (11)      # Number of (estimable) attribute levels.
  )
)
X_ho <- array(
  data = NA,
  dim = c(
    nrow(Y_ho),  # Number of respondents.
    1,        # Number of choice tasks per respondent.
    (1 + 1),  # Number of product alternatives per choice task.
    (11)      # Number of (estimable) attribute levels.
  )
)

# Recode the validation_data into X and X_ho and add the outside option coded as all zeros.
for (n in 1:dim(X)[1]) {
  # Filter for respondent n.
  X_n <- design %>% filter(version == final_data$Q3_Version[n])
  for (s in 1:dim(X)[2]) {
    # Filter for task s.
    X_s <- X_n %>%
      filter(task == s) %>%
      mutate(brand1 = 0) %>%
      select(brand1, brand2:price)
    for (p in 1:(dim(X)[3]-1)) {
      # Filter for task s and alt p brands.
      X_p <- X_n %>%
        filter(task == s, alt == p) %>%
        select(brand2:brand16) %>%
        as_vector()
      # Fill in brand1
      X_s[p, "brand1"] <- ifelse(sum(X_p) == 1, 0, 1)
    }
    # Save modified design, including outside option.
    X[n, s,,] <- rbind(X_s, rep(0, ncol(X_s))) %>% as.matrix()
  }
}

# Restructure covariates Z.
if (intercept == 1) Z <- matrix(data = 1, nrow = dim(X)[1], ncol = 1)
if (geo_locat == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>% 
    bind_cols(
      final_data %>% 
        select(Acura:Volkswagen)
      # mutate(
      #   dealer_visit = Acura	+ BMW	+ Chevrolet + Chrysler + Ferrari + `Ford Motor Company` +
      #     `GMC (General Motors Company)` + Honda + `Hyundai Motor` + Infiniti	+ `Kia Motors` +
      #     Lexus	+ Lincoln	+ Mazda	+ `Mercedes Benz`	+ `Mitsubishi Motors`	+ `Nissan North America` +
      #     Subaru + `Tesla Motors`	+ Toyota + Volkswagen
      # ) %>%
      # select(dealer_visit) %>%
      # mutate(dealer_visit = ifelse(dealer_visit >= 1, 1, 0))
    ) %>% 
    as.matrix()
  Z <- ifelse(Z > 5, 1, Z)
}
if (demo_vars == 1) {
  Z <- tibble(intercept = rep(1, dim(X)[1])) %>% 
    bind_cols(
      final_data %>% 
        select(Q4x1, Q4x4:Q4x6, Q4x9, Q4x10, Q4x12r1:Q4x12r4)
    ) %>% 
    as.matrix()
}
if (geo_demos == 1) {
  Z_geo <- tibble(intercept = rep(1, dim(X)[1])) %>% 
    bind_cols(
      final_data %>% 
        select(Acura:Volkswagen)
      # mutate(
      #   dealer_visit = Acura	+ BMW	+ Chevrolet + Chrysler + Ferrari + `Ford Motor Company` +
      #     `GMC (General Motors Company)` + Honda + `Hyundai Motor` + Infiniti	+ `Kia Motors` +
      #     Lexus	+ Lincoln	+ Mazda	+ `Mercedes Benz`	+ `Mitsubishi Motors`	+ `Nissan North America` +
      #     Subaru + `Tesla Motors`	+ Toyota + Volkswagen
      # ) %>%
      # select(dealer_visit) %>%
      # mutate(dealer_visit = ifelse(dealer_visit >= 1, 1, 0))
    ) %>% 
    as.matrix()
  Z_geo <- ifelse(Z_geo > 5, 1, Z_geo)
  Z_demo <- final_data %>% 
    select(Q4x1, Q4x4:Q4x6, Q4x9, Q4x10, Q4x12r1:Q4x12r4) %>% 
    as.matrix()
  Z <- cbind(Z_geo, Z_demo)
}
if (bnd_demos == 1) {
  Z_bnd <- final_data %>% 
    select(contains("Q1x2"), contains("Q2x3"), Q2x1, Q2x2, Q2x8) %>%
    as.matrix()
  Z_demo <- final_data %>% 
    select(Q4x1, Q4x4:Q4x6, Q4x9, Q4x10, Q4x12r1:Q4x12r4) %>% 
    as.matrix()
  Z <- cbind(Z_bnd, Z_demo)
}
if (all_three == 1) {
  Z_geo <- tibble(intercept = rep(1, dim(X)[1])) %>% 
    bind_cols(
      final_data %>% 
        select(Acura:Volkswagen)
      # mutate(
      #   dealer_visit = Acura	+ BMW	+ Chevrolet + Chrysler + Ferrari + `Ford Motor Company` +
      #     `GMC (General Motors Company)` + Honda + `Hyundai Motor` + Infiniti	+ `Kia Motors` +
      #     Lexus	+ Lincoln	+ Mazda	+ `Mercedes Benz`	+ `Mitsubishi Motors`	+ `Nissan North America` +
      #     Subaru + `Tesla Motors`	+ Toyota + Volkswagen
      # ) %>%
      # select(dealer_visit) %>%
      # mutate(dealer_visit = ifelse(dealer_visit >= 1, 1, 0))
    ) %>% 
    as.matrix()
  Z_geo <- ifelse(Z_geo > 5, 1, Z_geo)
  Z_demo <- final_data %>% 
    select(Q4x1, Q4x4:Q4x6, Q4x9, Q4x10, Q4x12r1:Q4x12r4) %>% 
    as.matrix()
  Z_bnd <- final_data %>% 
    select(contains("Q1x2"), contains("Q2x3"), Q2x1, Q2x2, Q2x8) %>%
    as.matrix()
  Z <- cbind(Z_geo, Z_demo, Z_bnd)
}



# Compute Validation Fit --------------------------------------------------
# # Create model validation table.
# model_validation_table <- tibble(
#   model = rep(NA, 6),
#   train_hr = rep(NA, 6),
#   train_hp = rep(NA, 6),
#   test_hr = rep(NA, 6),
#   test_hp = rep(NA, 6)
# )
# write_rds(model_validation_table, here::here("Figures", "model_validation_table.rds"))

# Load model validation table.
model_validation_table <- read_rds(here::here("Figures", "model_validation_table.rds"))

temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)





if (intercept == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Intercept 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (geo_locat == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Geolocation 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (demo_vars == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Demographics 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (geo_demos == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "More Geo-Demos 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (bnd_demos == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Brands 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}
if (all_three == 1) {
  temp <- model_fit(fit = fit, n_warmup = 500, Data = Data)
  model_fit_table <- model_fit_table %>% 
    bind_rows(
      tibble(
        model = "Geolocation, Brands, and Demographics 100k w/HO",
        lmd = temp[1],
        dic = temp[2],
        waic = temp[3],
        hr = temp[4],
        hp = temp[5]
      )
    )
}

write_rds(model_fit_table, here::here("Figures", "model_fit_table.RDS"))

