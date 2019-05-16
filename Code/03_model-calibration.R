# Load packages.
library(tidyverse)
library(rstan)

# Set Stan options.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load data and design.
final_data <- read_csv(here::here("Data", "218329_Final_Excel_050619.csv"))
dummy_design <- read_csv(here::here("Data", "dummy_design.csv")) %>% select(-X1)

# Restructure choice data Y.
Y <- final_data %>% 
  select(record, contains("Q3")) %>% 
  select(-c(record, Q3_Version)) %>% 
  as.matrix()

# Restructure design data X.
X <- array(
  data = NA, 
  dim = c(
    nrow(Y),                      # Number of respondents.
    max(dummy_design$task),       # Number of choice tasks per respondent.
    # (max(dummy_design$alt) + 1),  # Number of product alternatives per choice task.
    # (ncol(dummy_design) - 3 + 1) # Number of (estimable) attribute levels.
    max(dummy_design$alt),  # Number of product alternatives per choice task.
    (ncol(dummy_design) - 3) # Number of (estimable) attribute levels.
  )
)
# Add the outside option coded as all zeros.
for (n in 1:dim(X)[1]) {
  # Filter for respondent n.
  X_n <- dummy_design %>% filter(version == final_data$Q3_Version[n])
  for (s in 1:dim(X)[2]) {
    # Filter for task s.
    X_s <- X_n %>%
      filter(task == s) %>%
      # mutate(brand1 = 0) %>%
      # select(brand1, brand2:price17)
      select(brand2:price17)
    # for (p in 1:(dim(X)[3]-1)) {
    #   # Filter for task s and alt p brands.
    #   X_p_brand <- X_n %>%
    #     filter(task == s, alt == p) %>%
    #     select(brand2:brand16) %>%
    #     as_vector()
    #   # Fill in brand1
    #   X_s[p, "brand1"] <- ifelse(sum(X_p_brand) == 1, 0, 1)
    # }
    # Save modified design, including outside option.
    # X[n, s,,] <- rbind(X_s, rep(0, ncol(X_s))) %>% as.matrix()
    X[n, s,,] <- X_s %>% as.matrix()
  }
}
# # Add the outside option coded as dummy variable.
# for (n in 1:dim(X)[1]) {
#   # Filter for respondent n.
#   X_n <- dummy_design %>% filter(version == final_data$Q3_Version[n])
#   for (s in 1:dim(X)[2]) {
#     # Filter for task s.
#     X_s <- X_n %>% 
#       filter(task == s) %>% 
#       # mutate(brand1 = 0) %>% 
#       # select(brand1, brand2:price17)
#       mutate(outside = 0) %>% 
#       select(outside, brand2:price17)
#     # for (p in 1:(dim(X)[3]-1)) {
#     #   # Filter for task s and alt p brands.
#     #   X_p_brand <- X_n %>% 
#     #     filter(task == s, alt == p) %>% 
#     #     select(brand2:brand16) %>% 
#     #     as_vector()
#     #   # Fill in brand1
#     #   X_s[p, "brand1"] <- ifelse(sum(X_p_brand) == 1, 0, 1)
#     # }
#     # Save modified design, including outside option.
#     X[n, s,,] <- rbind(X_s, c(1, rep(0, ncol(X_s)-1))) %>% as.matrix()
#   }
# }

# Restructure covariates Z.
Z <- matrix(data = 1, nrow = dim(X)[1], ncol = 1)

# Specify the data for calibration in a list.
data <- list(
  N = dim(X)[1],           # Number of respondents.
  S = dim(X)[2],           # Number of choice tasks per respondent.
  P = dim(X)[3],           # Number of product alternatives per choice task.
  L = dim(X)[4],           # Number of (estimable) attribute levels.
  C = ncol(Z),             # Number of respondent-level covariates.
  
  Theta_mean = 0,          # Mean of coefficients for the heterogeneity model.
  Theta_scale = 10,        # Scale of coefficients for the heterogeneity model.
  tau_mean = 0,            # Mean of scale parameters for the heterogeneity model.
  tau_scale = 2.5,         # Scale of scale parameters for the heterogeneity model.
  Omega_shape = 2,         # Shape of correlation matrix for the heterogeneity model.
  
  Y = Y,                   # Matrix of observed choices.
  X = X,                   # Array of experimental designs per choice task.
  Z = Z                    # Matrix of respondent-level covariates.
)

# test <- diag(data$tau_scale, data$L) %*% 
#   rethinking::rlkjcorr(1, K = data$L, eta = data$Omega_shape) %*% 
#   # matrix(data = data$Omega_shape, nrow = data$L, ncol = data$L) %*% 
#   diag(data$tau_scale, data$L)
# 
# matrixcalc::is.positive.definite(test)

# Calibrate the model.
fit <- stan(
  file = here::here("Code", "hmnl_centered.stan"),
  data = data,
  seed = 42
)

# Save model output.
write_rds(fit, here::here("Output", "hmnl_intercept.RDS"))

