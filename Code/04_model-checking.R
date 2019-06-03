# Load Model Output -------------------------------------------------------
# Load packages.
library(tidyverse)
library(bayesplot)
library(tidybayes)
library(bayesm)
library(ggridges)

# Indicate the model to check.
intercept <- 0
geo_locat <- 0
demo_vars <- 1
geo_demos <- 0

# Load model output.
if (intercept == 1) run <- read_rds(here::here("Output", "hmnl_intercept.RDS"))
if (geo_locat == 1) run <- read_rds(here::here("Output", "hmnl_geo-locat.RDS"))
if (demo_vars == 1) run <- read_rds(here::here("Output", "hmnl_demo-vars.RDS"))
if (geo_demos == 1) run <- read_rds(here::here("Output", "hmnl_geo-demos.RDS"))

# General MCMC ------------------------------------------------------------
# Extract Data, Prior, Mcmc, and fit objects.
Data <- run$Data
Prior <- run$Prior
Mcmc <- run$Mcmc
fit <- run$fit

# Check trace plots.
fit$llikedraw %>% 
  matrix(dimnames = list(NULL, "llike")) %>% 
  mcmc_trace(
    n_warmup = 500
  )

# colnames(fit$Gammadraw) <- str_c("Gamma[", c(1:ncol(fit$Gammadraw)), ",1]")
# fit$Gammadraw %>% 
#   mcmc_trace(
#     n_warmup = 500,
#     facet_args = list(nrow = 5, labeller = label_parsed)
#   )
# 
# ggsave(
#   "mcmc_trace_conjugate.png",
#   path = here::here("Figures"),
#   width = 12, height = 6, units = "in"
# )

# Import model fit table.
# model_fit_table <- matrix(NA, nrow = 6, ncol = 6) %>%
#   as_tibble() %>%
#   rename(
#     model = V1,
#     lmd = V2,
#     dic = V3,
#     waic = V4,
#     hr = V5,
#     hp = V6
#   )
# write_rds(model_fit_table, here::here("Figures", "model_fit_table.RDS"))
model_fit_table <- read_rds(here::here("Figures", "model_fit_table.RDS"))

# Compute in-sample model fit.
source(here::here("Code", "model_fit.R"))

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

write_rds(model_fit_table, here::here("Figures", "model_fit_table.RDS"))

model_fit_table %>% 
  mutate(lmd_abs = abs(lmd)) %>% 
  summarize(
    min_lmd = min(lmd_abs),
    min_dic = min(dic),
    min_waic = min(waic),
    max_hr = max(hr, na.rm = TRUE),
    max_hp = max(hp, na.rm = TRUE)
  )

# HMC-Specific ------------------------------------------------------------
# Stan diagnostics.
source(here::here("Code", "stan_utility.R"))

# Check for divergences (HMC-specific).
check_div(fit_centered)
check_div(fit_noncentered)

# Check the effective sample size. (Not working for a hierarchical model?)
check_n_eff(fit_centered)
check_n_eff(fit_noncentered)
check_n_eff(fit_conjugate)

# Check the Rhat statistic. (Issues with mixing? Need to run longer?)
check_rhat(fit_centered)
check_rhat(fit_noncentered)
check_rhat(fit_conjugate)

# Check trace plots.
fit %>%
  extract(
    inc_warmup = TRUE,
    permuted = FALSE
  ) %>%
  mcmc_trace(
    regex_pars = "Theta",
    n_warmup = 1000,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace_centered.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

fit_noncentered %>%
  extract(
    inc_warmup = TRUE,
    permuted = FALSE
  ) %>%
  mcmc_trace(
    regex_pars = "Theta",
    n_warmup = 1000,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace_noncentered.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

# Plot Marginals ----------------------------------------------------------
draws_centered <- fit_centered %>% 
  spread_draws(Theta[i, j]) %>% 
  mutate(model = "centered") %>% 
  select(model, .chain, .iteration, .draw, i, j, Theta) %>% 
  ungroup()

draws_noncentered <- fit_noncentered %>% 
  spread_draws(Theta[i, j]) %>% 
  mutate(model = "noncentered") %>% 
  select(model, .chain, .iteration, .draw, i, j, Theta) %>% 
  ungroup()

draws_intercept <- as_tibble(fit$Gammadraw) %>% 
  mutate(
    .draw = row_number(),
    .iteration = row_number()
  ) %>% 
  gather(key = i, value = Theta, -c(.draw, .iteration)) %>% 
  separate(col = i, into = c("temp1", "i"), sep = "\\[") %>% 
  separate(col = i, into = c("i", "j"), sep = ",") %>% 
  separate(col = j, into = c("j", "temp2"), sep = "\\]") %>% 
  mutate(
    model = "conjugate",
    .chain = as.integer(1),
    i = as.integer(i),
    j = as.integer(j)
  ) %>% 
  select(model, .chain, .iteration, .draw, i, j, Theta) %>% 
  arrange(.iteration) %>% 
  filter(.iteration > 500)

draws <- draws_centered %>% 
  bind_rows(draws_noncentered) %>% 
  bind_rows(draws_conjugate)

draws %>% 
  mutate(
    model = factor(model),
    model = fct_relevel(
      model, "noncentered", "centered", "conjugate"
    )
  ) %>%
  ggplot(aes(x = Theta, y = model)) + 
  geom_halfeyeh(.width = c(.95, .95)) +
  facet_wrap(
    ~ as.factor(i), 
    nrow = 3, 
    ncol = 4,
    scales = "free_x"
  )

ggsave(
  "mcmc_marginal_posteriors.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

