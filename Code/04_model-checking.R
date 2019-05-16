# Load model output.
fit_centered <- read_rds(here::here("Output", "hmnl-centered_fit.RDS"))
fit_noncentered <- read_rds(here::here("Output", "hmnl-noncentered_fit.RDS"))
fit_conjugate <- read_rds(here::here("Output", "hmnl-conjugate-20k_fit.RDS"))
colnames(fit_conjugate$Gammadraw) <- 
  c(
    "Theta[1,1]", "Theta[2,1]", "Theta[3,1]", "Theta[4,1]", "Theta[5,1]", "Theta[6,1]",
    "Theta[7,1]", "Theta[8,1]", "Theta[9,1]", "Theta[10,1]", "Theta[11,1]", "Theta[12,1]"
  )

# Diagnostics.
library(bayesplot)
library(tidybayes)
library(ggridges)
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
fit_centered %>%
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

fit_conjugate$Gammadraw %>% 
  mcmc_trace(
    n_warmup = 500,
    facet_args = list(nrow = 2, labeller = label_parsed)
  )

ggsave(
  "mcmc_trace_conjugate.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)

# Recover parameter values.
Theta <- tibble(i = as.factor(1:ncol(sim_data$Theta)), Theta = t(sim_data$Theta))

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

draws_conjugate <- as_tibble(fit_conjugate$Gammadraw) %>% 
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
  ) +
  geom_vline(aes(xintercept = Theta), Theta, color = "red")

ggsave(
  "mcmc_marginal_posteriors.png",
  path = here::here("Figures"),
  width = 12, height = 6, units = "in"
)
