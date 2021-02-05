# Load packages.
library(tidyverse)

# Load data, including validation data.
data <- read_csv(here::here("Data", "218329_Final_Excel_050619_with_recontact.csv"))

data %>% 
  select(Acura:Volkswagen) %>% 
  pivot_longer(Acura:Volkswagen, names_to = "dealership", values_to = "visits") %>% 
  summarize(
    max_visits = max(visits),
    min_visits = min(visits)
  )

data %>% 
  select(Acura:Volkswagen) %>% 
  pivot_longer(Acura:Volkswagen, names_to = "dealership", values_to = "visits") %>% 
  filter(visits < 20) %>% 
  ggplot(aes(x = visits)) +
  geom_bar()

# Explore Recontact Data
rec_data <- data %>% 
  select(contains("REC_Q"))

