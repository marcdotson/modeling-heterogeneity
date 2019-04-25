# Load packages.
library(tidyverse)

# Load data.
data <- read_csv(here::here("Data", "218329_Partial_Excel_042419.csv"))

# Screening checks.
data %>% 
  count(Q2x4) %>% 
  filter(Q2x4 > 0) %>% 
  summarize(current = sum(n))

data %>% 
  summarize(
    min_exp_price = min(Q2x8),
    max_exp_price = max(Q2x8), 
  )

# Conjoint checks.
data %>% 
  count(Q3_Version) %>% 
  arrange(desc(Q3_Version))

data %>% 
  select(Q3_Version:Q3_1)

data %>% 
  select(contains("Q3")) %>% 
  gather(key = task, value = choice, Q3_1:Q3_12) %>% 
  ggplot(aes(x = choice)) +
  geom_bar()

# Restructure choices.
data_choice <- data %>% 
  select(record, contains("Q3")) %>% 
  select(-Q3_Version) %>% 
  gather(key = task, value = choice, -record) %>% 
  separate(col = task, into = c("question", "task")) %>% 
  select(-question)

# Restructure design.
data_design <- data %>% 
  select(record, contains("FinalConcept")) %>% 
  gather(key = concept, value = level, -record) %>% 
  separate(col = concept, into = c("concept", "task")) %>% 
  separate(col = task, into = c("task", "alt"), sep = "r") %>% 
  separate(col = alt, into = c("att", "alt"), sep = "c") %>% 
  select(-concept)

# Join choice and design and plot.
data_choice %>% 
  left_join(data_design) %>% 
  filter(choice == alt) %>% 
  count(att, level) %>% 
  ggplot(aes(x = level, y = n)) +
  geom_col() +
  facet_wrap(~ att, nrow = 2, scales = "free_x") +
  ggtitle("Count of Attribute Levels in Chosen Alternatives")
  
# Demographics.
data %>% 
  count(Q3_12)

data %>% 
  count(Q4x1)

