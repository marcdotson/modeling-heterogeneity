# Preamble ----------------------------------------------------------------
# Load packages.
library(tidyverse)

# Load data and design.
data <- read_csv(here::here("Data", "218329_Final_Excel_050619_with_recontact.csv"))
design <- read_csv(here::here("Data", "survey_design.csv"))

# Initial Survey ----------------------------------------------------------
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

# Confirm that the design being used is correct.
data %>% 
  select(record, Q3_Version, contains("FinalConcept")) %>% 
  gather(key = concept, value = level, -c(record, Q3_Version)) %>% 
  separate(col = concept, into = c("concept", "task")) %>% 
  separate(col = task, into = c("task", "alt"), sep = "r") %>% 
  separate(col = alt, into = c("att", "alt"), sep = "c") %>% 
  select(-c(record, concept)) %>% 
  spread(key = att, value = level) %>% 
  rename(version = Q3_Version) %>% 
  mutate(
    task = as.double(task),
    alt = as.double(alt)
  ) %>% 
  left_join(
    design %>% 
      select(-X1)
  ) %>% 
  mutate(
    diff_1 = `1` - brand,
    diff_2 = `2` - year,
    diff_3 = `3` - miles,
    diff_4 = `4` - warranty,
    diff_5 = `5` - seller,
    diff_6 = `6` - mpg,
    diff_7 = `7` - safety,
    diff_8 = `8` - price
  ) %>% 
  summarize(
    sum_diff_1 = sum(diff_1),
    sum_diff_2 = sum(diff_2),
    sum_diff_3 = sum(diff_3),
    sum_diff_4 = sum(diff_4),
    sum_diff_5 = sum(diff_5),
    sum_diff_6 = sum(diff_6),
    sum_diff_7 = sum(diff_7),
    sum_diff_8 = sum(diff_8)
  )

# Demographics.
data %>% 
  count(Q3_12)

data %>% 
  count(Q4x1)

# Ownership Data ----------------------------------------------------------
# after the data is cleaned, these columns won't be needed anymore
positions <- c(1:21, 24:33, 45, 455)

ownership_data <- data %>% 
  mutate(
    Intended_purchase = case_when(            #"Which of the following do you plan to purchase or lease in the next 6 months? "
      Q1x1r1 == 1 ~ "Vehicle", 
      Q1x1r2 == 1 ~ "Real Estate", 
      Q1x1r3 == 1 ~ "Lawn Equipment", 
      Q1x1r4 == 1 ~ "Kitchen Appliances", 
      Q1x1r5 == 1 ~ "None"
    )
  ) %>% 
  mutate(
    Considered_car = case_when(                #"Which of the following vehicles do you plan to purchase or lease within the next 6 months?"
      Q1x2r1 == 1 ~ 1,                        #1 Two-door coupe
      Q1x2r2 == 1 ~ 2,                        #2 Four-door sedan
      Q1x2r3 == 1 ~ 3,                        #3 Hatchback
      Q1x2r4 == 1 ~ 4,                        #4 Station wagon
      Q1x2r5 == 1 ~ 5,                        #5 Sports car
      Q1x2r6 == 1 ~ 6,                        #6 Minivan
      Q1x2r7 == 1 ~ 7,                        #7 Crossover or sport utility vehicle (SUV)
      Q1x2r8 == 1 ~ 8,                        #8 Truck
      Q1x2r9 == 1 ~ 9,                        #9 Van
      Q1x2r10 == 1 ~ 10                       #10 None of the above
    )
  ) %>% 
  mutate(
    Previous_vehicles = case_when(            #"Which of the following vehicle types have you owned?"
      Q2x3r1 == 1 ~ 1,                        #same number system as Considered_car
      Q2x3r2 == 1 ~ 2, 
      Q2x3r3 == 1 ~ 3, 
      Q2x3r4 == 1 ~ 4, 
      Q2x3r5 == 1 ~ 5, 
      Q2x3r6 == 1 ~ 6, 
      Q2x3r7 == 1 ~ 7, 
      Q2x3r8 == 1 ~ 8, 
      Q2x3r9 == 1 ~ 9, 
      Q2x3r10 == 1 ~ 10
    )
  ) %>% 
  rename(Purchase_likelihood = Q2x1,           #"Would you purchase or lease a new vehicle?" (on a scale of 1-4. 4 being the most likely)
         Used_vehicle_likelihood = Q2x2,       #"Would you purchase a used vehicle?" (same 1-4 scale)
         Num_vehicles = Q2x4,                  #"How many vehicles do you currently own or lease?"
         Purchase_to_replace = Q2x5,           #"Are you purchasing/leasing a new vehicle to replace a current vehicle?"
         Purchase_reason = Q2x6,               #"Why are you in the market for a vehicle in the next 6 months? Please be thorough and specific"
         First_choice_car = Q2x7r1,            #"What brands come to mind as something you would consider when purchasing or leasing your next vehicle?"
         Second_choice_car = Q2x7r2,          
         Third_choice_car = Q2x7r3, 
         Fourth_choice_car = Q2x7r4, 
         Fifth_choice_car = Q2x7r5, 
         Sixth_choice_car = Q2x7r6, 
         Seventh_choice_car = Q2x7r7, 
         Eighth_choice_car = Q2x7r8, 
         Anticipated_car_price = Q2x8
  ) %>% 
  mutate(
    Racial = case_when(                         #creating a new column for race that is easier to perform analysis on
      Q4x12r1 == 1 ~ "Asian", 
      Q4x12r2 == 1 ~ "African American", 
      Q4x12r3 == 1 ~ "Hispanic", 
      Q4x12r4 == 1 ~ "White", 
      Q4x12r5 == 1 ~ "Other"
    )
  ) %>% 
  rename(
    Gender = Q4x1,                          # 1 = Female, 2 = Male, 3 = other
    Year_born = Q4x2, 
    Income = Q4x4,                          # in thousands
    Marital_status = Q4x5,                   # 1 = single, 2 = married, 3 = divorced, 4 = separated
    Children = Q4x6,                        # 1 = yes, 2 = no
    Num_children = Q4x7, 
    Job = Q4x8, 
    Education = Q4x9,                       # 1 = high school, 2 = associate's, 3 = bachelor's, 4 = graduate's, 5 = none of the above
    Residence = Q4x10,                      # 1 = renter, 2 = owner
  ) %>% 
  select(-positions)

#there are a handful of outliers in the income data... Some reported making upwards of $10,000,000 a year. It could also be that they didn't enter in their salary as thousands
ggplot(ownership_data, aes(y = Income, x = Num_vehicles, color = factor(Num_children))) +
  geom_point()

#this confirms the previous plot. The individual who reported a $10,000,000 salary is also anticipating paying a $1,000,000 for a car. Likely a misunderstanding of the survey
ggplot(ownership_data, aes(x = Anticipated_car_price, y = Income, color = Education)) +
  geom_point()

ggplot(ownership_data, aes(x = Num_vehicles, y = Income, color = factor(Num_children))) +
  geom_point() +
  xlim(0, 5) +
  ylim(c(0, 350))

ggplot(ownership_data, aes(x = Num_vehicles, y = Income, color = Racial)) +
  geom_point() +
  xlim(0, 5) +
  ylim(c(0, 350))

#histogram of number of vehicles owned by the survey participants
ggplot(ownership_data, aes(Num_vehicles)) +
  geom_histogram()

#Salary info
#used median instead of mean to account for outliers
median(ownership_data$Income) # $60,000

ggplot(ownership_data, aes(Income)) +
  geom_density() +
  xlim(c(0, 350))

#checking distribution of education levels
#the median value is returned as associate's, but bachelor's is also quite frequent
median(ownership_data$Education) 
ggplot(ownership_data, aes(Education)) +
  geom_density()

#looking at number of children
max(ownership_data$Num_children, na.rm = T)

ggplot(ownership_data, aes(x = Num_children)) +
  geom_density()

median(ownership_data$Num_children, na.rm = T)

# Recontact Survey --------------------------------------------------------

# Select just Recontact data
rec_data <- data %>% 
  mutate(
    Racial = case_when(                         #creating a new column for race that is easier to perform analysis on
      Q4x12r1 == 1 ~ "Asian", 
      Q4x12r2 == 1 ~ "African American", 
      Q4x12r3 == 1 ~ "Hispanic", 
      Q4x12r4 == 1 ~ "White", 
      Q4x12r5 == 1 ~ "Other"
    )
  ) %>% 
  rename(
    Gender = Q4x1,                          # 1 = Female, 2 = Male, 3 = other
    Year_born = Q4x2, 
    Income = Q4x4,                          # in thousands
    Marital_status = Q4x5,                   # 1 = single, 2 = married, 3 = divorced, 4 = separated
    Children = Q4x6,                        # 1 = yes, 2 = no
    Num_children = Q4x7, 
    Job = Q4x8, 
    Education = Q4x9,                       # 1 = high school, 2 = associate's, 3 = bachelor's, 4 = graduate's, 5 = none of the above
    Residence = Q4x10,                      # 1 = renter, 2 = owner
  ) %>% 
  select(
    uuid, 
    Racial, 
    Gender, 
    Year_born, 
    Income, 
    Marital_status, 
    Children,
    Num_children,
    Job,
    Education,
    Residence,
    starts_with("REC_Q")
  ) %>% 
  filter(is.na(REC_Q1) == FALSE)

# How many people responded to recontact survey?
rec_data %>%
  summarize(
    responses = nrow(.)
  )

# How many people bought a car in last 18 months? 1=Yes 2=No
rec_data %>% 
  group_by(REC_Q1) %>% 
  summarize(
    n = n()
  )

## Clean Up Data Before Visualization

# Variables to discard after cleaning
discard <- c(16:23, 27:54)

# Recode some Variables
recontact <- rec_data %>% 
  # Rename Variables for easier understanding
  rename(
    Purchased = REC_Q1,              # 1 = Yes, 2 = No, 3 = Don't Remember
    Make = REC_Q2_1,
    Model = REC_Q2_2,
    Year = REC_Q2_3
  ) %>% 
  # Important Factors When Purchases Variable Recoding
  mutate(
    Factors_numeric = case_when(            #"Which of the following were important factors in deciding to purchase your car?"
      REC_Q3_1 == 1 ~ 1,                      
      REC_Q3_2 == 1 ~ 2, 
      REC_Q3_3 == 1 ~ 3, 
      REC_Q3_4 == 1 ~ 4, 
      REC_Q3_5 == 1 ~ 5, 
      REC_Q3_6 == 1 ~ 6, 
      REC_Q3_7 == 1 ~ 7
    ),
    Factors_text = case_when(               # Same as before just text version
      REC_Q3_1 == 1 ~ "Price",                      
      REC_Q3_2 == 1 ~ "Brand", 
      REC_Q3_3 == 1 ~ "Car Mileage", 
      REC_Q3_4 == 1 ~ "Warranty", 
      REC_Q3_5 == 1 ~ "Safety Rating", 
      REC_Q3_6 == 1 ~ "Relationship with Dealership", 
      REC_Q3_7 == 1 ~ "Other"
    )
  ) %>% 
  mutate(
    Important_text = case_when(            # Same as before just text version
      REC_Q4 == 1 ~ "Price",                      
      REC_Q4 == 2 ~ "Brand", 
      REC_Q4 == 3 ~ "Car Mileage", 
      REC_Q4 == 4 ~ "Warranty", 
      REC_Q4 == 5 ~ "Safety Rating", 
      REC_Q4 == 6 ~ "Relationship with Dealership", 
      REC_Q4 == 7 ~ "Other"
    ),
    Important_other = REC_Q4_7_SP          # Other Important reason for purchase
  ) %>% 
  rename(
    Important_numeric = REC_Q4,             # Most important Reason in Purchased 
    Purchase_satisfaction = REC_Q5_1       # Satisfaction Scale 1:5
  ) %>% 
  mutate(                                  # Reason for not Purchasing
    Why_no_purchase_numeric = case_when(
      REC_Q6_1 == 1 ~ 1,                      
      REC_Q6_2 == 1 ~ 2, 
      REC_Q6_3 == 1 ~ 3, 
      REC_Q6_4 == 1 ~ 4, 
      REC_Q6_5 == 1 ~ 5, 
      REC_Q6_6 == 1 ~ 6, 
      REC_Q6_7 == 1 ~ 7
    ),
    Why_no_purchase_text = case_when(
      REC_Q6_1 == 1 ~ "Not Driving Often",                      
      REC_Q6_2 == 1 ~ "Income Dropped", 
      REC_Q6_3 == 1 ~ "Medical Issues", 
      REC_Q6_4 == 1 ~ "Family Situation", 
      REC_Q6_5 == 1 ~ "Job Change", 
      REC_Q6_6 == 1 ~ "Other", 
      REC_Q6_7 == 1 ~ "Don't Know"
    ),
    No_purchase_explanation = REC_Q6_6_SP   # Reason for not Purchasing Explanation
  ) %>% 
  mutate(                                    # Dealerships Visited in Past 2 years
    Dealership_visited_numeric = case_when(
      REC_Q7_1 == 1 ~ 1,                      
      REC_Q7_2 == 1 ~ 2, 
      REC_Q7_3 == 1 ~ 3, 
      REC_Q7_4 == 1 ~ 4, 
      REC_Q7_5 == 1 ~ 5, 
      REC_Q7_6 == 1 ~ 6, 
      REC_Q7_7 == 1 ~ 7,
      REC_Q7_8 == 1 ~ 8, 
      REC_Q7_9 == 1 ~ 9,
      REC_Q7_10 == 1 ~ 10, 
      REC_Q7_11 == 1 ~ 11,
      REC_Q7_12 == 1 ~ 12,
      REC_Q7_13 == 1 ~ 13, 
      REC_Q7_14 == 1 ~ 14,
      REC_Q7_15 == 1 ~ 15,
      REC_Q7_16 == 1 ~ 16, 
      REC_Q7_17 == 1 ~ 17,
      REC_Q7_18 == 1 ~ 18, 
      REC_Q7_19 == 1 ~ 19
    ),
    Dealership_visited_text = case_when(
      REC_Q7_1 == 1 ~ "Jeep",                      
      REC_Q7_2 == 1 ~ "Toyota", 
      REC_Q7_3 == 1 ~ "Ford", 
      REC_Q7_4 == 1 ~ "Chevrolet", 
      REC_Q7_5 == 1 ~ "Honda", 
      REC_Q7_6 == 1 ~ "Nissan", 
      REC_Q7_7 == 1 ~ "Subaru",
      REC_Q7_8 == 1 ~ "Hyundai", 
      REC_Q7_9 == 1 ~ "GMC",
      REC_Q7_10 == 1 ~ "Kia", 
      REC_Q7_11 == 1 ~ "Lexus",
      REC_Q7_12 == 1 ~ "Mazda",
      REC_Q7_13 == 1 ~ "Buick", 
      REC_Q7_14 == 1 ~ "Mercedes-Benz",
      REC_Q7_15 == 1 ~ "Volkswagen",
      REC_Q7_16 == 1 ~ "BMW", 
      REC_Q7_17 == 1 ~ "Other",
      REC_Q7_18 == 1 ~ "None of the Above", 
      REC_Q7_19 == 1 ~ "Don't Remember"
    ),
    Dealership_other = REC_Q7_17_SP 
  ) %>% 
  select(-discard)


## Filter by the Q7 for Q8 analysis

## Visuals of Recontact Data

# Income Distribution of Purchased in last 18 months
recontact %>% 
  filter(Income < 500) %>% 
  ggplot(aes(x = Income)) +
  geom_histogram() +
  facet_grid(~Purchased)

# Most important Factor when purchasing a car
recontact %>% 
  group_by(Factors_text) %>% 
  summarize(
    n = n()
  ) %>% 
  drop_na() %>% 
  ggplot(aes(n, reorder(Factors_text, n))) +
  geom_col()

## Price and Brand Most important Factor when purchasing car

# Another way of showing what is most Important reason 
recontact %>% 
  group_by(Important_text) %>% 
  summarize(
    n = n()
  ) %>% 
  drop_na() %>% 
  ggplot(aes(n, reorder(Important_text, n))) +
  geom_col()

## Shouldn't this not be possible if it was filtered from above?

# Checking out which features made people more satisfied with their purchase
recontact %>% 
  drop_na(Important_text) %>% 
  ggplot(aes(Important_text, fill = factor(Purchase_satisfaction, ordered = TRUE))) +
  geom_bar() +
  coord_flip() +
  theme(
    legend.position = "none"
  )

## No relation between features and satisfaction

# Reason for not purchasing
recontact %>% 
  group_by(Why_no_purchase_text) %>% 
  summarize(
    n = n()
  ) %>% 
  drop_na() %>% 
  ggplot(aes(n, reorder(Why_no_purchase_text, n))) +
  geom_col()

## Income Dropping and Not driving enough is biggest reason for not purchasing

# looking at what dealerships visited the most
recontact %>% 
  group_by(Dealership_visited_text) %>% 
  summarize(
    n = n()
  ) %>% 
  ggplot(aes(n, reorder(Dealership_visited_text, n))) +
  geom_col()
  

recontact %>% 
  filter(Income < 500) %>% 
  drop_na(Purchase_satisfaction) %>% 
  ggplot(aes(y = Income, x = Dealership_visited_text, color = factor(Purchase_satisfaction))) +
  geom_point() +
  coord_flip()

# #this confirms the previous plot. The individual who reported a $10,000,000 salary is also anticipating paying a $1,000,000 for a car. Likely a misunderstanding of the survey
# ggplot(recontact, aes(x = Anticipated_car_price, y = Income, color = Education)) +
#   geom_point()
# 
# ggplot(recontact, aes(x = Num_vehicles, y = Income, color = factor(Num_children))) +
#   geom_point() +
#   xlim(0, 5) +
#   ylim(c(0, 350))
# 
# ggplot(recontact, aes(x = Num_vehicles, y = Income, color = Racial)) +
#   geom_point() +
#   xlim(0, 5) +
#   ylim(c(0, 350))

#Salary info
#used median instead of mean to account for outliers
median(recontact$Income) # $80,000

ggplot(recontact, aes(Income)) +
  geom_density() +
  xlim(c(0, 350))

#checking distribution of education levels
#the median value is returned as associate's, but bachelor's is also quite frequent
median(recontact$Education) 
ggplot(recontact, aes(Education)) +
  geom_density()

#looking at number of children
max(recontact$Num_children, na.rm = T)

ggplot(recontact, aes(x = Num_children)) +
  geom_density()

median(recontact$Num_children, na.rm = T)



