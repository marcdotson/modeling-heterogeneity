#cleaning the data frame
library(dplyr)

#read in data
df <- read.csv(file.choose(), header = T)


#after the data is cleaned, these columns won't be needed anymore
positions <- c(1:21, 24:33, 45, 455)


tidy_df <-
  df %>% 
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

