library(dplyr)
library(ggplot2)


#this uses the modified data frame titled ownership_data as well as the original df titled data found in 02_exploratory-data-analysis.R

#---------------------
#checking most preferred future car from original data frame
fav_cars <- ownership_data %>% 
  select(1:8, 412:425, 478:482) 

#-----------------------------------

#pie chart of the racial distribution of survey participants
race_chart <- ggplot(fav_cars, aes(x = "", fill = factor(Racial))) +
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Race", 
       x = NULL,
       y = NULL, 
       title = "Racial distribution")

race_chart + coord_polar(theta = "y", start = 0)

#---------------------------------------------------

#pie chart of the distribution of number of children among survey participants
kid_chart <- ggplot(fav_cars, aes(x = "", fill = factor(Num_children))) +
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Kids", 
       x = NULL,
       y = NULL, 
       title = "Number of children")

kid_chart + coord_polar(theta = "y", start = 0)

#---------------------------------------------------

#likelihood of purchasing a new vehicle by race
#1 = most likely
ggplot(ownership_data, aes(x = Purchase_likelihood)) + 
  geom_bar() +
  xlab("Likelihood\n1: most likely") +
  ggtitle("Likelihood of purchasing a new vehicle\nBy race") +
  facet_wrap( ~ Racial, scales = "free")

#------------------------------------------------

#likelihood of purchasing a used vehicle by race
#1 = most likely
ggplot(ownership_data, aes(x = Used_vehicle_likelihood)) +
  geom_bar() +
  xlab("Likelihood\n1: most likely") +
  ggtitle("Likelihood of purchasing a used vehicle\nBy race") +
  facet_wrap( ~ Racial, scales = "free")

#--------------------------------------

#anticipated next vehicle price by income
ggplot(ownership_data, aes(x = Income, y = Anticipated_car_price)) +
  geom_bar(stat='identity') + 
  xlim(c(0, 100)) +
  ylim(c(0, 350)) +
  xlab("Income") +
  ylab("Price") +
  ggtitle("Anticipated price of next vehicle")

#---------------------------------------------

#anticipated next vehicle price by age

#making an age variable
ownership_data <- 
  ownership_data %>% 
  mutate(Age = (2020 - Year_born))

#age seems to be a factor
ggplot(ownership_data, aes(x = Age, y = Anticipated_car_price)) +
  geom_histogram(stat='identity') + 
  ylim(c(0, 350)) +
  facet_wrap( ~ Gender)

#----------------------------------------------

#checking most preferred future car from original data frame
first_car_choice <- data %>% 
  select(Q2x7r1:Q2x7r3)

car_preferences <- first_car_choice %>% 
  group_by(Q2x7r1) %>% 
  summarize(number_rows = n()) %>% 
  arrange(desc(number_rows)) #Top 4 cars = Toyota, Ford, Honda, Chevrolet

top_10_choices <- 
  car_preferences %>% 
  top_n(10)

ggplot(top_10_choices, aes(x =reorder(Q2x7r1, -number_rows), y = number_rows)) +
  geom_bar(stat='identity') +
  xlab("Brands") +
  ylab("Number of picks") +
  ggtitle("Top 10 Brands \nFirst choice pick of new potential car")
