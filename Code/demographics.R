library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)

#looking at the demographics of our survey participants


#using the tidy_df created in the "Tidying data.R" file

#there are a handful of outliers in the income data... Some reported making upwards of $10,000,000 a year. It could also be that they didn't enter in their salary as thousands
ggplot(tidy_df, aes(y = Income, x = Num_vehicles, color = factor(Num_children))) +
  geom_point()

#this confirms the previous plot. The individual who reported a $10,000,000 salary is also anticipating paying a $1,000,000 for a car. Likely a misunderstanding of the survey
ggplot(tidy_df, aes(x = Anticipated_car_price, y = Income, color = Education)) +
  geom_point()

ggplot(tidy_df, aes(x = Num_vehicles, y = Income, color = factor(Num_children))) +
  geom_point() +
  xlim(0, 5) +
  ylim(c(0, 350))

ggplot(tidy_df, aes(x = Num_vehicles, y = Income, color = Racial)) +
  geom_point() +
  xlim(0, 5) +
  ylim(c(0, 350))


#histogram of number of vehicles owned by the survey participants
ggplot(tidy_df, aes(Num_vehicles)) +
  geom_histogram()



#Salary info
#used median instead of mean to account for outliers
median(tidy_df$Income) # $60,000

ggplot(tidy_df, aes(Income)) +
  geom_density() +
  xlim(c(0, 350))


#checking distribution of education levels
#the median value is returned as associate's, but bachelor's is also quite frequent
median(tidy_df$Education) 
ggplot(tidy_df, aes(Education)) +
  geom_density()


#looking at number of children
max(tidy_df$Num_children, na.rm = T)

ggplot(tidy_df, aes(x = Num_children)) +
  geom_density()

median(tidy_df$Num_children, na.rm = T)

