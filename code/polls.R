# Loading in the necessary libraries
library(tidyverse)
library(scales)

# Data
polls <- read_csv("raw-data/nyc_polls.csv")

# Plotting the data
polls %>% 
  pivot_longer(first_choice:third_choice, 
               names_to = "pick", 
               values_to = "response") %>% 
  ggplot(aes(x = date, y = response, color = name, label = pollster)) +
  geom_line() +
  facet_wrap(~pick) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "Recent Democratic Primary Polls for the 2021 NYC Mayoral Election",
       x = "",
       y = "")
