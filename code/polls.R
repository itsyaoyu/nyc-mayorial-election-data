# Loading in the necessary libraries
library(tidyverse)
library(scales)

# Data
polls <- read_csv("data/nyc_polls.csv",
                  col_types = cols(
                    date = col_date(format = "%m/%d/%y"),
                    pollster = col_character(),
                    sample_n = col_double(),
                    name = col_character(),
                    first_choice = col_double(),
                    second_choice = col_double(),
                    third_choice = col_double(),
                    link = col_character()
                  ))

# Plotting the data
polls %>% 
  pivot_longer(first_choice:third_choice, 
               names_to = "pick", 
               values_to = "response") %>% 
  drop_na() %>% 
  ggplot(aes(x = date, y = response, color = name, label = pollster)) +
  geom_line() +
  facet_wrap(~pick) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() + 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines")) +
  labs(title = "Recent Democratic Primary Polls for the 2021 NYC Mayoral Election",
       x = "",
       y = "")
