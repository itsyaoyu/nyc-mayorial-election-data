# Loading in the necessary libraries
library(tidyverse)
library(janitor)

# Data
candidates <- read_csv("data/candidates.csv",
                       col_types = cols(
                         recipname = col_character(),
                         name = col_character()
                       )) %>% 
  mutate(name = as_factor(name))

data <- read_csv("data/individual_contributions.csv",
                 col_types = cols(
                   .default = col_character(),
                   ELECTION = col_double(),
                   OFFICECD = col_double(),
                   RECIPID = col_double(),
                   FILING = col_double(),
                   PAGENO = col_logical(),
                   SEQUENCENO = col_logical(),
                   STRNO = col_logical(),
                   STRNAME = col_logical(),
                   APARTMENT = col_logical(),
                   AMNT = col_double(),
                   MATCHAMNT = col_double(),
                   PREVAMNT = col_double(),
                   PAY_METHOD = col_double(),
                   INTERMNO = col_double(),
                   INTSTRNO = col_logical(),
                   INTSTRNM = col_logical(),
                   INTAPTNO = col_logical(),
                   INTZIP = col_character(),
                   PURPOSECD = col_logical(),
                   EXEMPTCD = col_logical()
                 )) %>% 
  clean_names() %>% 
  select(recipname, date, name, boroughcd, city, state, zip, 
         occupation, empname, amnt, matchamnt, pay_method) %>% 
  rename(donor = name) %>% 
  mutate(date = as.Date.character(date, format = "%m/%d/%Y")) %>% 
  filter(recipname %in% candidates$recipname) %>% 
  left_join(candidates, by = "recipname")

### Data cleaning
# Filtering for only NYC Contributions
ny_contributions <- data %>% 
  filter(boroughcd != "Z")

# All unique contributors
contributions_unique <- data %>% 
  mutate(donor_recip = paste(donor, "_", recipname, sep = ""),
         unique = !duplicated(donor_recip)) %>% 
  filter(unique)

# 1a) How Many Individual Contributors Does Each Candidate Have? NYC
ny_contributions %>% 
  arrange(date) %>% 
  mutate(donor_recip = paste(donor, "_", recipname, sep = ""),
         unique = !duplicated(donor_recip)) %>% 
  filter(unique) %>% 
  group_by(name) %>% 
  count(date) %>% 
  arrange(date) %>% 
  mutate(total_contributors = cumsum(n)) %>% 
  rename(Date = date,
         `Total Contributors` = total_contributors,
         Candidate = name) %>% 
  ggplot(aes(x = Date, y = `Total Contributors`, color = Candidate)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "NYC",
       x = "",
       y = "# of People")

# 1b) How Many Individual Contributors Does Each Candidate Have? US

data %>% 
  arrange(date) %>% 
  mutate(donor_recip = paste(donor, "_", recipname, sep = ""),
         unique = !duplicated(donor_recip)) %>% 
  filter(unique) %>% 
  group_by(name) %>% 
  count(date) %>% 
  arrange(date) %>% 
  mutate(total_contributors = cumsum(n)) %>% 
  rename(Date = date,
         `Total Contributors` = total_contributors,
         Candidate = name) %>% 
  ggplot(aes(x = Date, y = `Total Contributors`, color = Candidate)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "US",
       x = "",
       y = "")

# 2) Which Borough Do Candidates Receive The Most Contributions From?
# Counting all unique donors from US
ind_donors <- data %>% 
  count(donor, recipname) %>% 
  count(recipname) %>% 
  rename(US = n)

# Counting all unique donors from NYC
nyc_ind_donors <- ny_contributions %>% 
  count(donor, recipname) %>% 
  count(recipname) %>% 
  rename(NYC = n)

# Calculating the proportion of unique donors by borough and outside NYC
contributions_boroughs <- data %>% 
  count(donor, recipname, boroughcd) %>% 
  count(recipname, boroughcd) %>% 
  drop_na() %>% 
  mutate(boroughcd = case_when(
    boroughcd == "X" ~ "Bronx",
    boroughcd == "K" ~ "Brooklyn",
    boroughcd == "M" ~ "Manhattan",
    boroughcd == "Q" ~ "Queens",
    boroughcd == "S" ~ "Staten Island",
    boroughcd == "Z" ~ "Outside New York City"
  )) %>% 
  group_by(recipname) %>% 
  summarize(borough = boroughcd,
            prop = n/sum(n),
            .groups = "drop_last") %>% 
  pivot_wider(names_from = borough,
              values_from = prop) %>% 
  mutate(`Staten Island` = ifelse(is.na(`Staten Island`), 0, `Staten Island`)) %>% 
  relocate(Bronx, .after = recipname)

# Joining all the data to create full table
candidates %>% 
  full_join(nyc_ind_donors, by = "recipname") %>% 
  full_join(ind_donors, by = "recipname") %>% 
  full_join(contributions_boroughs, by = "recipname") %>% 
  select(-recipname) %>% 
  rename(Candidate = name)

# 3) Which Candidates Do People From Each Borough Donate The Most To?
# Counting all unique donors per borough
ind_donors_borough <- contributions_unique %>% 
  count(boroughcd, recipname) %>% 
  drop_na() %>% 
  mutate(boroughcd = case_when(
    boroughcd == "X" ~ "Bronx",
    boroughcd == "K" ~ "Brooklyn",
    boroughcd == "M" ~ "Manhattan",
    boroughcd == "Q" ~ "Queens",
    boroughcd == "S" ~ "Staten Island",
    boroughcd == "Z" ~ "Outside New York City"
  )) %>% 
  group_by(boroughcd) %>% 
  summarize(recipname = recipname,
            prop = n/sum(n),
            .groups = "drop_last") %>% 
  pivot_wider(names_from = boroughcd,
              values_from = prop) %>% 
  relocate(`Outside New York City`, .after = `Staten Island`) %>% 
  mutate(`Staten Island` = ifelse(is.na(`Staten Island`), 0, `Staten Island`))

# Joining all the data to create full table
candidates %>% 
  full_join(ind_donors_borough, by = "recipname") %>% 
  select(-recipname) %>% 
  rename(Candidate = name)
