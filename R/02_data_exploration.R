# 02 Data exploration

rm(list = ls())

library(tidyverse)


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")


# Load data ---------------------------------------------------------------
fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")


# Convert to factors ------------------------------------------------------

fluke_data <- fluke_data %>% mutate(Group = factor(x = Group,
                                     levels = c("Calf",
                                                "Heifer",
                                                "Primiparous",
                                                "Multiparous")))

# Counts ------------------------------------------------------------------

# Number of cows on each farm
fluke_data %>% group_by(Farm) %>% 
  distinct(UniqueID) %>% 
  summarise(n = n())

# Proportion of cows in each group on each farm
fluke_data %>% group_by(Farm) %>% 
  distinct(UniqueID, .keep_all = TRUE) %>% 
  count(Group) %>% 
  mutate(n = n/sum(n)) %>% 
  ggplot(mapping = aes(x = Farm,
                       y = n,
                       fill = Group)) +
  geom_col(position = "dodge")
  

# Ranges for numerical variables ------------------------------------------

min_max <- fluke_data %>% 
  summarise(across(where(is.numeric),
                   .fns = min_max))

negative_range_EPG <- fluke_data %>% 
  filter(dEPG == FALSE) %>% 
  summarise(min(EPG),max(EPG))

positive_range_EPG <- fluke_data %>% 
  filter(dEPG == TRUE) %>% 
  summarise(min(EPG),max(EPG))

negative_range_Serum <- fluke_data %>% 
  filter(dSerum == FALSE) %>% 
  summarise(min(Serum),max(Serum))

positive_range_Serum <- fluke_data %>% 
  filter(dSerum == TRUE) %>% 
  summarise(min(Serum),max(Serum))

negative_range_Copro <- fluke_data %>% 
  filter(dCopro == FALSE) %>% 
  summarise(min(Copro),max(Copro))

positive_range_Copro <- fluke_data %>% 
  filter(dCopro == TRUE) %>% 
  summarise(min(Copro),max(Copro))


fluke_data %>% filter(Copro < 1.89 & Copro > 1.4)


# dEPG counts -------------------------------------------------------------


# Numbers of positives within each group on each farm at each visit
positive_visit <- fluke_data %>% group_by(Farm,Visit) %>%
  count(dEPG) %>% 
  filter(dEPG == TRUE) %>% 
  ungroup()

positive_visit_group <- fluke_data %>% group_by(Farm,Visit,Group) %>%
  count(dEPG) %>% 
  filter(dEPG == TRUE) %>% 
  ungroup()

# Graph - Number of positives at each visit
positive_visit %>% ggplot(mapping = aes(x = Visit,
                                        y = n,
                                        color = Farm,
                                        group = Farm)) +
                  geom_line() +
                  geom_point()

# Graph - Number of positives at each visit in each group
positive_visit_group %>% ggplot(mapping = aes(x = Visit,
                                        y = n,
                                        color = Group,
                                        group = Group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name = "Positives", 
                     breaks = c(0,2,4,6,8,10)) +
    facet_wrap(~Farm)


# dSerum counts ------------------------------------------------------------

# Numbers of positives within each group on each farm at each visit
positive_visit <- fluke_data %>% group_by(Farm,Visit) %>%
  count(dSerum) %>% 
  filter(dSerum == TRUE) %>% 
  ungroup()

positive_visit_group <- fluke_data %>% group_by(Farm,Visit,Group) %>%
  count(dSerum) %>% 
  filter(dSerum == TRUE) %>% 
  ungroup()

# Graph - Number of positives at each visit
positive_visit %>% ggplot(mapping = aes(x = Visit,
                                        y = n,
                                        color = Farm,
                                        group = Farm)) +
  geom_line() +
  geom_point()

# Graph - Number of positives at each visit in each group
positive_visit_group %>% ggplot(mapping = aes(x = Visit,
                                              y = n,
                                              color = Group,
                                              group = Group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name = "Positives", 
                     breaks = c(0,2,4,6,8,10,12)) +
  facet_wrap(~Farm)
  
# dCopro counts ------------------------------------------------------------

# Numbers of positives within each group on each farm at each visit
positive_visit <- fluke_data %>% group_by(Farm,Visit) %>%
  count(dCopro) %>% 
  filter(dCopro == TRUE) %>% 
  ungroup()

positive_visit_group <- fluke_data %>% group_by(Farm,Visit,Group) %>%
  count(dCopro) %>% 
  filter(dCopro == TRUE) %>% 
  ungroup()

# Graph - Number of positives at each visit
positive_visit %>% ggplot(mapping = aes(x = Visit,
                                        y = n,
                                        color = Farm,
                                        group = Farm)) +
  geom_line() +
  geom_point()

# Graph - Number of positives at each visit in each group
positive_visit_group %>% ggplot(mapping = aes(x = Visit,
                                              y = n,
                                              color = Group,
                                              group = Group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name = "Positives", 
                     breaks = c(0,2,4,6,8,10,12)) +
  facet_wrap(~Farm)



# Diagnostic total --------------------------------------------------------

fluke_data_diag <- fluke_data %>% rowwise() %>% 
                    mutate(Diag = sum(as.numeric(dEPG),
                                  as.numeric(dSerum),
                                  as.numeric(dCopro),
                                  na.rm = TRUE),
                           Diag = if_else(Diag > 0, TRUE, FALSE))
                             

positive_visit_group <- fluke_data_diag %>% group_by(Farm,Visit,Group) %>%
  count(Diag) %>% 
  filter(Diag == TRUE) %>% 
  ungroup()

# Graph - Number of positives at each visit in each group
positive_visit_group %>% ggplot(mapping = aes(x = Visit,
                                              y = n,
                                              color = Group,
                                              group = Group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name = "Positives", 
                     breaks = c(0,2,4,6,8,10,12)) +
  facet_wrap(~Farm)

# Change in status --------------------------------------------------------
#Nested dataframe with diagnosis at each visit per cow
diag_status <- fluke_data_diag %>% group_by(UniqueID) %>% 
                    select(UniqueID,Visit,Diag) %>% 
                    arrange(UniqueID,Visit) %>% 
                    mutate(Diag = if_else(Diag,"T","F")) %>% 
                    select(UniqueID,Diag) %>%
                    ungroup() %>% 
                    nest_by(UniqueID)  

# Concatenate string to detect if cow has recovered between visits

Recovered_cows <- diag_status %>% 
  mutate(Diag_total = map(data, concat_str)) %>% 
  select(-data) %>% 
  unnest(cols = Diag_total) %>% 
  mutate(Recovered = str_detect(string = Diag_total,
             pattern = "TF")) %>% 
  filter(Recovered == TRUE) %>% 
  ungroup() %>% 
  pull(UniqueID)

# Finding cows which have recovered and have been treated

fluke_data %>% filter(UniqueID %in% Recovered_cows) %>% 
               drop_na(LastTreatmentDays) %>% 
               distinct(UniqueID) %>% 
               summarise(n = n())



  





