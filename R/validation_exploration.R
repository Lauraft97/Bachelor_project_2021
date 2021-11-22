# Preparation for validation

rm(list = ls())

library(tidyverse)


# Import functions --------------------------------------------------------
source(file ="R/99_functions.R")


# Load data ---------------------------------------------------------------
fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")


fluke_data <- fluke_data %>% mutate(Group = if_else(str_detect(Group,"s$"),
                                                    "Cow",
                                                     Group))


fluke_data <- fluke_data %>% mutate(Group = factor(x = Group,
                                                   levels = c("Calf",
                                                              "Heifer",
                                                              "Cow")))
#Parameters
year <- 365
month10 <- 304



# Investigate the distribution within groups at each farm (at visit A)

fluke_data_groups <- fluke_data %>% group_by(Farm, Group) %>% 
                     summarise(n = n())


# The age of each cow in days at each visit
fluke_data <- fluke_data %>% mutate(Cow_Age = as.numeric(Date - DOB))


# Subsetting all the cows which at some point have a positive test
# Divides data sets into each farm

fluke_diag <- fluke_data %>% rowwise() %>% 
  mutate(Diag = sum(as.numeric(dEPG),
                    as.numeric(dSerum),
                    as.numeric(dCopro),
                    na.rm = TRUE),
         Diag = if_else(Diag > 0, TRUE, FALSE)) %>% 
  filter(Diag == TRUE)


#Finding the age of the cow when it was first infected (when it was the youngest)

fluke_infected <- fluke_diag %>% group_by(UniqueID) %>% 
                  mutate(age_inf = min(Cow_Age)) %>% 
                  filter(Cow_Age == age_inf & Cow_Age > 0) %>% 
                  mutate(model_group = case_when(Cow_Age <= month10 ~ 1,
                                                 Cow_Age > month10 & First_sample - DOB <= 2*year ~ 2,
                                                 Cow_Age > 2*year ~ 3))


fluke_infected <- fluke_diag %>% 
  mutate(model_group = case_when(Cow_Age <= month10 ~ 1,
                                 Cow_Age > month10 & First_sample - DOB <= 2*year ~ 2,
                                 Cow_Age > 2*year ~ 3))


inf_count <- fluke_infected %>% group_by(Farm, Visit, model_group) %>% 
                   summarise(Count = n()) 

inf_count <- inf_count %>% mutate(model_group = factor(x = model_group,
                                                   levels = c("1","2","3")))


inf_count %>%  ggplot(aes(x = Visit, 
                          y = inf,
                          color = model_group,
                          group = model_group)) +
              geom_line() + 
              geom_point() + 
              facet_wrap(~ Farm, scales = "free")









