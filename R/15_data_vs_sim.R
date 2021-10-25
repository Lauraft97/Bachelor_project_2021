# Validation vs data


# Load data ---------------------------------------------------------------
fluke_data <- read_tsv(file = "data/01_fluke_data_clean.tsv")


# Convert to factors ------------------------------------------------------

fluke_data <- fluke_data %>% mutate(Group = factor(x = Group,
                                                   levels = c("Calf",
                                                              "Heifer",
                                                              "Primiparous",
                                                              "Multiparous")))

# Subsetting all the cows which at some point have a positive test
# Divides data sets into each farm

fluke_diag <- fluke_data %>% rowwise() %>% 
  mutate(Diag = sum(as.numeric(dEPG),
                    as.numeric(dSerum),
                    as.numeric(dCopro),
                    na.rm = TRUE),
         Diag = if_else(Diag > 0, TRUE, FALSE))



sick_sim <- validation %>% filter(I_period > 0) %>% group_by(Visit_day_no) %>% summarise(n = n())

sick_sim %>% ggplot(aes(x = Visit_day_no,
                        y = n)) + geom_line()


sick_data <- fluke_diag %>% filter(Farm == "C1", Diag == T) %>% group_by(Visit) %>% summarise(n = n()) %>% slice(-1) 
  
sick_data %>%  ggplot(aes(x = Visit,
                          y = n)) + geom_point() + geom_point(aes(y = sick_sim$n,
                                                                  col = "red"))
