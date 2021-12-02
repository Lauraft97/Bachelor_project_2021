rm(list = ls())

library(tidyverse)
library(patchwork)
library("RColorBrewer")
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]

load(file = "results/sim_transmission.RData")

sim_transmission %>% ggplot(mapping = aes(x = Visit_name,
                                          y = trans_prob,
                                          group = Farm_simulation))+
  geom_line(data = sim_transmission %>% 
              filter(Farm_simulation %in% c("C1", "C2", "O1", "O2")),
            aes(color = Farm_simulation))+
  geom_point(data = sim_transmission %>% 
               filter(Farm_simulation %in% c("simulation_M6", "simulation_M7", "simulation_M8")),
             aes(color=Farm_simulation),size = 3,shape=4) +
  scale_color_manual(values=c(color_scheme[4],color_scheme[5],
                              color_scheme[6],color_scheme[8],
                              color_scheme_2[2],
                              color_scheme_2[6],
                              color_scheme_2[10]),
                     labels = c("C1", "C2", "O1","O2",expression("M_scaling = 10"^-8),
                                expression("M_scaling = 10"^-7),
                                expression("M_scaling = 10"^-6)),
                     name = "",
                     guide = guide_legend(override.aes = list(linetype = c(1,1,1,1,0,0,0),
                                                              shape = c(NA,NA,NA,NA,4,4,4)))) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright")+
  labs(x = "",
       y = "Transmission probability",
       title = "Transmission probabilities",
       subtitle = "Comparing simulations with farm data using different scaling factors")+
  theme(legend.position = "bottom")


p1 <- sim_transmission %>% ggplot(mapping = aes(x = dates,
                                                group = 6)) +
  geom_point(aes(y = C1, col = "C1"))+ 
  geom_line(aes(y = C1), color = color_scheme[4])+
  geom_point(aes(y = C2,col ="C2"))+
  geom_line(aes(y = C2),col = color_scheme[5])+
  geom_point(aes(y = O1, col = "O1"))+
  geom_line(aes(y = O1),col = color_scheme[6])+
  geom_point(aes(y = O2, col="O2"))+
  geom_line(aes(y = O2),col = color_scheme[8])+
  geom_point(aes(y = simulation_M8, col ="M_scaling = 10^(-8)"), shape = 4, size = 3) +
  geom_point(aes(y = simulation_M7, col ="M_scaling = 10^(-7)"), shape = 4, size = 3) +
  geom_point(aes(y = simulation_M6, col ="M_scaling = 10^(-6)"), shape = 4, size = 3) +
  theme(legend.position = "bottom") +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  scale_color_manual(name="",
                     breaks=c("M_scaling = 10^(-8)","M_scaling = 10^(-7)",
                              "M_scaling = 10^(-6)", "C1", "C2", "O1","O2"),
                     values=c("M_scaling = 10^(-8)"= color_scheme_2[2],
                              "M_scaling = 10^(-7)"= color_scheme_2[6],
                              "M_scaling = 10^(-6)"= color_scheme_2[10], 
                              "C1" = color_scheme[4],"C2" = color_scheme[5],
                              "O1" = color_scheme[6],"O2" = color_scheme[8]),
                     guide = guide_legend(override.aes = list(shape = c(4,4,4,16,16,16,16),
                                                              size = c(2,2,2,2,2,2,2))))+
  labs(x = "",
       y = "Transmission probability",
       title = "Comparing simulated transmission probabilities with farm \n data using different scaling factors")

p2 <- sim_transmission %>% ggplot(mapping = aes(x = dates,
                                                group = 5)) +
  geom_point(aes(y = C1, col = "C1"))+ 
  geom_line(aes(y = C1), color = color_scheme[4])+
  geom_point(aes(y = C2,col ="C2"))+
  geom_line(aes(y = C2),col = color_scheme[5])+
  geom_point(aes(y = O1, col = "O1"))+
  geom_line(aes(y = O1),col = color_scheme[6])+
  geom_point(aes(y = O2, col="O2"))+
  geom_line(aes(y = O2),col = color_scheme[8])+
  geom_point(aes(y = simulation_M8, col ="M_scaling = 10^(-8)"), shape = 4, size = 3) +
  geom_point(aes(y = simulation_M7, col ="M_scaling = 10^(-7)"), shape = 4, size = 3) +
  theme_bw(base_size = 12,
           base_family = "Lucida Bright") +
  scale_color_manual(name="",
                     breaks=c("M_scaling = 10^(-8)","M_scaling = 10^(-7)",
                              "C1", "C2", "O1","O2"),
                     values=c("M_scaling = 10^(-8)"= color_scheme_2[2],
                              "M_scaling = 10^(-7)"= color_scheme_2[6],
                              "C1" = color_scheme[4],"C2" = color_scheme[5],
                              "O1" = color_scheme[6],"O2" = color_scheme[8]),
                     guide = guide_legend(override.aes = list(shape = c(4,4,16,16,16,16),
                                                              size = c(2,2,2,2,2,2))))+
  labs(x = "Visit",
       y = "Transmission probability")+
  theme(legend.position = "none") 


p1/p2
ggsave(filename = "results/figures/Final_figures/03_trans_prob.png") 

