rm(list = ls())

library(tidyverse)
library(patchwork)
library(cowplot)
library(grid)
library(gridExtra)
color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
color_scheme_2 <- RColorBrewer::brewer.pal(12, "Paired")[1:12]
windowsFonts(sans = windowsFont("Lucida Bright"))
Sys.setlocale("LC_ALL","English")

load(file = "results/sim_transmission.RData")

legend <- sim_transmission %>% ggplot(mapping = aes(x = Visit_name,
                                          y = trans_prob,
                                          group = Farm_simulation)) +
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
                     labels = c("C1", "C2", "O1","O2",expression("M_scaling = 10"^-6),
                                expression("M_scaling = 10"^-7),
                                expression("M_scaling = 10"^-8)),
                     name = "",
                     guide = guide_legend(override.aes = list(linetype = c(1,1,1,1,0,0,0),
                                                              shape = c(NA,NA,NA,NA,4,4,4)))) +
  theme_bw(base_size = 12)+
  labs(x = "",
       y = "",
       title = "Transmission probabilities",
       subtitle = "Comparing simulations with farm data using different scaling factors")+
  theme(legend.position = "bottom")

p1 <- sim_transmission %>% ggplot(mapping = aes(x = Visit_name,
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
                     labels = c("C1", "C2", "O1","O2",expression("M_scaling = 10"^-6),
                                expression("M_scaling = 10"^-7),
                                expression("M_scaling = 10"^-8)),
                     name = "",
                     guide = guide_legend(override.aes = list(linetype = c(1,1,1,1,0,0,0),
                                                              shape = c(NA,NA,NA,NA,4,4,4)))) +
  theme_bw(base_size = 12)+
  labs(x = "",
       y = "",
       title = "Transmission probabilities",
       subtitle = "Comparing simulations with farm data using different scaling factors")+
  theme(legend.position = "none")

p2 <- sim_transmission %>% filter(!(Farm_simulation == "simulation_M6")) %>% 
  ggplot(mapping = aes(x = Visit_name,
                                                y = trans_prob,
                                                group = Farm_simulation))+
  geom_line(data = sim_transmission %>% 
              filter(Farm_simulation %in% c("C1", "C2", "O1", "O2")),
            aes(color = Farm_simulation))+
  geom_point(data = sim_transmission %>% 
               filter(Farm_simulation %in% c("simulation_M7", "simulation_M8")),
             aes(color=Farm_simulation),size = 3,shape=4) +
  scale_color_manual(values=c(color_scheme[4],color_scheme[5],
                              color_scheme[6],color_scheme[8],
                              color_scheme_2[6],
                              color_scheme_2[10]),
                     labels = c("C1", "C2", "O1","O2",expression("M_scaling = 10"^-7),
                                expression("M_scaling = 10"^-8)),
                     name = "",
                     guide = guide_legend(override.aes = list(linetype = c(1,1,1,1,0,0),
                                                              shape = c(NA,NA,NA,NA,4,4)))) +
  theme_bw(base_size = 12)+
  labs(x = "Visit",
       y = "")+
  theme(legend.position = "none")

plot_sim <- plot_grid(p1,p2,nrow = 2)
plot_legend <- get_legend(legend + theme(legend.justification="bottom"))

plot <- plot_grid(plot_sim, plot_legend,
          nrow = 2,
          rel_heights = c(15,1))


y.grob <- textGrob("Transmission probability", 
                   gp=gpar(fontsize=12), rot=90)

print_plot <-grid.arrange(arrangeGrob(plot, left = y.grob))

ggsave(filename = "results/figures/Final_figures/03_trans_prob.png", plot = print_plot) 
