#Plotting simulations for the four farms

#SEI plots
plot_SEI("C1", 10)
ggsave(filename = "results/figures/SEI_plot_C1.png")
plot_SEI("C2", 10)
ggsave(filename = "results/figures/SEI_plot_C2.png")
plot_SEI("O1", 10)
ggsave(filename = "results/figures/SEI_plot_O1.png")
plot_SEI("O2", 10)
ggsave(filename = "results/figures/SEI_plot_O2.png")


#ODE plots
plot_ODE("C1", "Egg")
ggsave(filename = "results/figures/ODE_plot_C1_Egg.png")
plot_ODE("C1", "Exposed_1")
ggsave(filename = "results/figures/ODE_plot_C1_Exposed_1.png")
plot_ODE("C1", "Exposed_2")
ggsave(filename = "results/figures/ODE_plot_C1_Exposed_2.png")
plot_ODE("C1", "Infected")
ggsave(filename = "results/figures/ODE_plot_C1_Infected.png")
plot_ODE("C1", "Susceptible_snail")
ggsave(filename = "results/figures/ODE_plot_C1_Susceptible_snail.png")
plot_ODE("C1", "Snail_population")
ggsave(filename = "results/figures/ODE_plot_C1_Snail_population.png")
plot_ODE("C1", "Metacercariae")
ggsave(filename = "results/figures/ODE_plot_C1_Smetacercariae.png")
