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
plot_ODE("C1", "Exposed_1")
plot_ODE("C1", "Exposed_2")
plot_ODE("C1", "Infected")
plot_ODE("C1", "Susceptible_snail")
plot_ODE("C1", "Snail_population")
plot_ODE("C1", "Metacercariae")
