C1 <- plot_SEI_total("C1",10)
C2 <- plot_SEI_total("C2",10)
O1 <- plot_SEI_total("O1",10)
O2 <- plot_SEI_total("O2",10)


legend <- plot_SEI_total("O2",10)


title <- "IBM - SEI dynamics"

combined_plot <- plot_grid(C1, C2, O1, O2, ncol=2)
plot_legend <- get_legend(legend + theme(legend.justification="bottom"))

plot_grid(combined_plot, plot_legend,
          ncol = 1,
          rel_heights = c(18,1))

ggsave(filename = "results/figures/Final_figures/SEI_total.png")
