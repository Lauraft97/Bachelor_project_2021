C1 <- plot_SEI_total("C1",30)
C2 <- plot_SEI_total("C2",30)
O1 <- plot_SEI_total("O1",30)
O2 <- plot_SEI_total("O2",30)

library(cowplot)


legend <- plot_SEI_total("O2",30)


title <- "IBM - SEI dynamics"

combined_plot <- plot_grid(C1, C2, O1, O2, ncol=2)
plot_legend <- get_legend(legend + theme(legend.justification="bottom"))

plot_grid(combined_plot, plot_legend,
          ncol = 1,
          rel_heights = c(18,1))

ggsave(filename = "results/figures/Final_figures/SEI_total.png")
