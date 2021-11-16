color_scheme <- RColorBrewer::brewer.pal(8, "Set2")[1:8]

# Temperature dependent exponential distribution CDF
x = seq(0,25,0.5)
ggplot(mapping = aes(x = x,
                     y = pexp(x,0.24))) +
  geom_line(col = color_scheme[1]) +
  #scale_color_brewer(palette = "Set2") +
  theme_bw(base_size = 8) +
  labs(x = "Corrected mean temperature [\u00B0C]",
       y = "Scalar",
       title = "Transmission temperature scalar") +
  theme(legend.position = "none")

ggsave(filename = "results/figures/trans_temp_scalar.png",
       width = 10, 
       height = 6.5, 
       units = "cm",
       dpi = 150)  


# Temperature dependent exponential distribution CDF
x = seq(0,25,0.5)
ggplot(mapping = aes(x = x,
                     y = 1-pexp(x,0.24))) +
  geom_line(col = color_scheme[1]) +
  #scale_color_brewer(palette = "Set2") +
  theme_bw(base_size = 8) +
  labs(x = "Corrected mean temperature [\u00B0C]",
       y = "Scalar",
       title = "Death rate temperature scalar") +
  theme(legend.position = "none")

ggsave(filename = "results/figures/mu_scalar.png",
       width = 10, 
       height = 6.5, 
       units = "cm",
       dpi = 150)  
