
library(ggplot2)
library(patchwork)
library(mdiHelpR)
library(magrittr)
set.seed(1)
d1 <- sample(1:100, 40) %>% matrix(nrow = 10)
d2 <- sample(1:25, 100, replace =T) %>% matrix(nrow = 10)
d3 <- sample(1:5, 10,replace = T) %>% matrix(nrow = 5)

col_pal <- dataColPal()
breaks <- defineBreaks(col_pal)
g1 <- draw_legend(col_pal, breaks)
 p1 <- ggplot() + cowplot::draw_grob(g1) + theme_minimal()

 p2+ggtitle("Plot 3") + theme(plot.margin = unit(c(1, 7, 1, 6), "cm"))
 p2+ggtitle("Plot 3") + theme(plot.margin = unit(c(1, 12, 1, 6), "cm"))
 p2+ggtitle("Plot 3") + theme(plot.margin = unit(c(1, 6, 3, 6), "cm"))
 p2+ggtitle("Plot 3") + theme(plot.margin = unit(c(1, 6, 5, 6), "cm"))
 p2+ggtitle("Plot 3") + theme(plot.margin = unit(c(1, 1, 1, 6), "cm"))
 p2+ggtitle("Plot 3") + theme(plot.margin = unit(c(2, 1, 1, 6), "cm"))
 p2+ggtitle("Plot 3") + theme(plot.margin = unit(c(2, 1, 1, 1), "cm"))

 p2 <- ggplot() + cowplot::draw_grob(g1) +  theme_void()+ggtitle("Plot 1") 
 p3 <- p2+ggtitle("Plot 3") + theme(plot.margin = unit(c(2, 1, 1, 1), "cm"))
 
 cowplot::plot_grid(compareMatricesGen(d1,d2), p_l, rel_widths = c(10,1))
 cowplot::plot_grid(compareMatricesGen(d1,d2), p_l, rel_widths = c(10,5))
 cowplot::plot_grid(compareMatricesGen(d1,d2), p2, p3, rel_widths = c(10,1,1))
 cowplot::plot_grid(compareMatricesGen(d1,d2), p2, p3, rel_widths = c(10,1,1), nrow = 1)
 ph_c <- compareMatricesGen(d1,d2)
 ph_c + big_p
 cowplot::plot_grid(p2,p2, p3, p3, nrow = 2)
 l_grid <- cowplot::plot_grid(p2,p2, p2, p2, nrow = 2)
 
 ph_c <- compareMatricesGen(d1,d2, d1, d3, d2, d3, d3, d1, d3, d3,d3, collect_legend = F)
 l_grid <- cowplot::plot_grid(p2,p2, p2, p2, p2 , p2, p2, p2, p2)
 
 cowplot::plot_grid(ph_c, l_grid, rel_widths = c(10,2))
 
 cowplot::plot_grid(ph_c, p1, rel_widths = c(10,1))

 compareMatricesGen(d3, d3, d3,d3)
 