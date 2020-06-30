
library(pheatmap)
library(mdiHelpR)
library(ggplot2)
library(magrittr)
my_data <- generateSimulationDataset(5, 100, 10)
ph1 <- annotatedHeatmap(my_data$data, my_data$cluster_IDs, silent=T, 
                        legend=T,
                        show_rownames = F,
                        show_colnames= F,
                        cluster_rows = F,
                        cluster_cols = F,
                        main=NA )

ph1

ph2 <- pheatmap(my_data$data,
                legend=T,
                show_rownames = F,
                show_colnames= F,
                cluster_rows = F,
                cluster_cols = F)

ph1$gtable$layout

mat <- ggplot()+
  cowplot::draw_grob(ph1$gtable$grobs[[1]])
ann_title <- ggplot()+
  cowplot::draw_grob(ph1$gtable$grobs[[2]])
ann_title

ph1$gtable$grobs[[2]]$height <- unit(0.01, "npc")

ann_row <- ggplot()+
  cowplot::draw_grob(ph1$gtable$grobs[[3]]) 

ann_legend <- ggplot()+
  cowplot::draw_grob(ph1$gtable$grobs[[4]])

legend <- ggplot()+ 
  cowplot::draw_grob(ph1$gtable$grobs[[5]])

library(patchwork)

legend+ann_legend

library(cowplot)


layout <- matrix(1, nrow=1,ncol=2)
plot(gridExtra::arrangeGrob(ph1$gtable$grobs[[5]], ph1$gtable$grobs[[4]],
                            layout_matrix = layout))

g <- gridExtra::arrangeGrob(ph1$gtable$grobs[[5]], ph1$gtable$grobs[[4]])
library(gridExtra)

m1 <- marrangeGrob(list(ph1$gtable$grobs[[4]], ph1$gtable$grobs[[5]]), nrow=1, ncol=2)

mat+m1

m2 <- grid.arrange(ph1$gtable$grobs[[4]], ph1$gtable$grobs[[5]], ncol=2)

cowplot::plot_grid(mat,m2, rel_widths = c(10,5))

m3 <- plot_grid(NULL, m2, rel_heights = c(1,10), nrow=2)
cowplot::plot_grid(mat,m3, rel_widths = c(10,5))


grid.arrange(ph1$gtable$grobs[[2]], ph1$gtable$grobs[[3]], ncol=1)

grid.arrange(ph1$gtable$grobs[[3]], ph1$gtable$grobs[[2]], ncol=1)

grid.arrange(ph1$gtable$grobs[[2]], ph1$gtable$grobs[[2]],ph1$gtable$grobs[[2]], ncol=1)

ann_grid <- cowplot::plot_grid(plotlist = list(ph1$gtable$grobs[[2]], 
                                   ph1$gtable$grobs[[2]], 
                                   ph1$gtable$grobs[[2]], 
                                   ph1$gtable$grobs[[2]]), nrow = 4, ncol = 1)


# p2ggplot()+
library(ggplotify)
p1 <- ggplotify::as.ggplot(ann_grob)
p2 <- ggplotify::as.ggplot(ph1$gtable$grobs[[2]])

ph1$gtable$grobs[[2]] %>% ls.str()
ph1$gtable$grobs[[1]] %>% ls.str()
ph1$gtable$grobs[[1]]$children %>% ls.str()

ph1$gtable$grobs[[2]]$y
ph1$gtable$grobs[[1]]$children$GRID.rect.5$y

ann_grid <- cowplot::plot_grid(NULL, p2, NULL, p2, NULL, p2, NULL, p2, ncol=1, rel_heights = c(0.1, rep(c(1,.1), 3),1))

ann_grid <- plot_grid(ann_title, ann_title, ann_title,ann_title, ncol =1)

ann_title
ann_row

# ann_grid <- cowplot::plot_grid(ann_grid, NULL, nrow = 2, ncol = 1, rel_heights = c(10,1))
# ann_plot1

ann_plot1 <-wrap_elements(panel = ph1$gtable$grobs[[2]])

ph_grid_small <- compareMatricesGen(my_data$data, my_data$data, n_col = 1)
my_ph <- pheatmap(my_data$data)

ph_plot1 <-wrap_elements(panel =my_ph$gtable$grobs[[3]])
my_list <- list(ann_plot1,ph_plot1, ann_plot1, ph_plot1)
p1 <- patchwork::wrap_plots(ph_plot1, ph_plot1, ncol = 1) # , widths = c(1,10))
p2 <- patchwork::wrap_plots(ann_plot1, ann_plot1, ncol = 1) # , widths = c(1,10))
patchwork::wrap_plots(p2, p1, ncol = 2, widths = c(1,10))

my_ph <- pheatmap(d2)

cluster_rows <- T
cluster_cols <- T
show_rownames <- T
show_colnames <- T
legend <- T

my_ph2 <- pheatmap(d2, 
                  cluster_rows = cluster_rows,
                  cluster_cols = cluster_cols,
                  show_rownames = show_rownames,
                  show_colnames = show_colnames,
                  silent = TRUE,
                  legend = legend,
                  annotation_row = NA,
                  annotation_colours = NA)$gtable

cluster_rows <- F
cluster_cols <- F
show_rownames <- T
show_colnames <- T
legend <- T

my_ph3 <- pheatmap(d2, 
                  cluster_rows = cluster_rows,
                  cluster_cols = cluster_cols,
                  show_rownames = show_rownames,
                  show_colnames = show_colnames,
                  silent = TRUE,
                  legend = legend,
                  annotation_row = NA,
                  annotation_colours = NA)$gtable

cluster_rows <- F
cluster_cols <- F
show_rownames <- F
show_colnames <- F
legend <- T

my_ph4 <-pheatmap(d2, 
                 cluster_rows = cluster_rows,
                 cluster_cols = cluster_cols,
                 show_rownames = show_rownames,
                 show_colnames = show_colnames,
                 silent = TRUE,
                 legend = legend,
                 annotation_row = NA,
                 annotation_colours = NA)$gtable

cluster_rows <- F
cluster_cols <- F
show_rownames <- F
show_colnames <- F
legend <- F

my_ph5 <-pheatmap(d2, 
                  cluster_rows = cluster_rows,
                  cluster_cols = cluster_cols,
                  show_rownames = show_rownames,
                  show_colnames = show_colnames,
                  silent = TRUE,
                  legend = legend,
                  annotation_row = NA,
                  annotation_colours = NA)$gtable


wrap_elements(panel =my_ph2$grobs[[1]])
wrap_elements(panel =my_ph2$grobs[[2]])
wrap_elements(panel =my_ph5$grobs[[1 + cluster_rows + cluster_cols]])
wrap_elements(panel =my_ph2$grobs[[4]])

wrap_elements(panel =my_ph$grobs[[1]])

my_ph_grid <- compareMatricesGen(d1, d2, collect_legend = T, order_cols = F, order_rows = F)

mat1 <- Matrix::readMM("/Users/stephen/Desktop/Work_update_2020_05_11/Model_performance/Model_performance/base_case/Bayesian/Simulation1PSMN1e+06S10.txt") %>% 
  as.matrix()

row.names(mat1)
colnames(mat1)

mat1

row_names <- row.names(mat1)
if(is.null(row_names)){
  row_names <- row.names(mat1) <- colnames(mat1) <- paste0("Person ", 1:nrow(mat1))
}

base_data <- generateSimulationDataset(5, 200, 20)

cluster_IDs <- base_data$cluster_IDs
# Create the annotation data.frame for the rows
anno_row <- data.frame(Cluster = factor(paste("Cluster", cluster_IDs))) %>%
  magrittr::set_rownames(row_names)


# The number of cololurs to use
K <- length(unique(cluster_IDs))

# Create the annotation colours
ann_colours <- list(Cluster = viridis::viridis(K) %>%
                      magrittr::set_names(paste("Cluster", sort(unique(cluster_IDs))))
)

anno_plot <- pheatmap::pheatmap(mat1, 
                                annotation_row = anno_row,
                                annotation_col = anno_row,
                                annotation_colors = ann_colours,
                                silent = F,
                                cluster_rows = F,
                                cluster_cols = F,
                                show_rownames = F, 
                                show_colnames = F)$gtable

wrap_elements(panel = anno_plot$grobs[[2]])
wrap_elements(panel = anno_plot$grobs[[4]])
wrap_elements(panel = anno_plot$grobs[[3]])
wrap_elements(panel = anno_plot$grobs[[5]])


anno_leg <- anno_plot$grobs[[6]]
grid.ls(grid.force())
anno_leg$children$GRID.text.3416
grid.gedit(anno_leg$children$GRID.text.3416, gp = gpar(col="grey70"))

anno_leg$children$GRID.text.3416
patchwork::wrap_plots(wrap_elements(panel = anno_plot$grobs[[7]]), 
wrap_elements(panel = anno_leg, full = rectGrob(gp = gpar(fill = "#21677e"))), nrow = 1
)
setMyTheme()
compareSimilarityMatricesAnnotated(mat1, mat1, mat1, mat1, cluster_IDs = cluster_IDs,
                                   title = "My lovely plot",
                                   fill = NULL)

compareSimilarityMatricesAnnotated(mat1, mat1, mat1, mat1, cluster_IDs = NULL,
                                   title = "My lovely plot",
                                   fill = NULL)

compareSimilarityMatricesAnnotated(mat1, mat1, mat1, mat1, cluster_IDs = cluster_IDs,
                                   title = "My lovely plot",
                                   fill = "#FFDEAD")

sim_col2 <- grDevices::colorRampPalette(c("white", "#21677e"))(100)

  
compareSimilarityMatricesAnnotated(mat1*0.5, mat1, mat1, mat1, cluster_IDs = cluster_IDs,
                                   col_pal = sim_col2,
                                   title = "My lovely plot",
                                   fill = "#B5D334")

mult <- sample(1:100, size = 200 * 200, replace = T) / 100
mat2 <- mat1*mult
diag(mat2) <- 1
compareSimilarityMatricesAnnotated(mat2, mat1, mat1, mat1, cluster_IDs = cluster_IDs,
                                   col_pal = sim_col2,
                                   matrix_imposing_order = 4,
                                   title = "My lovely plot",
                                   fill = "#8FCDE1")
compareSimilarityMatricesAnnotated(mat2, mat1, mat1, mat1, cluster_IDs = cluster_IDs,
                                   # col_pal = sim_col2,
                                   matrix_imposing_order = 4,
                                   title = "My lovely plot",
                                   fill = "#8FCDE1")


compareSimilarityMatricesAnnotated(mat2, mat1, mat1, mat1, cluster_IDs = cluster_IDs,
                                   col_pal = sim_col2,
                                   title = "My lovely plot",
                                   fill = "#EEECE1")



# Create the annotation data.frame for the rows
anno_row <- data.frame(Cluster = factor(paste("Cluster", my_data$cluster_IDs))) %>%
  magrittr::set_rownames(rownames(my_data$data))

# The number of cololurs to use
K <- length(unique(cluster_IDs))

# Create the annotation colours
ann_colours <- list(Cluster = viridis::viridis(K) %>%
                      magrittr::set_names(paste("Cluster", sort(unique(cluster_IDs))))
)

anno_plot <- pheatmap::pheatmap(m_star, 
                                annotation_row = anno_row,
                                annotation_colors = ann_colours,
                                silent = T)$gtable

ann_plot1 <-wrap_elements(panel = ph1$gtable$grobs[[2]])
ann_plot1_grid <- wrap_plots(ann_plot1, plot_spacer(), ncol = 1, heights = c(10, 1))

wrap_plots(ann_plot1_grid, my_ph_grid, ncol = 2, widths = c(1, 10))
          
p_anno <- ann_plot1 / ann_plot1 / ann_plot1 / ann_plot1 

(ann_plot1, ann_plot1, ann_plot1, ann_plot1)
ann_plot1 %>% ls.str()





ph1$gtable$grobs[[2]])
ann_grid <- plot_grid(ann_plot1, ann_plot1, ann_plot1,ann_plot1, ncol =1)
ann_grid <- cowplot::plot_grid(p_anno, NULL, nrow = 2, ncol = 1, rel_heights = c(10,1))

annotation_plots <- plot_grid(ann_row, ann_grid, ncol =1, rel_heights = c(1,10))

ph_grid <- compareMatricesGen(my_data$data, my_data$data, my_data$data, my_data$data, n_row=4, order_rows = F, order_cols=F, collect_legend = T)

tall_ph_grid <- plot_grid(NULL, ph_grid, rel_heights = c(1, 10), ncol=1)

plot_grid(annotation_plots, tall_ph_grid, ncol=2, rel_widths = c(1,10))

# Create the annotation data.frame for the rows
annotation_row <- data.frame(Cluster = factor(paste("Cluster", my_data$cluster_IDs))) %>%
  magrittr::set_rownames(rownames(my_data$data))

# The number of cololurs to use
K <- length(unique(my_data$cluster_IDs))

# Create the annotation colours
annotation_colors <- list(Cluster = viridis::viridis(K) %>%
                      magrittr::set_names(paste("Cluster", sort(unique(my_data$cluster_IDs))))
)

cl1 <- kmeans(my_data$data, 5)
cm <- createSimilarityMat(matrix(cl1$cluster,nrow=1)) %>% 
                            set_rownames(paste0("Person_", 1:100)) %>% 
  set_colnames(paste0("Person_", 1:100))
ph3 <- pheatmap(cm,
         annotation_col = annotation_row,
         annotation_colors = annotation_colors,
         cluster_rows=F,cluster_cols=F,show_rownames = F, show_colnames = F)

ggplot() + ph3$gtable$grobs[[2]] +ph1$gtable$grobs[[2]]

plot_grid(ph1$gtable$grobs[[2]], NULL, ph1$gtable$grobs[[2]], NULL, rel_heights=c(1,0.1,1,0.1),ncol=1)

grid.gedit(ph1$gtable$grobs[[2]], gp = gpar(col="grey70"))

border_color = "grey60"

converted_annotation = convert_annotations(annotation_row, annotation_colors)
elem = draw_annotations(converted_annotation, 
                        border_color,
                        gaps_row, 
                        fontsize,
                        horizontal = F
                        )

res = gtable_add_grob(res, elem, t = 4, l = 2, clip = "off", name = "row_annotation")

elem = draw_annotation_names(annotation_row, fontsize, horizontal = F, hjust_col = hjust_col, vjust_col = vjust_col, angle_col = angle_col)
res = gtable_add_grob(res, elem, t = 5, l = 2, clip = "off", name = "row_annotation_names")




convert_annotations = function(annotation, annotation_colors){
  new = annotation
  for(i in 1:ncol(annotation)){
    a = annotation[, i]
    b = annotation_colors[[colnames(annotation)[i]]]
    if(is.character(a) | is.factor(a)){
      a = as.character(a)
      
      if(length(setdiff(setdiff(a, NA), names(b))) > 0){
        stop(sprintf("Factor levels on variable %s do not match with annotation_colors", colnames(annotation)[i]))
      }
      new[, i] = b[a]
    }
    else{
      a = cut(a, breaks = 100)
      new[, i] = colorRampPalette(b)(100)[a]
    }
  }
  return(as.matrix(new))
}


ph3$gtable$layout$clip[2] <- "on"


ph3$gtable$grobs[[1]]

draw_annotations = function(converted_annotations, border_color, gaps, fontsize, horizontal){
  n = ncol(converted_annotations)
  m = nrow(converted_annotations)
  
  coord_x = find_coordinates(m, gaps)
  
  x = coord_x$coord - 0.5 * coord_x$size
  
  # y = cumsum(rep(fontsize, n)) - 4 + cumsum(rep(2, n))
  y = cumsum(rep(fontsize, n)) + cumsum(rep(2, n)) - fontsize / 2 + 1 
  y = unit(y, "bigpts")
  
  if(horizontal){
    coord = expand.grid(x = x, y = y)
    res = rectGrob(x = coord$x, y = coord$y, width = coord_x$size, height = unit(fontsize, "bigpts"), gp = gpar(fill = converted_annotations, col = border_color))
  }
  else{
    a = x
    x = unit(1, "npc") - y
    y = unit(1, "npc") - a
    
    coord = expand.grid(y = y, x = x)
    res = rectGrob(x = coord$x, y = coord$y, width = unit(fontsize, "bigpts"), height = coord_x$size, gp = gpar(fill = converted_annotations, col = border_color))
  }
  
  return(res)
}

draw_annotation_names = function(annotations, fontsize, horizontal, hjust_col, vjust_col, angle_col){
  n = ncol(annotations)
  
  x = unit(3, "bigpts")
  
  y = cumsum(rep(fontsize, n)) + cumsum(rep(2, n)) - fontsize / 2 + 1 
  y = unit(y, "bigpts")
  
  if(horizontal){
    res = textGrob(colnames(annotations), x = x, y = y, hjust = 0, gp = gpar(fontsize = fontsize, fontface = 2))
  }
  else{
    a = x
    x = unit(1, "npc") - y
    y = unit(1, "npc") - a
    
    res = textGrob(colnames(annotations), x = x, y = y, vjust = vjust_col, hjust = hjust_col, rot = angle_col, gp = gpar(fontsize = fontsize, fontface = 2))
  }
  
  return(res)
}

draw_annotation_legend = function(annotation, annotation_colors, border_color, ...){
  y = unit(1, "npc")
  text_height = unit(1, "grobheight", textGrob("FGH", gp = gpar(...)))
  
  res = gList()
  for(i in names(annotation)){
    res[[i]] = textGrob(i, x = 0, y = y, vjust = 1, hjust = 0, gp = gpar(fontface = "bold", ...))
    
    y = y - 1.5 * text_height
    if(is.character(annotation[[i]]) | is.factor(annotation[[i]])){
      n = length(annotation_colors[[i]])
      yy = y - (1:n - 1) * 2 * text_height
      
      res[[paste(i, "r")]] = rectGrob(x = unit(0, "npc"), y = yy, hjust = 0, vjust = 1, height = 2 * text_height, width = 2 * text_height, gp = gpar(col = border_color, fill = annotation_colors[[i]]))
      res[[paste(i, "t")]] = textGrob(names(annotation_colors[[i]]), x = text_height * 2.4, y = yy - text_height, hjust = 0, vjust = 0.5, gp = gpar(...))
      
      y = y - n * 2 * text_height
      
    }
    else{
      yy = y - 8 * text_height + seq(0, 1, 0.25)[-1] * 8 * text_height
      h = 8 * text_height * 0.25
      
      res[[paste(i, "r")]] = rectGrob(x = unit(0, "npc"), y = yy, hjust = 0, vjust = 1, height = h, width = 2 * text_height, gp = gpar(col = NA, fill = colorRampPalette(annotation_colors[[i]])(4)))
      res[[paste(i, "r2")]] = rectGrob(x = unit(0, "npc"), y = y, hjust = 0, vjust = 1, height = 8 * text_height, width = 2 * text_height, gp = gpar(col = border_color, fill = NA))
      
      txt = rev(range(grid.pretty(range(annotation[[i]], na.rm = TRUE))))
      yy = y - c(1, 7) * text_height
      res[[paste(i, "t")]]  = textGrob(txt, x = text_height * 2.4, y = yy, hjust = 0, vjust = 0.5, gp = gpar(...))
      y = y - 8 * text_height
    }
    y = y - 1.5 * text_height
  }
  
  res = gTree(children = res)
  
  return(res)
}


find_coordinates = function(n, gaps, m = 1:n){
  if(length(gaps) == 0){
    return(list(coord = unit(m / n, "npc"), size = unit(1 / n, "npc") ))
  }
  
  if(max(gaps) > n){
    stop("Gaps do not match with matrix size")
  }
  
  size = (1 / n) * (unit(1, "npc") - length(gaps) * unit("4", "bigpts"))
  
  gaps2 = apply(sapply(gaps, function(gap, x){x > gap}, m), 1, sum) 
  coord = m * size + (gaps2 * unit("4", "bigpts"))
  
  return(list(coord = coord, size = size))
}
