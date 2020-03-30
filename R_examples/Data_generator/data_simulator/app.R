#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(pheatmap)
library(ggplot2)
library(viridis)
library(magrittr)
# library(mdiHelpR)
library(ggfortify)
library(tidyr)
library(tibble)

# define our ggplot2 theme of choice
theme_set(theme_bw() +
  theme(strip.background = element_rect(fill = "#21677e")) +
  theme(strip.text = element_text(colour = "white")))

defineBreaks <- function(col_pal, lb = -1, ub = 1, mid_point = 0.5 * (lb + ub)) {
  palette_length <- length(col_pal)

  breaks <- c(
    seq(lb, mid_point, length.out = ceiling(palette_length / 2) + 1),
    seq(mid_point + 1 / palette_length, ub, length.out = floor(palette_length / 2))
  )
}

defineDataBreaks <- function(x, col_pal, mid_point = NULL) {
  if (is.null(col_pal)) {
    col_pal <- dataColPal(n)
  }
  lb <- min(x)
  ub <- max(x)

  if (is.null(mid_point)) {
    if (lb < 0 & ub > 0) {
      mid_point <- 0
    } else {
      mid_point <- 0.5 * (lb + ub)
    }
  }

  defineBreaks(col_pal, lb = lb, ub = ub, mid_point = mid_point)
}

#' @title Generate dataset
#' @description Generate a dataset based upon the cluster means
#' (assumes each feature is independent)
#' @param cluster_means A k-vector of cluster means defining the k clusters.
#' @param n The number of samples to generate in the entire dataset.
#' @param p The number of columns to generate in the dataset.
#' @param pi A k-vector of the expected proportion of points to be drawn from
#' each distribution.
#' @param row_names The row names of the generated dataset.
#' @param col_names The column names of the generated dataset.
generateDataset <- function(cluster_means, cluster_sds, n, p, pi,
                            row_names = paste0("Person_", 1:n),
                            col_names = paste0("Gene_", 1:p)) {

  # The number of distirbutions to sample from
  K <- length(cluster_means)

  # The membership vector for the n points
  cluster_IDs <- sample(K, n, replace = T, prob = pi)

  # The data matrix
  my_data <- matrix(nrow = n, ncol = p)

  # Iterate over each of the columns permuting the means associated with each
  # label.
  for (j in 1:p)
  {
    reordered_cluster_means <- sample(cluster_means)
    # reordered_cluster_sds <- sample(cluster_sds)

    # Draw n points from the K univariate Gaussians defined by the permuted means.
    for (i in 1:n) {
      my_data[i, j] <- rnorm(1,
        mean = reordered_cluster_means[cluster_IDs[i]],
        sd = cluster_sds
        # sd = reordered_cluster_sds[cluster_IDs[i]]
      )
    }
  }

  # Order based upon allocation label
  row_order <- order(cluster_IDs)

  # Assign rownames and column names
  rownames(my_data) <- row_names
  colnames(my_data) <- col_names

  # Return the data and the allocation labels
  list(
    data = my_data[row_order, ],
    cluster_IDs = cluster_IDs[row_order]
  )
}

# This is an idea for generating data
generateFullDataset <- function(K, n, p,
                                delta_mu = 1,
                                cluster_sd = 1,
                                pi_method = "even",
                                p_noisy = 0,
                                alpha = 2) {
  my_data <- list(
    data = NA,
    cluster_IDs = NA
  )

  # Generate some cluster means
  cluster_means <- seq(from = 0, to = (K - 1) * delta_mu, by = delta_mu) %>%
    scale(center = T, scale = F)

  if (delta_mu == 0) {
    cluster_means <- rep(0, K)
  }

  # Generate some cluster standard deviations
  # cluster_sds <- rep(cluster_sd, K)

  if (pi_method == "even") {
    pi <- rep(1, K)
  } else {
    pi <- rgamma(K, alpha)
    pi <- pi / sum(pi)
  }

  # Find the number of requested informative features
  p_signal <- p # max(0, (p - p_noisy))

  data_sd <- 1

  # Generate data
  if (p_signal > 0) {
    my_data <- generateDataset(cluster_means, cluster_sd, n, p_signal, pi)
    data_sd <- sd(my_data$data)
  }

  # If irrelevant features are desired, generate such data
  if (p_noisy > 0) {
    noisy_data <- lapply(1:p_noisy, function(x) {
      rnorm(n, sd = data_sd)
    }) %>%
      unlist() %>%
      matrix(ncol = p_noisy) %>%
      set_colnames(paste0("Noise_", 1:p_noisy))

    if (p_signal > 0) {
      my_data$data <- cbind(my_data$data, noisy_data)
    } else {
      my_data$data <- noisy_data %>%
        set_rownames(paste0("Person_", 1:n))

      my_data$cluster_IDs <- rep(1, n)
    }
  }

  my_data
}

# Badly named heatmapping function
plotData <- function(x, cluster_IDs,
                     col_pal = colorRampPalette(c("#146EB4", "white", "#FF9900"))(100),
                     my_breaks = defineDataBreaks(x, col_pal, mid_point = 0),
                     main = "gen_dataset",
                     ...) {
  anno_row <- data.frame(Cluster = factor(paste("Cluster", cluster_IDs))) %>%
    set_rownames(rownames(x))

  K <- length(unique(cluster_IDs))

  ann_colours <- list(Cluster = viridis(K) %>%
    set_names(paste("Cluster", sort(unique(cluster_IDs)))))

  ph <- pheatmap(x,
    color = col_pal,
    breaks = my_breaks,
    annotation_row = anno_row,
    annotation_colors = ann_colours,
    main = main,
    ...
  )

  ph
}


#!/usr/bin/env Rscript

#' @title PCA plot
#' @description Plot the items loadings along the principal components.
#' @param x A matrix of the loadings of each item upon each component (the "x"
#' part of the prcomp() output).
#' @param labels A vector of the labels associated with the rows of x.
#' @param n_comp The number of components to plot.
#' @return A ``ggplot2'' object.
#' @importFrom ggplot2 ggplot geom_point scale_color_viridis_d xlim aes
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer
#' @export
pcaPlot <- function(x, labels = NA, n_comp = 10) {
  if (all(is.na(labels)) & length(labels) == 1) {
    
    # Gather the data from wide to long format and convert to a data.frame ready
    # for ggplot
    plt_pca_data <- x %>%
      as.data.frame(row.names = row.names(.)) %>%
      tibble::add_column(Item = as.factor(row.names(.))) %>%
      tidyr::pivot_longer(-Item,
                          names_to = "Component",
                          values_to = "Loadings",
                          names_ptypes = list(Component = factor()),
                          values_ptypes = list(Loadings = numeric()),
      ) %>%
      suppressWarnings()
    
    
    # plot the data along the components
    p <- suppressWarnings(
      ggplot2::ggplot(plt_pca_data, ggplot2::aes(x = Component, y = Loadings)) +
        ggplot2::geom_point() +
        ggplot2::xlim(paste0("PC", 1:n_comp))
    )
    
  } else {
    
    # Gather the data from wide to long format and convert to a data.frame ready
    # for ggplot and include the labels as a factor
    plt_pca_data <- x %>%
      as.data.frame(row.names = row.names(.)) %>% 
      tibble::add_column(
        Cluster = as.factor(labels), 
        Item = as.factor(row.names(.))
      ) %>%
      tidyr::pivot_longer(-c(Cluster, Item),
                          names_to = "Component",
                          values_to = "Loadings",
                          names_ptypes = list(Component = factor()),
                          values_ptypes = list(Loadings = numeric())
      ) 
    
    # plot the data along the components colouring by the labels
    p <- ggplot2::ggplot(
      plt_pca_data, 
      ggplot2::aes(x = Component, y = Loadings, colour = Cluster)
    ) +
      ggplot2::geom_point() +
      ggplot2::scale_color_viridis_d() +
      ggplot2::xlim(paste0("PC", 1:n_comp))
  }
  
  p
}

#' @title PCA series plot
#' @description Plot the median loadings for each cluster along the principal
#' components as a series, with an option to include a ribbon describing the 
#' range from the first ot the third quartile around this.
#' @param x A matrix of the loadings of each item upon each component (the "x"
#' part of the prcomp() output).
#' @param labels A vector of the labels associated with the rows of x.
#' @param n_comp The number of components to plot.
#' @param include_area Logical indicating inclusion of [quartile_1, quartile_3]
#' about the median value of the clusters across components.
#' @param ribbon_alpha The alpha used on the ribbon geom describing the range 
#' about the median of the clusters.
#' @param ribbon_lty The linetype used on the ribbon geom describing the range 
#' about the median of the clusters.
#' @return A ``ggplot2'' plot.
#' @importFrom ggplot2 ggplot geom_line scale_color_viridis_d xlim aes
#' @importFrom dplyr group_by summarise
#' @export
pcaSeriesPlot <- function(x, labels,
                          alpha = 0.8,
                          lty = 3,
                          n_comp = 10,
                          include_area = F,
                          ribbon_alpha = 0.2,
                          ribbon_lty = 4) {
  p1 <- pcaPlot(x, labels = labels, n_comp = n_comp)
  
  
  sum_data <- p1$data %>%
    dplyr::group_by(Cluster, Component) %>%
    dplyr::summarise(
      Median_value = median(Loadings),
      q1 = quantile(Loadings, probs = 0.25),
      q3 = quantile(Loadings, probs = 0.75),
      q0 = quantile(Loadings, probs = 0),
      q4 = quantile(Loadings, probs = 1)
    )
  
  
  p2 <- p1 +
    ggplot2::geom_line(ggplot2::aes(group = Item), alpha = alpha, lty = lty) +
    ggplot2::geom_line(
      data = sum_data,
      ggplot2::aes(x = Component, y = Median_value, group = Cluster),
      size = 1
    )
  
  if (include_area) {
    p2 <- p2 +
      scale_fill_viridis_d() +
      geom_ribbon(
        data = sum_data,
        aes(
          x = Component,
          y = Median_value,
          ymin = q1,
          ymax = q3,
          group = Cluster,
          fill = Cluster
        ),
        alpha = ribbon_alpha,
        lty = ribbon_lty,
        colour = NA
      )
  }
  
  p2
}

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Generated data for Gaussian mixture modelling"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("seed",
        "Random seed used to generate data:",
        min = 1,
        max = 1e5,
        value = 1,
        step = 1
      ),

      numericInput("N",
        "Number of samples:",
        min = 2,
        max = 10000,
        value = 100,
        step = 1
      ),

      numericInput("P",
        "Number of relevant features:",
        min = 0,
        max = 1000,
        value = 2,
        step = 1
      ),

      numericInput("phi",
        "Number of irrelevant features:",
        min = 0,
        max = 1000,
        value = 0,
        step = 1
      ),

      sliderInput("K",
        "Number of clusters:",
        min = 1,
        max = 50,
        value = 5,
        step = 1
      ),


      selectInput(
        inputId = "pi",
        label = "Choose how class proportions are defined:",
        choices = c("even", "varying (sampled from Dirichlet)")
      ),

      sliderInput("delta_mu",
        "Distance between means defining each cluster:",
        min = 0,
        max = 5,
        value = 1,
        step = 0.1
      ),

      sliderInput("Sigma",
        "Standard deviation for each feature:",
        min = 0.1,
        max = 5,
        value = 1,
        step = 0.1
      ),

      sliderInput("alpha",
        "Concentration parameter for the Dirichlet distribution (only used if class proportions are set to ``varying''):",
        min = 0.1,
        max = 5,
        value = 2,
        width = "400px",
        step = 0.05
      ),

      numericInput("PCa",
        "Principal component displayed in X-axis:",
        min = 1,
        max = 100,
        value = 1,
        step = 1
      ),

      numericInput("PCb",
        "Principal component displayed in Y-axis:",
        min = 1,
        max = 100,
        value = 2,
        step = 1
      ),
      
      numericInput("nComp",
                   "Number of prinicpal components displayed on x-axis:",
                   min = 1,
                   max = 100,
                   value = 2,
                   step = 1
      ),

      checkboxInput(
        "plotDensity",
        "Plot density of cluster members in PCA plot."
      ),
      
      checkboxInput(
        "clusterRows",
        "Cluster rows in heatmap."
      ),
      
      checkboxInput(
        "facetWrap",
        "Facet by cluster number in PCA series plot."
      )
      
    ),

    # Show a plot of the generated distribution
    mainPanel(

      # Plot first two PCs
      plotOutput("distPlot"),
      
      # Plot PCs
      plotOutput("pcaPlot"),

      # PLot annotated heatmap of data
      plotOutput("themap"),

      # Download data generated
      downloadButton("downloadData", "Download data")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # seed <- reactive({
  #   seed <- sample(1:1e5, size = 1)
  #   set.seed(seed)
  #   seed
  # })

  # set.seed(input$seed)

  # Generate the data and cluster IDs
  a <- reactive({
    set.seed(input$seed)
    my_data <- generateFullDataset(
      input$K,
      input$N,
      input$P,
      input$delta_mu,
      input$Sigma,
      input$pi,
      input$phi,
      input$alpha
    )
  })


  pca <- reactive({
    my_data <- a()
    
    pc_1 <- prcomp(my_data$data)
  }
  )
  
  
  # Create a plot of the first two PCs
  output$distPlot <- renderPlot({
    my_data <- a()

    pc_1 <- pca()
    # pc_1 <- prcomp(my_data$data)

    if (input$K < 10) {
      if (input$plotDensity) {
        autoplot(pc_1, data = my_data$data, x = input$PCa, y = input$PCb) + # , colour = my_data$cluster_IDs) +
          geom_point(aes(colour = as.factor(my_data$cluster_IDs))) +
          geom_density_2d(aes(colour = as.factor(my_data$cluster_IDs))) +
          labs(
            title = "PCA of generated data",
            subtitle = "Coloured by cluster IDs",
            colour = "Cluster"
          ) +
          scale_colour_viridis_d()
      } else {
        autoplot(pc_1, data = my_data$data, x = input$PCa, y = input$PCb) + # , colour = my_data$cluster_IDs) +
          geom_point(aes(colour = as.factor(my_data$cluster_IDs))) +
          labs(
            title = "PCA of generated data",
            subtitle = "Coloured by cluster IDs",
            colour = "Cluster"
          ) +
          scale_colour_viridis_d()
      }
    } else {
      if (input$plotDensity) {
        autoplot(pc_1, data = my_data$data, x = input$PCa, y = input$PCb) + # , colour = my_data$cluster_IDs) +
          geom_point(aes(colour = as.factor(my_data$cluster_IDs))) +
          geom_density_2d(aes(colour = as.factor(my_data$cluster_IDs))) +
          labs(
            title = "PCA of generated data",
            subtitle = "Coloured by cluster IDs",
            colour = "Cluster"
          ) +
          scale_colour_viridis_d() +
          theme(legend.position = "none")
      } else {
        autoplot(pc_1, data = my_data$data, x = input$PCa, y = input$PCb) + # , colour = my_data$cluster_IDs) +
          geom_point(aes(colour = as.factor(my_data$cluster_IDs))) +
          labs(
            title = "PCA of generated data",
            subtitle = "Coloured by cluster IDs",
            colour = "Cluster"
          ) +
          scale_colour_viridis_d() +
          theme(legend.position = "none")
      }
    }
  })

  output$pcaPlot <- renderPlot({
    
    pc_1 <- pca()
    my_data <- a()
    
    # oldw <- getOption("warn")
    # options(warn = -1)abc%123
    
    # pcaScatterPlot(pc_1$x, my_data$cluster_IDs, n_comp = input$nComp)
    pca_plot <- pcaSeriesPlot(pc_1$x, my_data$cluster_IDs,
                  n_comp = input$nComp,
                  include_area = T)
    
    if(input$facetWrap){
    # New facet label names for cluster variable
    cluster_labs <- c(paste0("Cluster ", 1:input$K))
    names(cluster_labs) <- 1:input$K
    
    # Create the plot
    pca_plot <- pca_plot +
      facet_wrap(~Cluster, labeller = labeller(Cluster = cluster_labs)) +
      # labs(title = "PCA plot including medians and inter-quartile range") + 
      theme(legend.position="none", strip.text.x = element_text(size = 12) )
    }
    
    pca_plot
    # options(warn = oldw)
  }
  )
  
  # Render a heatmap of the data
  output$themap <- renderPlot({
    my_data <- a()

    show_rownames <- T
    if (input$N > 50) {
      show_rownames <- F
    }

    show_colnames <- T
    if ((input$P + input$phi) > 50) {
      show_colnames <- F
    }


    annotation_legend <- T
    if(input$K > 20){
      annotation_legend <- F
    }
    
    # Heatmap the data, annotated by cluster ID
    ph <- plotData(my_data$data, my_data$cluster_IDs,
      cluster_rows = input$clusterRows,
      main = "Generated data",
      show_rownames = show_rownames,
      show_colnames = show_colnames,
      annotation_legend = annotation_legend,
      silent = F
    )
    
    

    # plot(ph$gtable)
  })

  # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = "generated_data.csv",
    content = function(file) {
      write.csv(a(), file, row.names = T)
    },
    contentType = "text/csv"
  )
}

# Run the application
shinyApp(ui = ui, server = server)
