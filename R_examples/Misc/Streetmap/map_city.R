#! user/env/bin/Rscript
#
# This code creates a plot of a map of a given city

# === Libraries ================================================================

# Is ggplot2 sufficinet?
# library(tidyverse, quietly = T)
library(gpglot2, quietly = T)

# Mapping functions
library(osmdata, quietly = T)

# For command line arguments
library(optparse, quietly = T) # install.packages("optparse")

# === Functions ================================================================

# User inputs from command line
input_arguments <- function() {
  option_list <- list(

    # Name of city to map
    optparse::make_option(c("--city", "-c"),
      type = "character",
      default = NA,
      help = "Name of city to map [default= %default]",
      metavar = "character"
    ),

    # Plot map as black background or white (TRUE gives black)
    optparse::make_option(c("--black", "-b"),
      type = "logical",
      default = T,
      help = "Plot map as black background or white (TRUE gives black) [default= %default]",
      metavar = "logical"
    ),

    # Limits on x-coordinates to use
    optparse::make_option(c("--xlim", "-x"),
      type = "double",
      default = NA,
      help = "Limits on x-coordinates to use (defaults to the limits suggested by osmdata).",
      metavar = "double"
    ),

    # Limits on y-coordinates to use
    optparse::make_option(c("--ylim", "-y"),
      type = "double",
      default = NA,
      help = "Limits on y-coordinates to use (defaults to the limits suggested by osmdata).",
      metavar = "double"
    )
  )
  opt_parser <- optparse::OptionParser(option_list = option_list)
  opt <- optparse::parse_args(opt_parser)
}

# === Main =====================================================================

args <- input_arguments()

city_of_interest <- args$city
black <- args$black
x_lim <- args$xlim
y_lim <- args$ylim


# city_of_interest <- "Cambridge England"
# black <- TRUE
# city_of_interest <- "Wageningen Netherlands"

coords <- getbb(city_of_interest)

if (is.na(x_lim)) {
  x_lim <- coords[1, ]
}

if (is.na(y_lim)) {
  y_lim <- coords[2, ]
}

streets <- getbb(city_of_interest) %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway", "primary",
      "secondary", "tertiary"
    )
  ) %>%
  osmdata_sf()

small_streets <- getbb(city_of_interest) %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "residential", "living_street",
      "unclassified",
      "service", "footway"
    )
  ) %>%
  osmdata_sf()



river <- getbb(city_of_interest) %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

if (!black) {
  ggplot() +
    geom_sf(
      data = streets$osm_lines,
      inherit.aes = FALSE,
      color = "black",
      size = .4,
      alpha = .8
    ) +
    geom_sf(
      data = small_streets$osm_lines,
      inherit.aes = FALSE,
      color = "black",
      size = .4,
      alpha = .6
    ) +
    geom_sf(
      data = river$osm_lines,
      inherit.aes = FALSE,
      color = "black",
      size = .2,
      alpha = .5
    ) +
    coord_sf(
      xlim = x_lim,
      ylim = y_lim,
      expand = FALSE
    ) +
    theme_void()
} else {
  ggplot() +
    geom_sf(
      data = streets$osm_lines,
      inherit.aes = FALSE,
      color = "#7fc0ff",
      size = .4,
      alpha = .8
    ) +
    geom_sf(
      data = small_streets$osm_lines,
      inherit.aes = FALSE,
      color = "#ffbe7f",
      size = .2,
      alpha = .6
    ) +
    geom_sf(
      data = river$osm_lines,
      inherit.aes = FALSE,
      color = "#ffbe7f",
      size = .2,
      alpha = .5
    ) +
    coord_sf(
      xlim = x_lim,
      ylim = y_lim,
      expand = FALSE
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#282828")
    )
}

save_name <- stringr::str_replace_all(city_of_interest, " ", "_")
ggsave(paste0(save_name, "_map.png"), width = 6, height = 6)
# ggsave("wageningen_map.png", width = 6, height = 6)
