# This script is the various libraries and theme function necessary for the rest of 
#the landcover classification codes to run
# Call this code via source() (info via ?source in console)
library(terra)
library(ggplot2)
library(ggpp)
library(ggtext)
library(ggpubr)
library(tidyverse)
library(cowplot)
library(stringr)
library(ggnewscale)
library(gsubfn)
theme_cust <- function(base_size = 11, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.text = element_text(color = "black")
    )
} #<- credit to Anna Bergstrom for this styling function

