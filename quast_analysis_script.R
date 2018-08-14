#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

report_loc <- args[1]
output_dir <- args[2]

library(dplyr)
library(ggplot2)
library(tidyr)
library(svglite)
library(R.utils)

# Import functions
sourceDirectory("src/")

# Run functions
check_dir(output_dir)

output_dir <- paste0(output_dir, "/results_", Sys.Date())

raw_df <- read.delim(report_loc,
                         sep = "\t",
                         header = TRUE,
                         stringsAsFactors = F)

# Data wrangling
clean_df <- create_df(raw_df)
boxplot_df <- create_boxplot_df(clean_df)

# Plotting
create_plots(clean_df)
create_boxplots(boxplot_df)

# Data
save_data(clean_df)
