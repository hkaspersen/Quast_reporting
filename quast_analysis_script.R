#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

library(dplyr)
library(ggplot2)
library(tidyr)
library(svglite)
library(openxlsx)

source("functions.R")

report_loc <- args[1]
output_dir <- args[2]

check_dir(output_dir)
output_dir <- paste0(output_dir, "/results_", Sys.Date())

raw_report <- read.delim(paste0(report_loc,
                                "/transposed_report.tsv"),
                         sep = "\t",
                         header = TRUE,
                         stringsAsFactors = F)

# Data wrangling
clean_report <- create_report(raw_report)
boxplot_report <- create_boxplot_report(clean_report)

# Plotting
create_plots(clean_report)
create_boxplots(boxplot_report)

# Data
save_data(clean_report)
