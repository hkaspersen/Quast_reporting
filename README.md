# Information
R Script for reporting quast results on assemblies. The script utilizes 
the transposed_report.tsv file from quast to generate a few informative 
figures and an excel table with selected information.
The script creates a new folder called "results_DATE" in the specified 
directory.

The functions file is not needed as the functions have been moved inside 
the quast_analysis_script.R.

# Dependencies
The script relies on the following packages being installed:

dplyr

ggplot2

tidyr

svglite

# Usage: 
Rscript quast_analysis_script.R transposed_report.tsv output_dir
