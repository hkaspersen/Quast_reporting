# Quast_reporting
R Script for reporting quast results on assemblies. The script utilizes 
the transposed_report.tsv file from quast to generate a few informative 
figures and an excel table with selected information.
The script creates a new folder called "results_DATE" in the specified 
directory.

libpath is the path to the library with extra packages not installed in 
the default library location on the HPC cluster.

The functions file is not needed as the functions have ben moved inside 
the quast_analysis_script.R.

## Usage: 
Rscript quast_analysis_script.R transposed_report.tsv output_dir libpath
