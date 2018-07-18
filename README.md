# Quast_reporting
R Script for reporting quast results on assemblies. The script utilizes 
the transposed_report.tsv file from quast to generate a few informative 
figures and an excel table with selected information.

libpath is the path to the library with extra packages not installed in 
the default library location on the HPC cluster.

## Usage: 
Rscript quast_analysis_script.R transposed_report.tsv output_dir libpath
