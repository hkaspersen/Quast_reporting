library(tidyverse)

source("functions.R")

report_loc <- readline(prompt = "Please supply location of transposed_report.tsv (use '/' instead of '\'): ")


raw_report <- read.delim(paste0(report_loc,
                                "/transposed_report.tsv"),
                         sep = "\t",
                         header = TRUE,
                         stringsAsFactors = F)


clean_report <- data_wrangle(raw_report)
boxplot_report <- create_boxplot_reports(clean_report)

# Contig sizes
ggplot(boxplot_report, aes(key_contig, value_contig))+
  geom_boxplot()+
  theme_classic()+
  labs(y = "# of contigs")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Total sizes
ggplot(boxplot_report, aes(key_total, value_total))+
  geom_boxplot()+
  theme_classic()+
  labs(y = "# of contigs")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# N50
ggplot(boxplot_report, aes(key_ng, value_ng))+
  geom_boxplot()+
  theme_classic()+
  labs(y = "# of contigs")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Number of contigs per assembly
ggplot(clean_report, aes(Assembly, no_of_contigs))+
  geom_col(color = "black")+
  theme_classic()+
  labs(y = "# of contigs")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        axis.title.x = element_blank())

# Misassemlies
ggplot(clean_report, aes(Assembly, no_of_misassemblies))+
  geom_col(color = "black")+
  theme_classic()+
  labs(title = "Misassembled contigs",
       y = "# of contigs")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        axis.title.x = element_blank())

# N per 100 kbps
ggplot(clean_report, aes(Assembly, N_per_100kbp))+
  geom_col(color = "black")+
  theme_classic()+
  labs(title = "Ns per 100 kbp",
       y = "# of Ns")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        axis.title.x = element_blank())
