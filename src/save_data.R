# Saves Quast data as a txt file
save_data <- function(df) {
  df <- df %>%
    select(Assembly,
           no_of_contigs,
           Largest.contig,
           Total.length,
           N50,
           GC....,
           no_of_misassemblies,
           N_per_100kbp,
           Largest.alignment) %>%
    rename(`GC%` = GC....,
           "Number of contigs" = no_of_contigs,
           "Largest contig" = Largest.contig,
           "Total length" = Total.length,
           "Number of misassemblies" = no_of_misassemblies,
           "N per 100 kbp" = N_per_100kbp,
           "Largest alignment" = Largest.alignment)
  
  write.table(df,
              paste0(output_dir,
                     "/selected_results.txt"),
              sep = "\t",
              row.names = F)
}