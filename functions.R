check_dir <- function(output_dir) {
  folder <- paste0("results_", Sys.Date())
  dir.create(file.path(output_dir, folder), showWarnings = FALSE)
}

create_report <- function(df) {
  df <- df %>%
    rename(ctg_0_bp = X..contigs.....0.bp.,
           ctg_1000_bp = X..contigs.....1000.bp.,
           ctg_5000_bp = X..contigs.....5000.bp.,
           ctg_10000_bp = X..contigs.....10000.bp.,
           ctg_25000_bp = X..contigs.....25000.bp.,
           ctg_50000_bp = X..contigs.....50000.bp.,
           tot_len_0_bp = Total.length.....0.bp.,
           tot_len_1000_bp = Total.length.....1000.bp.,
           tot_len_5000_bp = Total.length.....5000.bp.,
           tot_len_10000_bp = Total.length.....10000.bp.,
           tot_len_25000_bp = Total.length.....25000.bp.,
           tot_len_50000_bp = Total.length.....50000.bp.,
           no_of_contigs = X..contigs,
           no_of_misassemblies = X..misassemblies,
           no_of_misassembled_ctg = X..misassembled.contigs,
           no_of_local_misassemblies = X..local.misassemblies,
           unaligned_contigs = X..unaligned.contigs,
           N_per_100kbp = X..N.s.per.100.kbp,
           mismatches_per_100kbp = X..mismatches.per.100.kbp,
           indels_per_100kbp = X..indels.per.100.kbp,
           no_of_genes = X..genes) %>%
    mutate(Assembly = gsub("(.*?)_.+", "\\1", Assembly))
  return(df)
}

create_boxplot_report <- function(df) {
  df <- df %>%
    gather(key_contig,
           value_contig,
           ctg_0_bp,
           ctg_1000_bp,
           ctg_5000_bp,
           ctg_10000_bp,
           ctg_25000_bp,
           ctg_50000_bp) %>%
    gather(key_total,
           value_total,
           tot_len_0_bp,
           tot_len_1000_bp, 
           tot_len_5000_bp,
           tot_len_10000_bp,
           tot_len_25000_bp,
           tot_len_50000_bp) %>%
    gather(key_ng,
           value_ng,
           N50,
           NG50)
  return(df)
}

## Plots

create_plots <- function(df) {
  cols <- c("#e7f0fa","#c9e2f6",
            "#95cbee","#0099dc",
            "#4ab04a", "#ffd73e",
            "#eec73a","#e29421",
            "#f05336","#ce472e")
  
  p1 <- ggplot(df, aes(Assembly, no_of_contigs))+
    geom_col(color = "black")+
    theme_classic()+
    labs(y = "# of contigs")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
          axis.title.x = element_blank())
  
  p2 <- ggplot(df, aes(Assembly, no_of_misassemblies))+
    geom_col(color = "black")+
    theme_classic()+
    labs(title = "Misassembled contigs",
         y = "# of contigs")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
          axis.title.x = element_blank())
  
  p3 <- ggplot(df, aes(Assembly, N_per_100kbp))+
    geom_col(color = "black")+
    theme_classic()+
    labs(title = "Ns per 100 kbp",
         y = "# of Ns")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
          axis.title.x = element_blank())
  
  p4 <- ggplot(df, aes(N50, Largest.contig, fill = no_of_contigs))+
    geom_point(pch = 21,
               size = 3)+
    scale_fill_gradientn(colors=cols,na.value="black",
                         limits=c(0,500),
                         guide=guide_colourbar(ticks=T,nbin=50,
                                               barheight=.5,label=T, 
                                               barwidth=10))+
    theme_classic()+
    labs(y = "Largest Contig",
         fill = "Number of Contigs")+
    theme(legend.position = "bottom")
  
  ggsave(paste0(output_dir,"/assembly_contigs.tiff"),
         p1,
         dpi = 100,
         units = "cm",
         device = "tiff",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/assembly_misassemblies.tiff"),
         p2,
         dpi = 100,
         units = "cm",
         device = "tiff",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/assembly_Ns.tiff"),
         p3,
         dpi = 100,
         units = "cm",
         device = "tiff",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/N50_largest_contigs.tiff"),
         p4,
         dpi = 100,
         units = "cm",
         device = "tiff",
         height = 20,
         width = 25)
}

create_boxplots <- function(df) {
  p1 <- ggplot(df, aes(key_contig, value_contig))+
    geom_boxplot()+
    theme_classic()+
    labs(y = "# of contigs")+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 <- ggplot(df, aes(key_total, value_total))+
    geom_boxplot()+
    theme_classic()+
    labs(y = "Total size of contigs")+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  p3 <- ggplot(df, aes(key_ng, value_ng))+
    geom_boxplot()+
    theme_classic()+
    labs(y = "# of contigs")+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(paste0(output_dir,"/no_of_contigs_per_size.tiff"),
         p1,
         dpi = 100,
         units = "cm",
         device = "tiff",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/assembly_contig_sizes.tiff"),
         p2,
         dpi = 100,
         units = "cm",
         device = "tiff",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/N50_NG50.tiff"),
         p3,
         dpi = 100,
         units = "cm",
         device = "tiff",
         height = 20,
         width = 25)
}


# Data
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
  
  write.xlsx(df, paste0(output_dir, "/selected_results.xlsx"))
}

