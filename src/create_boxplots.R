# Creates and saves boxplots
create_boxplots <- function(df) {
  p1 <- ggplot(df, aes(key_contig, value_contig))+
    stat_boxplot(geom = "errorbar", width = 0.6)+
    geom_boxplot()+
    theme_classic()+
    labs(y = "# of contigs")+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 <- ggplot(df, aes(key_total, value_total))+
    stat_boxplot(geom = "errorbar", width = 0.6)+
    geom_boxplot()+
    theme_classic()+
    labs(y = "Total size of contigs")+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  p3 <- ggplot(df, aes(key_ng, value_ng))+
    stat_boxplot(geom = "errorbar", width = 0.6)+
    geom_boxplot()+
    theme_classic()+
    labs(y = "# of contigs")+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(paste0(output_dir,"/no_of_contigs_per_size.svg"),
         p1,
         dpi = 100,
         units = "cm",
         device = "svg",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/assembly_contig_sizes.svg"),
         p2,
         dpi = 100,
         units = "cm",
         device = "svg",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/N50_NG50.svg"),
         p3,
         dpi = 100,
         units = "cm",
         device = "svg",
         height = 20,
         width = 25)
}