# Creates and saves plots from Quast data
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
  
  ggsave(paste0(output_dir,"/assembly_contigs.svg"),
         p1,
         dpi = 100,
         units = "cm",
         device = "svg",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/assembly_misassemblies.svg"),
         p2,
         dpi = 100,
         units = "cm",
         device = "svg",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/assembly_Ns.svg"),
         p3,
         dpi = 100,
         units = "cm",
         device = "svg",
         height = 20,
         width = 25)
  
  ggsave(paste0(output_dir,"/N50_largest_contigs.svg"),
         p4,
         dpi = 100,
         units = "cm",
         device = "svg",
         height = 20,
         width = 25)
}