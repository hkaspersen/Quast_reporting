# Creates a data frame for boxplots
create_boxplot_df <- function(df) {
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