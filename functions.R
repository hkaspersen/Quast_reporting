data_wrangle <- function(df) {
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

create_boxplot_reports <- function(df) {
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

filter_cutoffs <- function(x) {
  max <- max(x)
  third <- quantile(x, 0.75)[[1]]
  min <- min(x)
  
  dist_max <- abs(third - max)
  dist_min <- abs(third - min)
  
  df <- c()
  
  if (dist_max > dist_min) {
    df <- append(df, third)
  }
  
  return(df)
}