#' Compute average piece-wise b-star for given genome regions
#'
#' This function compute the average piece-wise b-star for a given genome regions within a given ethnicity group.
#' 
#' @param pop_list  A vector containing populations for analysis. 
#' @param chr_id    A numeric number identifying the ID for the target autosomal chromosome.
#' @param pos_sta   A numeric number marking the start position for the genome region.
#' @param pos_end   A numeric number marking the end position for the genome region.
#' 
#' @return The merged dataset of population-specific B-score for a given genome region.
#' 
#' @export
b_star_regional_mean <- function(pop_list, chr_id, pos_sta, pos_end){
  dat_out <- data.frame()
  for(pop in pop_list){
    path_in <- paste('~/Desktop/Lab_Essay/AkeyLab/BGS/data/B_2000/',pop,'/b_hg38_',pop,'_2000_chr',chr_id,'.bed',sep='')
    dat_in <- read.table(path_in, header=FALSE, sep='\t',
                         col.names=c('chr','Start','End','B'))
    dat_temp <- subset(dat_in, (dat_in$Start >= pos_sta) & (dat_in$End <= pos_end))
    row_temp <- data.frame(Chr   = paste('chr',chr_id, sep=''),
                           Start = pos_sta,
                           End   = pos_end,
                           B     = mean(dat_temp$B),
                           Pop   = pop)
    dat_out <- rbind(dat_out, row_temp)
  }
  colnames(dat_out) <- c('Chromosome','Start','End','B','Population')
  return(dat_out)
}
  
  
  
  




