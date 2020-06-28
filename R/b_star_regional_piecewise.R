#' Extract piece-wise b-star as a vector within given genome regions
#'
#' This function extracts all piece-wise b-star for a given genome regions within a given ethnicity group.
#' 
#' @param pop_list  A vector containing populations for analysis. 
#' @param chr_id    A numeric number identifying the ID for the target autosomal chromosome.
#' @param pos_sta   A numeric number marking the start position for the genome region.
#' @param pos_end   A numeric number marking the end position for the genome region.
#' 
#' @return A dataset of population-specific B-score for a given genome region as a vector for each population group.
#' 
#' @export
b_star_regional_piecewise <- function(pop_list, chr_id, pos_sta, pos_end){
  dat_out <- data.frame()
  dat_in <- b_star_regional(pop_list, chr_id, pos_sta, pos_end)
  for(i in 1:length(pop_list)){
    if(i == 1){
      temp <- as.factor(unlist(strsplit(as.character(dat_in$B[i]), ', ')))
      temp <- as.numeric(levels(temp))[temp]
      dat_out <- data.frame(value=temp)
      colnames(dat_out) <- pop_list[i]
    }
    else{
      temp <- as.factor(unlist(strsplit(as.character(dat_in$B[i]), ', ')))
      temp <- as.numeric(levels(temp))[temp]
      dat_out_col <- data.frame(value=temp)
      colnames(dat_out_col) <- pop_list[i]
      dat_out <- cbind(dat_out, dat_out_col)
    }
  }
  return(dat_out)
}
  
  
  
  




