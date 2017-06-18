#' normalization and scale
#'
#' normalizate a vector and scale it
#' @name normalit
#' @import magrittr
#' @import dplyr
#' @import caret
#' @param m a vector to be normalized
#' @param n_partition integer, the factor to be scaled
#' @return the normalized and scaled vector
#' @export
normalit<-function(m, n_partition){
  res = (m - min(m))/(max(m)-min(m))*n_partition
}

#' round integer
#'
#' round down integer function special for gbod which ignore the partition
#' @name special_floor
#' @param m a vector to be rounded
#' @param n_partition integer, the factor to be scaled
#' @return the rounded integer vector
#' @export
special_floor <- function(m, n_partition){
  res = floor(m)
  res[res==n_partition] <- n_partition - 1
  return(res)
}

#' gbod
#'
#' Grid-based anomaly detection base algorithm
#'
#' @name gbod
#' @import magrittr
#' @import dplyr
#' @import caret
#' @param df : Input Dataframe, contains only the columns to be analyzed and no missing values
#' @param n_partition : integer, decide how many parts will be separated on each attributes
#' @param outlier_percent : number, the outlier percentage of the total records.
#' @return dataframe with additional column: label, 0 means normal, 1 means outlier
#' @examples
#' gbod(df, n_partition=10, outlier_percent=1)
#' @export
gbod <- function(df, n_partition=10, outlier_percent=1){

  df_grouped <-
    df %>%
    apply(2, normalit, n_partition=n_partition) %>%
    apply(2, special_floor, n_partition=n_partition) %>%
    tbl_df %>%
    group_by_(.dots=names(.))

  grid_info <-
    df_grouped %>%
    summarise(count=n()) %>%
    arrange(count) %>%
    mutate(label=0) %>%
    ungroup

  n_outliers = floor(nrow(df)*outlier_percent/100.0)
  mySum = 0
  count = 1

  while(TRUE){

    mySum = mySum + grid_info$count[count]

    if(mySum > n_outliers){
      break
    }
    grid_info$label[count] = 1
    count = count+1
  }


  res <-
    grid_info %>%
    select(-count) %>%
    left_join(df_grouped, .)

  df$label <- res$label

  return(df)

}


#' gbod score
#'
#' calculate the outlier score for each record
#'
#' @name gbod_score
#' @param df : input dataframe without missing values
#' @param partition_range : vector with two elements, a range of partitions, for example c(5,8)
#' @param outlier_percent : number
#' @return dataframe with different partition result and score
#' @examples
#' gbod_score(df, partition_range=c(5,8), outlier_percent=1)
#' @export
gbod_score <- function(df, partition_range=c(5,8), outlier_percent=1){

  mySeq = seq(partition_range[1], partition_range[2], by = 1)

  beginCol = length(colnames(df))+1

  res <- df

  for (n_partition in mySeq){
    res[paste0('p_', n_partition)] <- gbod(df, n_partition = n_partition, outlier_percent = outlier_percent)$label
  }

  beginCol = length(colnames(df))+1
  endCol = length(colnames(res))

  res %<>% mutate(score=rowSums(.[beginCol:endCol]))

  return(res)
}
