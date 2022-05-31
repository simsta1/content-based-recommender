library("reshape2")
library("recommenderlab")
library("dplyr")
library("tidyr")
library("ggplot2")
library("vegan")
library("coop")
library("bench")
library("gridExtra")
library("fmsb")




#' turn_into_wide
#' Turns a long dataframe into wide dataframe
#'
#' @param df 
#'
#' @return df in wide format
turn_into_wide <- function(df){
  df$value <- 1  
  df <- tidyr::pivot_wider(data = df, names_from = "combinations", values_from = "value")
  
  return(df)
}


#' Plots all user profiles
#'
#' @param M Matrix consisting of user genre ratings
#' @param user_idx Plot specific users
#' @param sample_size Size of the sample to take
#' @param n_cols ncols used for plot
#'
#' @return Plot
plot_user_profiles <- function(M, user_idx = NULL, 
                               sample_size = 20, n_cols = 4){
  
  if (user_idx){
    user_selection <- M[user_idx, ]
  } else {
    random_idx <- sample(seq(1, dim(M)[[1]]), 
                         size = sample_size)
    user_selection <- M[random_idx, ]
  }  
  user_selection <- as.data.frame(user_selection)
  user_selection <- rbind(rep(max(user_selection), 
                              nrow(user_selection)), 
                          rep(min(user_selection),
                              nrow(user_selection)), 
                          user_selection)
  n_rows <- as.integer(sample_size / n_cols)
  par(mar = rep(1, 4))
  par(mfrow = c(n_rows, 4))
  
  for (i in 3:nrow(user_selection)){
    fmsb::radarchart(
      df  = user_selection[c(1:2, i), ],  
      pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5),
      plwd = 1, cglcol="grey", cglty=1, axislabcol="grey",
      caxislabels=seq(0,20,5), cglwd=0.8,
      vlcex=0.8, title = paste("User: ", row.names(user_selection)[i]))
  }
}


calc_cosine_similarity <- function(UG, MG, make_sparse=FALSE){
  if (make_sparse){
    UG <- Matrix(UG, sparse = TRUE)
    MG <- Matrix(MG, sparse = TRUE) 
  }
  M <- UG %*% t(MG) / ( sqrt(rowSums(UG**2)) %*% t(sqrt(rowSums(MG**2))) )
  return(M)
}


#' Function returns top n recommendations.
#'
#' @param df dataframe
#' @param n top-n recommendations
#'
#' @return recommendations
get_topn_recos <- function(df,n){
  df1 <- df[order(df$value, decreasing = TRUE), ]  # Order data descending
  df11 <- Reduce(rbind,
                 by(df1,
                    df1["Var2"],
                    head,
                    n = n))
  return(df11)
}



