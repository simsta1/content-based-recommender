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

#' Function returns top n recommendations.
#'
#' @param user_profiles dataframe containing the number of genres of movies of the ground truth and our recommendations.
#'
#' @return cleveland-plot with ground_truth of genres vs genres of our recommendations.
create_cleveland_plot <- function(user_profiles){
  # Plot cleveland plot with relevant difference percentage of prediced movies
  for (u_id in unique(user_profiles$user)){
    user_profile_select <- user_profiles[user_profiles$user == u_id, ] %>%
      select(-c(user)) 
    
    rownames(user_profile_select) <- user_profile_select$ground_truth 
    user_profile_select <- t(user_profile_select)
    user_profile_select <- head(user_profile_select, dim(user_profile_select)[[1]] - 1) %>%
      as.data.frame()
    user_profile_select <- create_recomm_truth_percentages(user_profile_select)
    
    # Plot
    p <- ggplot(user_profile_select) +
      geom_segment( aes(x=genre, xend=genre, y=ground_truth, yend=recommendation), color="grey") +
      geom_point( aes(x=genre, y=ground_truth), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
      geom_point( aes(x=genre, y=recommendation), color=rgb(0.7,0.2,0.1,0.5), size=3) +
      coord_flip()+
      xlab("") +
      ylab("Percentage recommended vs. watched genre") + 
      labs(title = paste("Recommendation vs Ground Truth for user", u_id)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border = element_blank(), 
                     axis.line = element_line())

    print(p)
    }
  }

#' Function which creates the ground truth and recommendation of genres as percentages.
#'
#' @param user_profile_select dataframe containing the number of genres of movies of the ground truth and our recommendations.
#'
#' @return user_profile_select with ground_truth of genres vs genres of our recommendations as percentages.
create_recomm_truth_percentages <- function(user_profile_select){
  # Plot cleveland plot with relevant difference percentage of prediced movies

  colnames(user_profile_select) <- c("ground_truth", "recommendation")
  
  user_profile_select$ground_truth <- user_profile_select$ground_truth / sum(user_profile_select$ground_truth) 
  user_profile_select$recommendation <- user_profile_select$recommendation / sum(user_profile_select$recommendation) 
  user_profile_select$genre <- rownames(user_profile_select)
  rownames(user_profile_select) <- NULL
  return(user_profile_select)
}

#' Function which creates the ground truth and recommendation of genres as percentages and calculates the MAE from it.
#'
#' @param user_profiles dataframe containing the number of genres of movies of the ground truth and our recommendations.
#'
#' @return user_profile_select with ground_truth of genres vs genres of our recommendations as percentages.
create_recomm_truth_MSE <- function(user_profiles){
  # Plot cleveland plot with relevant difference percentage of prediced movies
  
  squared_errors <- vector( "numeric" , length(unique(user_profiles$user)))
  user_idx = 1
  for (u_id in unique(user_profiles$user)){
    user_profile_select <- user_profiles[user_profiles$user == u_id, ] %>%
      select(-c(user)) 
    
    rownames(user_profile_select) <- user_profile_select$ground_truth 
    user_profile_select <- t(user_profile_select)
    user_profile_select <- head(user_profile_select, dim(user_profile_select)[[1]] - 1) %>%
      as.data.frame()
    user_profile_select <- create_recomm_truth_percentages(user_profile_select)
    squared_errors[user_idx] <- MSE(user_profile_select$ground_truth, user_profile_select$recommendation)
    user_idx = user_idx + 1
    
  }
  squared_errors <- data.frame(User = unique(user_profiles$user), MSE = squared_errors)
  return(squared_errors)
}


squared_errors <- vector( "numeric" , length(unique(user_profiles$user)))
user_idx = 1
for (u_id in unique(user_profiles$user)){
  user_profile_select <- user_profiles[user_profiles$user == u_id, ] %>%
    select(-c(user)) 
  
  rownames(user_profile_select) <- user_profile_select$ground_truth 
  user_profile_select <- t(user_profile_select)
  user_profile_select <- head(user_profile_select, dim(user_profile_select)[[1]] - 1) %>%
    as.data.frame()
  user_profile_select <- create_recomm_truth_percentages(user_profile_select)
  squared_errors[user_idx] <- MAE(user_profile_select$ground_truth, user_profile_select$recommendation)
  user_idx = user_idx + 1
  
}

#' Function returns top n recommendations.
#'
#' @param selected_users melted similarity matrix of selected users
#' @param n n for the Top n list
#'
#' @return cleveland-plot with ground_truth of genres vs genres of our recommendations for the selected users.
#' 
analyze_topn_recos <- function(selected_users, n){
  # Extract recommended user profiles
  top_n_recos = get_topn_recos(selected_users, n)
  
  colnames(top_n_recos) <- c("title", "user", "similarity")
  top_n_recos$title <- as.character(top_n_recos$title)
  
  top_n_recos <- merge(x = top_n_recos, y = movie_genre, 
                       by.x = "title", by.y = "title") %>%
    select(-c(title, similarity))
  
  selected_cols <- top_n_recos %>%
    select(-user) %>%
    colnames()
  
  recommended_user_profiles <- top_n_recos %>%
    group_by(user) %>%
    summarise(across(everything(), sum))
  recommended_user_profiles$ground_truth <- FALSE
  
  
  # Extract true user profiles
  tmp <- user_movie_mat %*% movie_genre_mat
  tmp <- as.data.frame(tmp)
  tmp$user <- rownames(tmp)
  
  # filter users
  true_user_profiles <- tmp[tmp$user %in% c(recommended_user_profiles$user), ]
  true_user_profiles$ground_truth <- TRUE
  true_user_profiles
  
  
  user_profiles <- rbind(true_user_profiles, recommended_user_profiles)
  return(user_profiles)
}