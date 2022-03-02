## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
#install packages
library("reshape2")
library("recommenderlab")
library("knitr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("vegan")
library("coop")
library("bench")
library("gridExtra")
library("renv")
library("testthat")
library("knitr")


#renv::snapshot()
#renv::restore()



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("MovieLense")
MovieLense


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
########### Param ##########
MINIMUM_RATING = 1
##############################

MovieLense_binary <- recommenderlab::binarize(x = MovieLense, minRating = MINIMUM_RATING)

# Test
check_binary_matrix <- test_that(
  "Check binary matrix", {
    expect_equal(class(as(MovieLense_binary, "matrix")[1]), "logical")
  }
)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(MovieLense_binary)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
movie_genre <- MovieLenseMeta %>%
  dplyr::select(-c(url, year))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(movie_genre)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmp <- movie_genre %>%
  dplyr::select(-title) %>%
  rowSums()

hist(tmp, xlim=c(0,10), 
     col="lightblue", xlab="Number of Genres", 
     ylab="Count", main="Distribution of Ratings per Movie" )


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmp <- tidyr::pivot_longer(data = movie_genre, cols = 2:dim(movie_genre)[[2]], names_to = "genre")

tmp <- tmp %>%
  dplyr::filter(value > 0) %>%
  dplyr::group_by(genre) %>%
  dplyr::count() %>%
  dplyr::arrange(n)

tmp$genre <- factor(tmp$genre, levels = tmp$genre)

ggplot2::ggplot(tmp, ggplot2::aes(x = n, y = genre, label = n)) +
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = genre, xend = n, yend = genre), color = "grey50") +
  ggplot2::geom_point(size=3, color = "lightblue") +
  ggplot2::geom_text(nudge_x =25) +
  ggplot2::theme_bw() + 
  ggplot2::xlab("Counts")



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
########## Param ##########
NORMALIZED_BY_VIEWS <- TRUE

###########################
# Convert both into matrices
user_movie_mat <- as(MovieLense_binary, "matrix")


movie_genre_mat <- movie_genre %>%
  dplyr::select(-title) %>%
  as.matrix()

# Create user genre profile
if (NORMALIZED_BY_VIEWS){
  user_genre_mat <- ((user_movie_mat / as.vector(rowSums(user_movie_mat)))  %*% movie_genre_mat)
} else {
  user_genre_mat <- user_movie_mat %*% movie_genre_mat
}


# Test
check_both_matrices <- test_that(
  "Check matrices types", {
    expect_equal(class(user_movie_mat), class(movie_genre_mat))
  }
)

check_mat_dim <- test_that(
  "Check mat dims", {
    expect_equal(dim(user_movie_mat)[2], dim(movie_genre)[1])
  }
)

check_mat_dim_out <- test_that(
  "Check mat dims output", {
    expect_equal(dim(user_genre_mat), c(dim(user_movie_mat)[1], dim(movie_genre_mat)[2]))
  }
)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(user_genre_mat)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------




