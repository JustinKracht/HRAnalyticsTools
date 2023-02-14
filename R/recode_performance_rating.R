#' Recode performance rating
#'
#' @param performance_rating A character vector of performance ratings in all historical formats.
#'
#' @return A numeric vector of performance ratings on the 1-3 scale.
#' @export
recode_performance_rating <- function(performance_rating) {
  performance_rating <- performance_rating |>
    str_remove("[:punct:]+") |>
    recode(performance_rating,
           "Exceptional Contribution" = 3,
           "Very Successful Contribution" = 3,
           "Successful Contribution" = 2,
           "Inconsistent Contribution" = 2,
           "Unsatisfactory Contribution" = 1,
           "Lower than Most Peers" = 1,
           "Higher than Most Peers" = 3,
           "In Line with Most Peers" = 2,
           "1 What 1 How" = 1,
           "1 What 2 How" = 2,
           "2 What 1 How" = 2,
           "2 What 2 How" = 2,
           "3 What 1 How" = 2,
           "2 What 3 How" = 3,
           "3 What 2 How" = 3,
           "3 What 3 How" = 3,
           .default = NA_real_)

  return(performance_rating)
}
