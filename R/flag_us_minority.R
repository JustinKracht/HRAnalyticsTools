#' Flag U.S. Minority
#'
#' @param race_ethnicity Character vector containing the employee race/ethnicity data.
#'
#' @export
flag_us_minority <- function(race_ethnicity) {

  us_minority <- c("American Indian or Alaskan native (Non Hispanic or Latino) (United States of America)",
                   "Asian (Non Hispanic or Latino) (United States of America)",
                   "Black or African American (Non Hispanic or Latino) (United States of America)",
                   "Hispanic or Latino (United States of America)",
                   "Native Hawaiian or other Pacific Islander (Non Hispanic or Latino) (United States of America)",
                   "Two or more races (Non Hispanic or Latino) (United States of America)")
  us_non_minority <- "White (Non Hispanic or Latino) (United States of America)"

  us_minority_flag <- rep(NA, length = length(race_ethnicity))

  us_minority_flag[race_ethnicity %in% us_minority] <- TRUE
  us_minority_flag[race_ethnicity == us_non_minority] <- FALSE

  return(us_minority_flag)
}
