#' Recode race/ethnicity variable
#'
#' @param race_ethnicity (Character) Vector containing the raw race/ethnicity field from Workday.
#'
#' @export
recode_race_ethnicity <- function(race_ethnicity) {
  race_ethnicity <- factor(
    race_ethnicity,
    levels = c("White (Non Hispanic or Latino) (United States of America)",
               "American Indian or Alaskan native (Non Hispanic or Latino) (United States of America)",
               "Asian (Non Hispanic or Latino) (United States of America)",
               "Black or African American (Non Hispanic or Latino) (United States of America)",
               "Hispanic or Latino (United States of America)",
               "Native Hawaiian or other Pacific Islander (Non Hispanic or Latino) (United States of America)",
               "Two or more races (Non Hispanic or Latino) (United States of America)")
  )
  
  race_ethnicity <- forcats::fct_collapse(
    race_ethnicity,
    "Other" = c("American Indian or Alaskan native (Non Hispanic or Latino) (United States of America)",
                "Native Hawaiian or other Pacific Islander (Non Hispanic or Latino) (United States of America)",
                "Two or more races (Non Hispanic or Latino) (United States of America)")
  )
  
  race_ethnicity <- forcats::fct_relabel(race_ethnicity,
                                         .fun = function(x) str_extract(x, "^[A-Za-z]+"))
  
  race_ethnicity <- forcats::fct_explicit_na(race_ethnicity)
  race_ethnicity <- forcats::fct_infreq(race_ethnicity)
  
  return(race_ethnicity)
}
