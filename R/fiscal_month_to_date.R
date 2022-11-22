#' Convert fiscal month to year-month-day date format
#'
#' @param fiscal_month (character) Date in fiscal month format (e.g., "FY22 - Apr").
#' @param day (character) Which day to use in date? If NULL, defaults to last day of the month.
#'
#' @return (date) lubridate date object in "yyyy-mm-dd" format.
#' @export
fiscal_month_to_date <- function(fiscal_month, day = NULL) {
  month_part <- stringr::str_extract(fiscal_month, "[A-Za-z]{3,4}$")
  year_part <- paste0("20", stringr::str_extract(fiscal_month, "[0-9]{2}"))

  year_month <- paste(year_part, month_part, "01", sep = "-") |> lubridate::ymd()
  year_month <- dplyr::case_when(lubridate::month(year_month) >= 6 ~ year_month %m-% years(1),
                                 TRUE ~ year_month) |>
    lubridate::as_date()

  if (is.null(day)) {
    year_month <- lubridate::ceiling_date(year_month, "month") %m-% days(1)
  } else {
    year_month <- year_month %m+% days(as.numeric(day) - 1)
  }

  return(year_month)
}

#' Fiscal quarter to date
#'
#' @param fiscal_quarter (character) Date in fiscal quarter format (e.g., "FY22 - Apr").
#' @param use_quarter_end (character) Use the last day of the quarter? Default is TRUE, set to FALSE to use the first day of the quarter.
#'
#' @return (date) lubridate date object in "yyyy-mm-dd" format.
#' @export
fiscal_quarter_to_date <- function(fiscal_quarter, use_quarter_end = TRUE) {
  quarter_part <- stringr::str_extract(fiscal_quarter, "[0-9]$")
  year_part <- paste0("20", stringr::str_extract(fiscal_quarter, "[0-9]{2}"))

  quarter_end <- dplyr::case_when(quarter_part == "1" ~ "08",
                                  quarter_part == "2" ~ "11",
                                  quarter_part == "3" ~ "02",
                                  quarter_part == "4" ~ "05")

  quarter_start <- dplyr::case_when(quarter_part == "1" ~ "06",
                                    quarter_part == "2" ~ "09",
                                    quarter_part == "3" ~ "12",
                                    quarter_part == "4" ~ "03")

  month_part <- dplyr::case_when(use_quarter_end ~ quarter_end,
                                 !use_quarter_end ~ quarter_start)

  year_month <- paste(year_part, month_part, "01", sep = "-") |> lubridate::ymd()
  year_month <- case_when(as.numeric(quarter_start) >= 6 ~ year_month %m-% years(1),
                          TRUE ~ year_month) |>
    lubridate::as_date()

  year_month <- dplyr::case_when(use_quarter_end ~ lubridate::ceiling_date(year_month, "month") %m-% days(1),
                                 TRUE ~ lubridate::floor_date(year_month, "month"))

  return(year_month)
}
