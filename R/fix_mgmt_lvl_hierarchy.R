#' Fix management level hierarchy (L2/L3/L4)
#'
#' @param dat Data containing management level variables, which should be called
#'   "management_chain_level_2", "management_chain_level_3", and ""management_chain_level_4".
#'   Currently doesn't support management levels beyond that.
#'
#' @return Original data from with management chain variables fixed.
#' @export

fix_mgmt_lvl_hierarchy <- function(dat) {
  dat |>
    mutate(management_chain_level_2 = case_when(management_chain_level_2 == "Jeff Harmening" ~ management_chain_level_3,
                                                TRUE ~ management_chain_level_2)) |>
    mutate(management_chain_level_3 = case_when(management_chain_level_2 == management_chain_level_3 ~ management_chain_level_4,
                                                TRUE ~ management_chain_level_3)) |>
    mutate(management_chain_level_4 = case_when(management_chain_level_3 == management_chain_level_4 ~ NA_character_)
    )
}
