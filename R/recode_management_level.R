#' Recode management level, optionally collapsing officer groups into a single category
#'
#' @param management_level Character vector of management levels.
#' @param collapse_officer (Logical) Collapse VP, Senior VP, SLT, and CEO into a single group? Default is TRUE.
#'
#' @export
recode_management_level <- function(management_level, collapse_officer = TRUE) {
  management_level <- forcats::fct_relevel(
    management_level,
    "Support",
    "Specialized Support",
    "Experienced Support",

    "Professional",
    "Experienced Professional",
    "Senior Professional",

    "Associate Manager/Associate Expert",
    "Manager/Expert",

    "Director",

    "Vice President",
    "Senior Vice President",
    "Senior Leadership Team",
    "Chief Executive Officer"
  )

  if (collapse_officer) {
    management_level <- forcats::fct_collapse(
      management_level,
      "Officer" = c("Vice President",
                    "Senior Vice President",
                    "Senior Leadership Team",
                    "Chief Executive Officer")
    )
  }

  return(management_level)
}

recode_management_level_group <- function(management_level_group) {
  management_level_group <- forcats::fct_relevel(
    management_level_group,
    "Support",
    "Professional",
    "Manager",
    "Director",
    "Officer"
  )

  return(management_level_group)
}
