#' Basic GMI theme
#'
#' @param base_size Base text size.
#' @param base_family Base text family.
#' @param base_line_size Base line size.
#' @param base_rect_size Base rectangle size.
#'
#' @export
theme_gmi <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
                       base_rect_size = base_size/22) {
  theme_grey(base_size = base_size, base_family = base_family,
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "white",
                                          colour = NA),
          panel.border = element_rect(fill = NA,
                                      colour = "grey80"),
          axis.ticks = element_line(color = "grey80"),
          text = element_text(family = "Segoe UI"),
          axis.title.y = element_text(margin = margin(r = "10", unit = "pt"), angle = 90),
          axis.title.x = element_text(margin = margin(t = "10", unit = "pt")),
          plot.caption = element_text(face = "italic", hjust = 1, size = 8),
          panel.grid = element_line(colour = "grey92"),
          panel.grid.minor = element_line(size = rel(0.5)),
          strip.background = element_rect(fill = "grey85",
                                          colour = "grey20"),
          legend.key = element_rect(fill = "white",
                                    colour = NA), complete = TRUE)
}
