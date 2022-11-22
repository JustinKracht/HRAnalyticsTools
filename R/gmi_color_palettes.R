# General Mills custom color palettes
#
# Author: Justin Kracht
# Email: justin.kracht@genmills.com
# Date: 2021-09-16
#
# Description:
#   A variety of color palettes to use with ggplot2 in the GMI corporate colors.
#
# Notes:
#   Inspired by https://gmi.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# GMI corporate colors
gmi_colors <- c(
  "teal" = "#00ADB4",
  "orange" = "#F7941D",
  "light_blue" = "#009ADA",
  "light_green" = "#71BF48",
  "dark_blue" = "#0756A5",
  "red" = "#E22028",
  "grey" = "grey90"
)

#' Function to extract gmi colors as hex codes
#'
#' @param ... Character names of gmi_colors
#' @export
gmi_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (gmi_colors)

  gmi_colors[cols]
}

gmi_palettes <- list(
  "main"  = gmi_cols("light_blue", "dark_blue", "light_green", "teal", "orange"),
  "cool"  = gmi_cols("light_blue", "light_green"),
  "blues" = gmi_cols("dark_blue", "light_blue", "grey20"),
  "divergent" = gmi_cols("orange", "grey", "dark_blue")
)

#' Return function to interpolate a gmi color palette
#'
#' @param palette Character name of palette in gmi_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @export
gmi_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- gmi_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal, ...)
}

#' Color scale constructor for gmi colors
#'
#' @param palette Character name of palette in gmi_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
#' @examples
#' ggplot2::ggplot(iris,
#'                 aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
#'   geom_point() +
#'   scale_color_gmi()
scale_color_gmi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- gmi_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("gmi_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for gmi colors
#'
#' @param palette Character name of palette in gmi_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#' @examples
#' ggplot2::ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   scale_fill_gmi()
scale_fill_gmi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- gmi_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("gmi_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
