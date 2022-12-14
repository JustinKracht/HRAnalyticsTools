% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gmi_color_palettes.R
\name{scale_color_gmi}
\alias{scale_color_gmi}
\title{Color scale constructor for gmi colors}
\usage{
scale_color_gmi(palette = "main", discrete = TRUE, reverse = FALSE, ...)
}
\arguments{
\item{palette}{Character name of palette in gmi_palettes}

\item{discrete}{Boolean indicating whether color aesthetic is discrete or not}

\item{reverse}{Boolean indicating whether the palette should be reversed}

\item{...}{Additional arguments passed to discrete_scale() or
scale_color_gradientn(), used respectively when discrete is TRUE or FALSE}
}
\description{
Color scale constructor for gmi colors
}
\examples{
ggplot2::ggplot(iris,
                aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
  geom_point() +
  scale_color_gmi()
}
