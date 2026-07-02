\name{theme_factominer}
\alias{theme_factominer}
\alias{scale_colour_factominer}
\alias{scale_fill_factominer}
\alias{scale_colour_factominer_contrib}
\alias{scale_fill_factominer_contrib}
\alias{scale_colour_factominer_cos2}
\alias{scale_fill_factominer_cos2}
\alias{scale_colour_factominer_coord}
\alias{scale_fill_factominer_coord}
\alias{scale_colour_factominer_pvalue}
\alias{scale_fill_factominer_pvalue}
\alias{geom_contrib_reference}
\alias{factominer_breaks_contrib}
\alias{factominer_breaks_cos2}
\alias{factominer_labels_pct}
\title{Theme for ggplot graphs and colour scales for FactoMineR}
\description{
  \code{theme_factominer} for the ggplot graphs of FactoMineR.
}
\usage{
theme_factominer(
    base_size         = 11,
    base_family       = "Inclusive Sans",
    title_family      = "Atkinson Hyperlegible Next",
    
    rel_title         = 1.5,
    rel_subtitle      = 1.05,
    rel_caption       = 0.78,
    rel_axis_title    = 0.9,
    rel_axis_text     = 0.85,
    rel_legend_title  = 0.88,
    rel_legend_text   = 0.85,
    rel_strip_text    = 0.9,
    
    # point_size        = 2.5,
    # label_size        = 3.5, 
    
    dark_text         = "#1C2B3A",
    mid_text          = "#4A5A6A",
    light_text        = "#7A8A9A",
    factominer_grid_col = "#E2E8F0"
)

scale_colour_factominer(...)
scale_fill_factominer(...)

scale_colour_factominer_contrib(name = "Contribution", limits = NULL, ...)
scale_fill_factominer_contrib(name = "Contribution", limits = NULL, ...)

scale_colour_factominer_cos2(name = "cos2", breaks = c(0, 0.25, 0.5, 0.75, 1), ...)
scale_fill_factominer_cos2(name = "cos2", breaks = c(0, 0.25, 0.5, 0.75, 1), ...)

scale_colour_factominer_coord(name = "Coordinate", midpoint = 0, ...)
scale_fill_factominer_coord(name = "Coordinate", midpoint = 0, ...)

scale_colour_factominer_pvalue(name = "p-valeur", threshold = 0.05, 
                               breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 1), ...)
scale_fill_factominer_pvalue(name = "p-valeur", threshold = 0.05, 
                             breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 1), ...)

geom_contrib_reference(n, ...)
factominer_breaks_contrib(max_val, n_breaks = 5)
factominer_breaks_cos2()
factominer_labels_pct(digits = 1)
}
\arguments{
\item{base_size}{Base font size (default: 11).}
\item{base_family}{Font family used for the main text (default: "Inclusive Sans"); choose in the google fonts}
\item{title_family}{Font family used for titles (default: "Atkinson Hyperlegible Next"); choose in the google fonts.}
\item{rel_title}{Relative size of title. Default 1.5}
\item{rel_subtitle}{Relative size of subtitle. Default 1.05}
\item{rel_caption}{Relative size of caption. Default 0.78}
\item{rel_axis_title}{Relative size of axis_title. Default 0.9}
\item{rel_axis_text}{Relative size of axis_text. Default 0.85}
\item{rel_legend_title}{Relative size of legend_title. Default 0.88}
\item{rel_legend_text}{Relative size of legend_text. Default 0.85}
\item{rel_strip_text}{Relative size of strip_text. Default 0.9}
\item{dark_text}{Color used for titles and dark text.}
\item{mid_text}{Color used for regular text and legends.}
\item{factominer_grid_col}{Color used for the grid.}
\item{light_text}{Color used for axes and secondary legends.}
\item{name}{Legend title.}
\item{limits}{Limits of the scale.}
\item{breaks}{Break points of the scale.}
\item{midpoint}{Midpoint value for the diverging coordinate scale.}
\item{threshold}{Significance threshold used to highlight p-values.}
\item{n}{Number of individuals or variables used to compute the contribution reference line.}
\item{max_val}{Maximum value used to compute scale breaks.}
\item{n_breaks}{Desired number of scale breaks.}
\item{digits}{Number of decimal places for percentages.}
\item{...}{Additional arguments passed to the underlying ggplot2 functions.}
}
\value{
  Object of class \code{theme} or scale structure for \code{ggplot2}.
}
\author{
  F. Husson
}
\examples{
data(decathlon)
res.pca <- PCA(decathlon,quali.sup=13,quanti.sup=11:12, graph=FALSE)
plot(res.pca)                                                          #theme by defaut
plot.PCA(res.pca, theme=theme_factominer(base_size=8, rel_title=1.2),  # smaller theme
   ggoptions = list(size=2.5))                                         # and smaller size
plot(res.pca,theme=ggplot2::theme_grey())                              # ggplot2 theme
}
\keyword{aplot}