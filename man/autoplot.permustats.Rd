% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.permustats.R
\name{autoplot.permustats}
\alias{autoplot.permustats}
\alias{fortify.permustats}
\title{Autoplot Graphics for vegan permustats Objects}
\usage{
\method{autoplot}{permustats}(
  object,
  plot = c("box", "violin", "density", "qqnorm"),
  scale = FALSE,
  facet = FALSE,
  gg.params = list(),
  ...
)

\method{fortify}{permustats}(model, data, scale = FALSE, ...)
}
\arguments{
\item{object}{Result object from \code{\link[vegan]{permustats}}.}

\item{plot}{Plot type, or geometry of \pkg{ggplot2}.}

\item{scale}{Use standardized effect sizes (SES).}

\item{facet}{Split graph to facets by \code{Term}.}

\item{gg.params}{Arguments passed to function drawing the box-like
object. Depending on argument \code{plot} the parameters are
passed to
\code{\link[ggplot2]{geom_boxplot}},\code{\link[ggplot2]{geom_violin}},
\code{\link[ggplot2]{geom_density}} or
\code{\link[ggplot2]{geom_qq}}}

\item{...}{Other parameters passed to functions (ignored).}

\item{model}{model or other R object to convert to data frame}

\item{data}{original dataset, if needed}
}
\description{
Alternatives for \pkg{lattice} graphics functions
\code{\link[vegan]{densityplot.permustats}},
\code{\link[vegan]{qqmath.permustats}} and
\code{\link[vegan]{boxplot.permustats}}.
}
\details{
Function \code{fortify} returns a data frame with variables
\code{Permutations} (numeric) and \code{Term} (factor labelling the
permutation). The result of \code{fortify} can be used to custom
build diagnostic plots. \code{autoplot} provides quick basic graphs
with limited flexibility.
}
\examples{
data(dune, dune.env)
mod <- cca(dune ~ A1 + Management + Moisture, dune.env)
(ano <- anova(mod, by="onedf"))
pstat <- permustats(ano)
head(fortify(pstat))
autoplot(pstat, "box")
## avoid overplotting x-axis text
autoplot(pstat, "violin") + scale_x_discrete(guide = guide_axis(n.dodge = 2))
autoplot(pstat, "density", facet = TRUE)
autoplot(pstat, "qqnorm", facet = TRUE) + geom_qq_line()

}
