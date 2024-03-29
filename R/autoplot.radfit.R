### Rank-Abundance Distribution models: ggplot2 alternatives to
### lattice graphics in vegan.

#' Autoplot Graphics for vegan RAD models
#'
#' \CRANpkg{ggplot2} graphics for Rank-Abundance Distribution models
#' fitted with \CRANpkg{vegan} function \code{\link[vegan]{radfit}}.
#'
#' The \code{autoplot} function draws graphics which are \pkg{ggplot2}
#' alternatives for \CRANpkg{lattice} graphics in \pkg{vegan}. In
#' addition, there are functions for \code{\link[vegan]{as.rad}}
#' results which do not have dedicated graphics in\pkg{vegan}. The
#' \code{fortify} functions produce \dQuote{tidy} data frames that can
#' be used to produce the graphics.
#'
#' @examples
#' data(mite)
#' m1 <- radfit(mite[1,])
#' ## With logarithmic y-axis (default) Pre-emption model is a line
#' autoplot(m1) + labs(title="log-Abundance: Pre-emption model is a line")
#' ## With log-log scale, Zipf model is a line
#' autoplot(m1) + scale_x_log10() +
#'    labs(title="log-log Scale: Zipf model is a line")
#' ## Show only the best model
#' autoplot(m1, pick = "AIC")
#' ## Show selected models in one frame
#' autoplot(m1, pick = c("Z","M","L"), facet=FALSE)
#' ## plot best models for several sites
#' m <- radfit(mite[1:12,])
#' autoplot(m) + labs(title="Model Selection AIC (Default)")
#' ## use BIC and reoreder sites by their diversity
#' autoplot(m, pick="BIC", order.by = diversity(mite[1:12,])) +
#'    labs(title="Model Selection BIC, Ordered by Increasing Diversity")
#' ## Plot RAD models without fits highlighting most abundant species in the
#' ## whole data.
#' m0 <- as.rad(mite[1:12,])
#' dominants <- names(sort(colSums(mite), decreasing=TRUE))[1:6]
#' autoplot(m0, highlight = dominants)

#' @param object Result object from \code{\link[vegan]{radfit}}.
#' @param facet Draw each fitted model to a separate facet or (if
#'     \code{FALSE}) all fitted lines to a single graph.
#' @param point.params,line.params Parameters to modify points or
#'     lines (passed to \code{\link[ggplot2]{geom_point}} and
#'     \code{\link[ggplot2]{geom_line}}).
#'
#' @importFrom ggplot2 ggplot aes_ scale_y_log10 facet_wrap geom_point
#'     geom_line fortify autoplot
#' @importFrom scales oob_squish_infinite
#' @importFrom utils modifyList
#'
#' @export
`autoplot.radfit` <-
    function(object, facet = TRUE, point.params = list(), line.params = list(),
             ...)
{
    df <- fortify(object, ...)
    ymin <- min(1, df$Abundance)
    point.params <- modifyList(list(mapping=aes_(y = ~ Abundance)),
                               point.params)
    line.params <- modifyList(list(mapping=aes_(y = ~Fit, colour = ~ Model)),
                                   line.params)
    pl <- ggplot(df, aes_(~Rank)) +
        scale_y_log10(limit=c(ymin,NA), oob = oob_squish_infinite) +
        do.call("geom_point", point.params) +
        do.call("geom_line", line.params)
    if(facet)
        pl <- pl + facet_wrap(~Model)
    pl
}

#'
#' @importFrom ggplot2 fortify aes_ scale_y_log10 geom_point geom_line
#'     facet_wrap
#' @importFrom scales oob_squish_infinite
#' @importFrom utils modifyList
#'
#' @rdname autoplot.radfit
#' @export
`autoplot.radfit.frame` <-
    function(object, point.params=list(), line.params=list(), ...)
{
    df <- fortify(object, ...)
    ymin <- min(1, df$Abundance)
    point.params <- modifyList(list(mapping=aes_(y = ~ Abundance)),
                               point.params)
    line.params <- modifyList(list(mapping=aes_(y = ~Fit, colour = ~Model)),
                              line.params)
    ggplot(df, aes_(~Rank)) +
        scale_y_log10(limit=c(ymin,NA), oob = oob_squish_infinite) +
        do.call("geom_point", point.params) +
        do.call("geom_line", line.params) +
        facet_wrap(~Site)
}

#' @importFrom ggplot2 ggplot scale_y_log10 geom_point geom_line aes_
#'     fortify
#' @importFrom utils modifyList
#' @rdname autoplot.radfit
#' @export
`autoplot.radline`<-
    function(object, point.params = list(), line.params = list(), ...)
{
    df <- fortify(object, ...)
    ymin <- min(1, df$Abundance)
    point.params <- modifyList(list(mapping=aes_(y = ~ Abundance)),
                               point.params)
    line.params <- modifyList(list(mapping=aes_(y = ~ Fit)),
                              line.params)
    ggplot(df, aes_(~Rank)) +
        scale_y_log10() +  # no lower limit for a single line
        do.call("geom_point", point.params) +
        do.call("geom_line", line.params)
}

### Methods for rad: only points, no lines from vegan::as.rad()

#' @importFrom utils modifyList
#' @importFrom ggplot2 aes_ scale_y_log10 geom_point
#' @rdname autoplot.radfit
#' @export
`autoplot.rad` <-
    function(object, point.params = list(), line.params = list(), ...)
{
    df <- fortify(object, ...)
    ymin <- min(1, df$Abundance)
    point.params <- modifyList(list(mapping=aes_(y = ~ Abundance)),
                               point.params)
    ggplot(df, aes_(~Rank)) +
        scale_y_log10() +  # no lower limit for a single line
        do.call("geom_point", point.params)
}

#' @param highlight Names of species that should be highlighted as
#'     coloured points.
#' @importFrom utils modifyList
#' @importFrom ggplot2 aes_ scale_colour_discrete scale_y_log10
#'     geom_point facet_wrap
#' @rdname autoplot.radfit
#' @export
`autoplot.rad.frame` <-
    function(object, point.params=list(), highlight = NULL, ...)
{
    df <- fortify(object, ...)
    ymin <- min(1, df$Abundance)
    point.params <- modifyList(list(mapping=aes_(y = ~ Abundance)),
                               point.params)
    pl <- ggplot(df, aes_(~Rank)) +
        scale_y_log10(limit=c(ymin,NA)) +
        do.call("geom_point", point.params) +
        facet_wrap(~Site)
    if (!is.null(highlight)) {
        highlight <- factor(highlight, levels=highlight)
        for(sp in highlight)
            pl <- pl + geom_point(data=df[df$Species == sp, ,drop=FALSE],
                                  aes_(y = ~Abundance, colour = sp))
        pl <- pl + scale_colour_discrete(highlight, name = "Species")
    }
    pl
}

#'
#' @inheritParams ggplot2::fortify
#'
#' @param pick Pick or several models. Allowed values are \code{"AIC"}
#'     and \code{"BIC"} for selecting the best model by AIC or BIC, or
#'     (a vector of) model names that can be abbreviated. The default
#'     returns all fitted models.
#'
#' @importFrom stats AIC fitted
#'
#' @rdname autoplot.radfit
#' @export
`fortify.radfit` <-
    function(model, data, pick = NULL, ...)
{
    Abundance <- model$y
    nsp <- length(Abundance)
    Rank <- seq_len(nsp)
    Species <- names(Abundance)
    fv <- fitted(model)
    ## only pick wanted models
    if (!is.null(pick)) {
        pick <- match.arg(pick, c(names(model$models), "AIC","BIC"),
                          several.ok=TRUE)
        if (any(c("AIC","BIC") %in% pick)) {
            if("BIC" %in% pick)
                k <- log(length(model$y))
            else
                k <- 2
            pick <- which.min(AIC(model, k=k))
        }
        fv <- fv[, pick, drop=FALSE]
    }
    nmods <- NCOL(fv)
    modnames <- colnames(fv)
    mods <- factor(rep(modnames, each=nsp), levels=modnames)
    data.frame(Species = Species,
               Rank = rep(Rank, nmods),
               Abundance = rep(Abundance, nmods),
               Fit = as.vector(fv),
               Model = mods)
}

#' @param order.by A vector used for ordering site panels.
#'
#' @rdname autoplot.radfit
#' @export
`fortify.radfit.frame` <-
    function(model, data, pick = "AIC", order.by = NULL, ...)
{
    allmods <- names(model[[1]]$models)
    pick <- match.arg(pick, c(allmods, "AIC", "BIC"))
    abu <- lapply(model, function(x) x$y)
    nsp <- sapply(abu, length)
    spe <- lapply(abu, names)
    sit <- names(model)
    fv <- lapply(model, radpicker, pick = pick)
    mod <- sapply(fv, colnames)
    ## enable re-ordering of Site panels
    if (is.null(order.by))
        order.by <- seq_along(sit)
    else
        order.by <- order(order.by)
    data.frame(
        "Site" = factor(rep(sit, nsp), levels=sit[order.by]),
        "Species" = unlist(spe, use.names=FALSE),
        "Rank" =  unlist(sapply(nsp, seq_len), use.names=FALSE),
        "Abundance" = unlist(abu, use.names=FALSE),
        "Fit" = drop(do.call(rbind, fv)),
        "Model" = factor(rep(mod, nsp), levels=allmods)
    )
}
## support function to pick the model with lowest AIC or BIC or by the
## name. Input is a single model from a radfit.frame and pick is a
## single argument value.

#' @importFrom stats AIC fitted
#'
`radpicker` <-
    function(mod1, pick, ...)
{
    fv <- fitted(mod1)
    switch(pick,
           "AIC" = fv[, which.min(AIC(mod1)), drop=FALSE],
           "BIC" = fv[, which.min(AIC(mod1, k=log(nrow(fv)))), drop=FALSE],
           fv[,pick, drop=FALSE])
}

#' @importFrom stats fitted
#' @rdname autoplot.radfit
#' @export
`fortify.radline` <-
    function(model, data, ...)
{
    data.frame(
        Species = names(model$y),
        Rank = seq_along(model$y),
        Abundance = unclass(model$y),
        Fit = fitted(model)
    )
}

#' @rdname autoplot.radfit
#' @export
`fortify.rad` <-
    function(model, data, ...)
{
    data.frame(
        Species = names(model),
        Rank = seq_along(model),
        Abundance = as.vector(model)
    )
}

#' @rdname autoplot.radfit
#' @export
`fortify.rad.frame` <-
    function(model, data, order.by = NULL, ... )
{
    abu <- lapply(model, as.vector)
    nsp <- sapply(model, length)
    spe <- lapply(model, names)
    sit <- names(model)
    ## enable re-ordering of Site panels
    if (is.null(order.by))
        order.by <- seq_along(sit)
    else
        order.by <- order(order.by)
    data.frame(
        "Site" = factor(rep(sit, nsp), levels=sit[order.by]),
        "Species" = unlist(spe, use.names=FALSE),
        "Rank" =  unlist(sapply(nsp, seq_len), use.names=FALSE),
        "Abundance" = unlist(abu, use.names=FALSE)
    )
}


