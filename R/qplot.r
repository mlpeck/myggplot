#' my pilfered `qplot`
#'
#' ggplot2's qplot function without the annoying deprecated warnings
#'
#' This was copied directly from the ggplot2 function of the same name. Checks for
#' deprecation were edited out. Also, some unexported ggplot2 functions are invoked
#' with `ggplot2:::`.
#'
#' @param x       x values to plot
#' @param y       y values
#' @param ...     other variables in plot
#' @param facets  facets
#' @param margins margins
#' @param geom    `geom` to use or `auto` to make a reasonable guess
#' @param xlim    x axis limits
#' @param ylim    y axis limits
#' @param log     specify axis to be log-scaled 
#' @param main    graph title
#' @param xlab    x axis label
#' @param ylab    y axis label
#' @param asp     x/y aspect ratio
#'
#' @return the graph
#'
#' @examples
#' x <- runif(100)
#' y <- x + rnorm(100, sd=0.1)
#' qplot(x, y, main="Fake data")
#'
qplot <- function (x, y, ..., data, facets = NULL, margins = FALSE, geom = "auto", 
                   xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, 
                   xlab = NULL, ylab = NULL, asp = NA) {
    
    require(ggplot2)
    require(rlang)
    
    caller_env <- parent.frame()
    if (!is.character(geom)) {
        stop("geom must be character")
    }
    exprs <- enquos(x = x, y = y, ...)
    is_missing <- vapply(exprs, quo_is_missing, logical(1))
    is_constant <- (!names(exprs) %in% ggplot2:::ggplot_global$all_aesthetics) | 
    vapply(exprs, quo_is_call, logical(1), name = "I")
    mapping <- ggplot2:::new_aes(exprs[!is_missing & !is_constant], env = caller_env)
    consts <- exprs[is_constant]
    aes_names <- names(mapping)
    if (is.null(xlab)) {
        if (quo_is_missing(exprs$x)) {
            xlab <- ""
        }
        else {
            xlab <- as_label(exprs$x)
        }
    }
    if (is.null(ylab)) {
        if (quo_is_missing(exprs$y)) {
            ylab <- ""
        }
        else {
            ylab <- as_label(exprs$y)
        }
    }
    if (missing(data)) {
        data <- ggplot2:::data_frame0()
        facetvars <- all.vars(facets)
        facetvars <- facetvars[facetvars != "."]
        names(facetvars) <- facetvars
        facetsdf <- as.data.frame(mget(facetvars, envir = caller_env))
        if (nrow(facetsdf)) 
            data <- facetsdf
    }
    if ("auto" %in% geom) {
        if ("sample" %in% aes_names) {
            geom[geom == "auto"] <- "qq"
        }
        else if (missing(y)) {
            x <- eval_tidy(mapping$x, data, caller_env)
            if (is.discrete(x)) {
                geom[geom == "auto"] <- "bar"
            }
            else {
                geom[geom == "auto"] <- "histogram"
            }
            if (is.null(ylab)) 
                ylab <- "count"
        }
        else {
            if (missing(x)) {
                mapping$x <- quo(seq_along(!!mapping$y))
            }
            geom[geom == "auto"] <- "point"
        }
    }
    p <- ggplot(data, mapping, environment = caller_env)
    if (is.null(facets)) {
        p <- p + facet_null()
    }
    else if (is.formula(facets) && length(facets) == 2) {
        p <- p + facet_wrap(facets)
    }
    else {
        p <- p + facet_grid(rows = deparse(facets), margins = margins)
    }
    if (!is.null(main)) 
        p <- p + ggtitle(main)
        for (g in geom) {
            params <- lapply(consts, eval_tidy)
            p <- p + do.call(paste0("geom_", g), params)
        }
        logv <- function(var) var %in% strsplit(log, "")[[1]]
    if (logv("x")) 
        p <- p + scale_x_log10()
    if (logv("y")) 
        p <- p + scale_y_log10()
    if (!is.na(asp)) 
        p <- p + theme(aspect.ratio = asp)
    if (!missing(xlab)) 
        p <- p + xlab(xlab)
    if (!missing(ylab)) 
        p <- p + ylab(ylab)
    if (!missing(xlim) && !all(is.na(xlim))) 
        p <- p + xlim(xlim)
    if (!missing(ylim) && !all(is.na(ylim))) 
        p <- p + ylim(ylim)
    p
}
                   
                   
