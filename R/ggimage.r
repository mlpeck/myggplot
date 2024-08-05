#' A ggplot2 based raster image function
#'
#' Displays a matrix as a raster image with user selectable colors
#'
#' `zmat` is the maatrix to display. All other arguments are optional
#' with mostly `NULL` defaults
#'
#' @param zmat       the matrix to display
#' @param x          x axis values -- either a vector the same length as number of rows or a matrix with the same dimensions as `zmat`
#' @param y          y axis values or NULL
#' @param zlim       range of z values to display
#' @param col        color palette. Defaults to viridis(256)
#' @param xlab       x axis label
#' @param ylab       y axis label
#' @param show.legend display legend?
#' @param legend     label for legend
#' @param title      main graph title
#' @param xrev       if `TRUE` reverse X axis
#' @param yrev       if `TRUE` reverse Y axis
#' @param asp        aspect ratio
#' @param addcontour add contours?
#' @param binwidth   contour spacing if used
#'
#' @return data frame constructed to build plot and the graph, invisibly in a list
#'
ggimage <- function(zmat, x=NULL, y=NULL, zlim=range(zmat, na.rm=TRUE),
                    col=viridisLite::viridis(256),
                    xlab=NULL, ylab=NULL, 
                    show.legend=TRUE, legend=NULL, title=NULL,
                    xrev=FALSE, yrev=FALSE, asp=1,
                    addcontour=FALSE, binwidth=NULL
                   ) {
    require(ggplot2)
    if (is.null(x)) x <- 1:nrow(zmat)
    if (is.null(y)) y <- 1:ncol(zmat)
    if (!is.null(dim(x))) {
      x <- colMeans(x)
      y <- rowMeans(y)
    }
    xy <- expand.grid(x,y)
    df <- data.frame(cbind(xy, as.vector(zmat)))
    names(df) <- c("x","y","z")
    g1 <- ggplot(df, aes(x=x, y=y, z=z)) + 
        geom_raster(aes(fill=z), show.legend=show.legend) + 
        scale_fill_gradientn(colors=col, na.value="#FFFFFF00", limits=zlim) +
        coord_fixed(ratio = asp)
    if (addcontour) {
        if (!is.null(binwidth)) {
            g1 <- g1 + geom_contour(binwidth=binwidth, na.rm=TRUE)
        } else {
            g1 <- g1 + geom_contour(na.rm=TRUE)
        }
    }
    if (xrev) g1 <- g1 + scale_x_reverse()
    if (yrev) g1 <- g1 + scale_y_reverse()
    if (!is.null(xlab)) {
        g1 <- g1 + xlab(xlab)
    }
    if (!is.null(ylab)) {
        g1 <- g1 + ylab(ylab)
    }
    if (!is.null(legend)) {
        g1 <- g1 + labs(fill=legend)
    }
    if (!is.null(title)) {
        g1 <- g1 + ggtitle(title)
    }
    plot(g1)
    invisible(list(df=df, graph=g1))
}

