## greyscale

#' grey256
#' 
#' 256 level greyscale palette
#'
#' @aliases gray256
grey256 <- gray(seq(0,1,length=256))
gray256 <- grey256


#' base graphics raster image
#'
#' A simple raster image function with sensible defaults
#'
#' The color palette defaults to 256 level grayscale, but can be anything
#'
#' @param zmat   matrix to be displayed, or a named list with the image, x coordinates, y coordinates
#' @param x      vector of x coordinates
#' @param y      vector of y coordinates
#' @param col    color palette
#' @param xlab   X axis label
#' @param ylab   Y axis label
#' @param ...    additional arguments passed to `image()`
#'
#' @aliases grayimage
#'
greyimage <- function(zmat, x=NULL, y=NULL, col=grey256, xlab="X", ylab="Y", ...) {

  if (is.list(zmat)) {
    x <- zmat[[2]]
    y <- zmat[[3]]
    xlab <- names(zmat)[2]
    ylab <- names(zmat)[3]
    zmat <- zmat[[1]]
  }
  if (is.null(x)) x <- 1:nrow(zmat)
  if (is.null(y)) y <- 1:ncol(zmat)
  image(x, y, zmat, col=col, useRaster=TRUE, asp=1, xlab=xlab, ylab=ylab, ...)
}

grayimage <- greyimage

