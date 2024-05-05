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
#' @param img    matrix to be displayed
#' @param col    color palette
#' @param xlab   X axis label
#' @param ylab   Y axis label
#' @param ...    additional arguments passed to `image()`
#'
#' @aliases grayimage
#'
greyimage <- function(img, col=grey256, xlab="X", ylab="Y", ...) {
  nr <- nrow(img)
  nc <- ncol(img)
  image(1:nr, 1:nc, img, col=col, useRaster=TRUE, asp=1, xlab=xlab, ylab=ylab, ...)
}

grayimage <- greyimage

