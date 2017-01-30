#' Pearson's Measure of Kurtosis
#'
#' This function computes the estimator of Pearson's measure of \emph{kurtosis}.
#'
#' @param x a numeric vector
#'
#' @return A numeric value of skewness. Returns \code{NA} if \code{x} contains
#'    \code{NA} value(s), and \code{NaN} if \code{length(unique(x))==1} is
#'    \code{TRUE}.
#'
#' @references
#' Komsta, L. and Novomestky, F. (2015). \emph{moments: Moments, cumulants,
#' skewness, kurtosis and related tests.} R package version 0.14.
#' \url{https://CRAN.R-project.org/package=moments}
#'
#' @examples
#' kurtosis(c(9, 1, 3, 0))
#'
#' @seealso \code{\link{cumkurt}} for cummulative kurtosis.
#'
#' @author Adapted by Arturo Erdely, \email{arturo.erdely@@comunidad.unam.mx},
#'    from \code{moments} R package by Lukasz Komsta.
#'
#' @export
#'

kurtosis <-
function(x) length(x) * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
