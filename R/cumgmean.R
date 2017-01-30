#' Cumulative Geometric Mean
#'
#' Returns a vector whose elements are the cumulative \emph{geometric mean} of
#' the elements of the argument.
#'
#' @param x a numeric vector
#'
#' @return A numeric vector of the same length as \code{x}. An \code{NA} value
#'    in \code{x} causes the corresponding and following elements of the return
#'    value to be \code{NA}.
#'
#' @references
#' Kotz, S., Balakrishnan, N., Read, C.B, Vidakovic, B., Johnson, N.L. (2006)
#'    \emph{Encyclopedia of Statistical Sciences.} Wiley, New Jersey.
#'
#' @examples
#' cumgmean(c(9, 1, 4, 0, 3, NA, 8, 5))
#'
#' z <- cumgmean(rlnorm(10000, 0, 1))
#' head(z); tail(z)
#'
#' @seealso \code{\link{cumhmean}} for cummulative harmonic mean, and
#'    \code{\link{cummean}} for cummulative mean.
#'
#' @author Arturo Erdely, \email{arturo.erdely@@comunidad.unam.mx}
#'
#' @export
#'

cumgmean <-
function(x) exp(cummean(log(x)))
