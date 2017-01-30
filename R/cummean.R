#' Cumulative Arithmetic Mean
#'
#' Returns a vector whose elements are the cumulative \emph{arithmetic mean} of
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
#' cummean(c(9, 1, 4, 0, 3, NA, 8, 5))
#'
#' @seealso \code{\link{cumgmean}} for cummulative geometric mean, and
#'    \code{\link{cumhmean}} for cummulative harmonic mean.
#'
#' @author Arturo Erdely, \email{arturo.erdely@@comunidad.unam.mx}
#'
#' @export
#'

cummean <-
function(x) cumsum(as.numeric(x)) / seq_along(x)
