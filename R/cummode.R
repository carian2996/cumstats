#' Cumulative Mode
#'
#' Returns a list whose elements are the cumulative \emph{statistical mode(s)}
#' of the elements of the argument.
#'
#' @param x a numeric vector
#'
#' @return A list of the same length as \code{x} with numeric vectors. \code{NA}
#'    values are also counted.
#'
#' @references
#' Kotz, S., Balakrishnan, N., Read, C.B, Vidakovic, B., Johnson, N.L. (2006)
#'    \emph{Encyclopedia of Statistical Sciences.} Wiley, New Jersey.
#'
#' @examples
#' cummode(c(rep(1, 2), rep(12, 5), rep(44, 3), rep(8, 5), 55))
#'
#' cummode(c(rep(1, 2), rep(12, 5), rep(44, 3), rep(8, 5), rep(NA, 7), 55))
#'
#' cummode(runif(5))
#'
#' cummode(c(rep("a", 2), rep("b", 5), rep("d", 3), rep("e", 5), rep(NA, 5)))
#'
#' @seealso \code{\link{Mode}}
#'
#' @author Arturo Erdely, \email{arturo.erdely@@comunidad.unam.mx}
#'
#' @export
#'

cummode <-
function(x) sapply(seq_along(x), function(k, z) Mode(z[1:k])$Values, z = x)
