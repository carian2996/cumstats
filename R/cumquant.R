#' Cumulative Quantile
#'
#' Returns a vector whose elements are the cumulative \emph{quantile} of the
#' elements of the argument.
#'
#' @param x a numeric vector
#' @param p probability for the desired quantile
#' @param type See \code{quatile} in R base package
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
#' y <- c(9, 1, 3, 0, NA, 2, 5)
#' cummedian(y)
#' cumquant(y, 0.5)
#'
#' z <- cumquant(rcauchy(10000), 0.75)
#' head(z); tail(z)
#'
#' @author Arturo Erdely, \email{arturo.erdely@@comunidad.unam.mx}
#'
#' @export
#'

cumquant <-
function(x, p, type = 7){
  na.pos <- which(is.na(x))
  if (length(na.pos) > 0) {
    if (min(na.pos) == 1) xx <- rep(NA, length(x))
    if (min(na.pos) > 1) {
      y <- rep(NA, length(x) - min(na.pos) + 1)
      x <- x[0:(min(na.pos) - 1)]
      xx <- c(sapply(seq_along(x),
                     function(k, z) stats::quantile(z[1:k],
                                                    probs = p[1],
                                                    names = FALSE,
                                                    type = type), z = x), y)
    }
  } else xx <- sapply(seq_along(x),
                      function(k, z) stats::quantile(z[1:k],
                                                     probs = p[1],
                                                     names = FALSE,
                                                     type = type), z = x)
  return(xx)
}
