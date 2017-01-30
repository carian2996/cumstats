#' Statistical Mode
#'
#' This function computes the statistical \emph{mode} of given data.
#'
#' @param x a numeric vector
#'
#' @return A list containing the following components:
#'    \itemize{
#'      \item{Values }{of statistical mode(s) found, in the order they appear in \code{x}}
#'      \item{Frequency }{number of times the mode(s) appear in \code{x}}
#'    }
#'    \code{NA} values are also considered.
#'
#' @references
#' Kotz, S., Balakrishnan, N., Read, C.B, Vidakovic, B., Johnson, N.L. (2006)
#'    \emph{Encyclopedia of Statistical Sciences.} Wiley, New Jersey.
#'
#' @examples
#' Mode(c(rep(1, 2), rep(12, 5), rep(44, 3), rep(8, 5), 55))
#'
#' Mode(c(rep(1, 2), rep(12, 5), rep(44, 3), rep(8, 5), rep(NA, 7), 55))
#'
#' Mode(runif(5))
#'
#' Mode(c(rep("a", 2), rep("b", 5), rep("d", 3), rep("e", 5), rep(NA, 5)))
#'
#' @seealso \code{\link{cummode}} for cummulative mode.
#'
#' @author Ian Castillo
#'
#' @export
#'

Mode <-
function(x){
    counts <- tabulate(match(x, unique(x)))
    list(Values = unique(x)[counts == max(counts)], Frequency = max(counts))
}
