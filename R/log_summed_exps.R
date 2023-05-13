#' Logged Summation of Exponentiated Vector Elements
#'
#' @param x A vector of type numeric
#'
#' @return The logged summation of exponentiated input vector
#'
#' @usage
#' log_summed_exps(x)
#'
#' @examples
#' log_summed_exps(1:2000)
#' log_summed_exps(seq(1, 4000, by = 2))
#' log_summed_exps(c(2000, 50, 30, 1, 5, 7))
#'
#' @export
log_summed_exps <- function(x) {
  if (!is.numeric(x)) {
    stop("Please enter a numeric vector.")
  } else {
    n = length(x)
    x = sort(x)
    x_n = x[n]
    x = x[-n]
    lse = x_n + log(1 + sum(exp(x - x_n)))
    return(lse)
  }
}
