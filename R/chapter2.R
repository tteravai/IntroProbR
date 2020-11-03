#' Monte Carlo
#'
#' This function implements Monte Carlo integration with `n` steps,
#' estimating the area of a function, `f`, under the unit square.
#' With `sample_y = TRUE`, the default, the method in the text is used.
#' With `sample_y = FALSE`, the x points are sampled but f(x) is directly evaluated.
#' @export
MonteCarlo <- function(n, f = function(x) x^2, sample_y = TRUE) {
  x_spl <- RandomNumbers(n)
  f_spl <- pmin(f(x_spl), 1)
  y_spl <- {if (sample_y) RandomNumbers(n) else NULL}

  if (sample_y) return(mean(y_spl < f_spl))
  else return(mean(f_spl))
}

#' Buffon's Needle
#'
#' This simulates Buffon's Needle for estimating π. It is not done.
#' @export
BuffonsNeedle <- function(n, table_side = 8) {
  stop("Not implemented")
  x_spl <- RandomNumbers(n) * table_side
  y_spl <- RandomNumbers(n) * table_side
  theta <- RandomNumbers(n) * 2 * pi

}
