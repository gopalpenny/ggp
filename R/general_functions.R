# general_functions.R

#' Get object size
#'
#' Get size of R object
#' @examples
#' x <- rnorm(100)
#' obj_size(x)
obj_size <- function(x,units="MB") {print(format(object.size(x),units=units))}

#' Bound a numeric vector
#'
#' Bound a numeric vector with upper and lower limits, so that anything outside the limit is replaced by the limit.
#' @param x A number or numeric vector
#' @param bounds A vector \code{c(a,b)} which identify the lower (a) and upper (b) limits, respectively.
#' @examples
#' bound_vec(1:20,c(2.5,10.5))
#' dplyr::mutate(data.frame(a=1:20),b=bound_vec(a,c(3.2,15.7)))
bound_vec <- function(x,bounds) {
  x[x < bounds[1]] <- bounds[1]
  x[x > bounds[2]] <- bounds[2]
  return(x)
}
