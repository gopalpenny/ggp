# matrix_processing.R

#' Bilinear data.frame interpolation
#'
#' Interpolated on a grid using akima interpolation
#' @param df data.frame containing gridded x, y locations and z values
#' @param out_dim vector containing \code{c(nx, ny)} as number of x and y output locations
#' @importFrom dplyr %>%
#' @details
#' The data.frame df must contain columns for x, y, and z. The x and y
#' values must be on a grid.
#'
#' Interpolation is done using \code{akima::bilinear}
#' @examples
#' library(ggplot2)
#' library(ggp)
#' df <- expand.grid(x=1:5,y=1:5)
#' df$z <- df$x^2 + df$y^2
#' df_out <- akima_bilinear(df, c(100,100))
#' ggplot(df) + geom_raster(aes(x,y,fill=z)) + coord_equal()
#' ggplot(df_out) + geom_raster(aes(x,y,fill=z)) + coord_equal()
akima_bilinear <- function(df, out_dim) {
  x <- sort(unique(df$x))
  y <- sort(unique(df$y))
  z <- matrix(df$z[order(df$x,df$y)],nrow=length(x),byrow = TRUE)
  grid_out <- expand.grid(x = seq(min(x), max(x), length.out = out_dim[1]),
                          y = seq(min(y), max(y), length.out = out_dim[2]))
  df_out <- akima::bilinear(x = x, y = y, z = z, x0 = grid_out$x, y0 = grid_out$y) %>%
    dplyr::bind_cols()
}
