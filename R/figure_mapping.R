# figure_mapping.R


#' Add scalebar to a gglpot
#'
#' Add scalebar to a gglpot // deprecated. Use ggspatial
#' @export
scalebar_geoms <- function(x_start,x_diff_m,y_start,y_diff,p4s_orig,p4s_utm,slabs) {
  # plots a single black scalebar. p4s is proj4string character object
  pts_CRS_orig <- SpatialPoints(matrix(c(x_start,y_start),ncol=2),proj4string = CRS(p4s_orig))
  pts_CRS_utm <- spTransform(pts_CRS_orig,CRSobj = CRS(p4s_utm))
  pts_CRS_utm_end <- pts_CRS_utm
  pts_CRS_utm_end@coords[1,1] <- pts_CRS_utm@coords[1,1] + x_diff_m
  x_end <- spTransform(pts_CRS_utm_end,CRSobj = CRS(p4s_orig))@coords[1,1]

  scalebar_coords <- matrix(c(x_start,y_start,
                              x_end,y_start,
                              x_end,y_start+y_diff,
                              x_start,y_start+y_diff,
                              x_start,y_start),ncol=2,byrow=TRUE)

  srl_1 <- Polygon(scalebar_coords,hole=FALSE)
  Srl_1 <- Polygons(list(srl_1),1)
  scalebar_poly <- SpatialPolygons(list(Srl_1),proj4string = CRS(p4s_orig))
  scalebar_shape_geom <- geom_polygon(data=scalebar_poly,aes(x=long,y=lat))

  text_df <- data.frame(lab=slabs,ref=0:(length(slabs)-1))
  text_df$x <- text_df$ref * (x_end-x_start) + x_start
  text_df$y <- y_start+y_diff*1.5
  scalebar_text_geom <- geom_text(data=text_df,aes(x=x,y=y,label=lab,vjust=0)) #,position="center"
  # p_stations_all + scalebar_shape_geom + scalebar_text_geom

  return(list(scalebar_shape_geom,scalebar_text_geom))
}
