## code to prepare `my_colors` dataset goes here

my_colors <- list(
  count=colorspace::diverge_hcl(9,h=c(0,220),c = 80,l=c(40,90)),
  vegetation=colorspace::heat_hcl(6,h=c(40,250),c = c(30,70),l=c(80,40)),
  temperature=colorspace::heat_hcl(6,h=c(250,0),c = c(60,100),l=c(90,60)),
  red_blue_diverging=colorspace::diverge_hcl(9,h=c(0,220),c = 80,l=c(40,90))
)
usethis::use_data(my_colors)
