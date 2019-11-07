context("general_functions")


test_that("bound_vec works on vector and data.frames",{
  expect_equal(bound_vec(1:20,c(2.5,10.5)),
               c(2.5,2.5,3:10,10.5,10.5,10.5,10.5,10.5,10.5,10.5,10.5,10.5,10.5))
  expect_equal(dplyr::mutate(data.frame(a=1:20),b=bound_vec(a,c(3.2,15.7))),
               data.frame(a=1:20,b=c(3.2,3.2,3.2,4:15,15.7,15.7,15.7,15.7,15.7)))
})
