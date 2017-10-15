library(testthat)
library(tobitar)
expect_that(make_filename(2013), matches("accident_2013.csv.bz2"))
