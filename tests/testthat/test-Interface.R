library(testthat)

data(std1997, package = "yabaf")
b <- Breeder(std1997)
s <- SplitPlot(b)

test_that("Breeder abstract class test", {
    expect_true(is(b, "Breeder")) 
})

test_that("SplitPlot concrete class test", {
    expect_true(is(s, "SplitPlot")) 
})

test_that("SplitPlot concrete class sigma check", {
    expect_equal(round(s$report$fit$sigma, 4), 4.5068) ## see reference
})
