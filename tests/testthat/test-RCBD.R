library(testthat)

data(ergoStool, package = "nlme")
names(ergoStool) <- c("response", "treat", "block")
b <- Breeder(as.data.frame(ergoStool))
r <- RCBD(b)

test_that("Breeder abstract class test", {
    expect_true(is(b, "Breeder")) 
})

test_that("RCBD concrete class test", {
    expect_true(is(r, "RCBD")) 
})

test_that("RCBD concrete class sigma check", {
    expect_equal(round(r$report$fit$sigma, 4), 1.1003) ## see reference
})

