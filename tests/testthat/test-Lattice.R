library(testthat)

data(gg1984, package = "yabaf")
b <- Breeder(gg1984)
l <- Lattice(b)

test_that("Breeder abstract class test", {
    expect_true(is(b, "Breeder")) 
})

test_that("Lattice concrete class test", {
    expect_true(is(l, "Lattice")) 
})

## test_that("Lattice concrete class sigma check", {
##     expect_equal(round(l$report$fit$sigma, 4), 17.9712) ## see reference
## })

test_that("Lattice anova terms", {
    expect_true(all(c("(Intercept)", "residual (MS)") %in% rownames(l$report$anova)))
})

test_that("Lattice variance components", {
    expect_true(all(c("replication", "replication:block") %in% rownames(l$report$variance)))
})
