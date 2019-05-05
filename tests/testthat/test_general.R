context("general")

test_that("the `it` argument has the right value at the end of the loop",{
  expect_equal(10, {pb_for(); for (i in 1:10) {}; i})
  expect_equal(11, {pb_for(); for (i in 1:10) {i <- i+1}; i})
  expect_equal(3, {
    pb_for()
    try(for (i in 1:10) stopifnot(i <3) ,silent = T)
    i})

  pb <- progress::progress_bar$new()
  expect_equal(10, {pb -> for (i in 1:10) {}; i})
  expect_equal(11, {pb -> for (i in 1:10) {i <- i+1}; i})
  expect_equal(3, {
    try(pb -> for (i in 1:10) stopifnot(i <3) ,silent = T)
    i})
})

test_that("we can use our functions in diverse environments", {
  expect_equal(10, {pb_for(); j <- 0; for (i in 1:4) {j <- j + i}; j})
  local(expect_identical(c("a","test"), {
    test <- function(){
      pb_for()
      j <- 0
      for (i in 1:4) {j <- j + i}
      j
    }
    a <- test()
    ls()
    }))

  expect_equal(10, {
    test <- function(){
      pb_for()
      j <- 0
      for (i in 1:4) {j <- j + i}
      j
    }
    a <- test()
    a
  })
})
