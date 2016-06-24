# If you are looking at this to grade the assignment, I think you safely can ignore 
# this code. It's just here to test that my code is working.


source("../cachematrix.R")

test_that("Inversion", {
  a.matrix <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE) 
  cache.matrix <- makeCacheMatrix(a.matrix)
  
  expect_true(1 == 1)
  expect_true(isTRUE(all.equal(cache.matrix$get(), a.matrix)))
  #expect_message()
  expect_true(isTRUE(all.equal(cache.matrix$inverse(), solve(a.matrix))))
  expect_true(isTRUE(all.equal(cache.matrix$inverse(), solve(a.matrix)))) # Second time, cache should kick in
  #expect_message()
})