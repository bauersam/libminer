test_that("lib_summary returns expected result", {
  #First create object to test
  res <- lib_summary()
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 2)
  expect_equal(names(res), c("Library", "n_packages"))
  expect_type(res$Library, "character")
  expect_type(res$n_packages, "integer")
})

#the error message that will appear if we run it with a character
test_that("lib_summary fails appropriately",{
          expect_error(lib_summary("foo"), "'sizes' must be TRUE or FALSE.")
  })

test_that("sizes argument works", {
  res <- lib_summary(sizes = TRUE)
  expect_equal(ncol(res), 3)
  expect_equal(names(res), c("Library", "n_packages", "lib_size"))
  expect_type(res$lib_size, "double")

})
