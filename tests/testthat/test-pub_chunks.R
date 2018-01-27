context("pub_chunks")

x <- system.file("examples/frontiers_1.xml", package = "pubchunks")

test_that("pub_chunks returns...", {
  skip_on_cran()

  # single sections
  aa <- pub_chunks(x, "title")
  bb <- pub_chunks(x, "abstract")
  
  # multiple sections
  cc <- pub_chunks(x, c("title", "abstract"))
  
  expect_is(aa, "list")
  expect_is(bb, "list")
  expect_is(cc, "list")
  
  expect_named(aa, "title")
  expect_named(bb, "abstract")
  expect_named(cc, c("title", "abstract"))
})

test_that("pub_chunks fails well", {
  skip_on_cran()

  expect_error(pub_chunks(), "\"x\" is missing")
  expect_error(pub_chunks('adfafsdf'), "does not exist")
  expect_error(pub_chunks(5), "method for numeric")
  expect_error(pub_chunks(mtcars), "method for data.frame")
})
