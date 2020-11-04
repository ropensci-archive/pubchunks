context("pub_chunks")

x <- system.file("examples/frontiers_1.xml", package = "pubchunks")

test_that("pub_chunks works", {
  skip_on_cran()

  # single sections
  aa <- pub_chunks(x, "title")
  bb <- pub_chunks(x, "abstract")
  
  # multiple sections
  cc <- pub_chunks(x, c("title", "abstract"))
  
  expect_is(aa, "pub_chunks")
  expect_is(bb, "pub_chunks")
  expect_is(cc, "pub_chunks")

  expect_is(unclass(aa), "list")
  expect_is(unclass(bb), "list")
  expect_is(unclass(cc), "list")

  expect_equal(attr(aa, "from"), "file")
  expect_equal(attr(aa, "sections"), "title")

  expect_equal(attr(bb, "from"), "file")
  expect_equal(attr(bb, "sections"), "abstract")

  expect_equal(attr(cc, "from"), "file")
  expect_equal(attr(cc, "sections"), c("title", "abstract"))
  
  expect_named(aa, c("title", ".publisher"))
  expect_named(bb, c("abstract", ".publisher"))
  expect_named(cc, c("title", "abstract", ".publisher"))
})

test_that("pub_chunks: extract param", {
  skip_on_cran()

  z <- system.file("examples/plos_2.xml", package = "pubchunks")
  zz <- system.file("examples/frontiers_1.xml", package = "pubchunks")

  # not an allowed value for extract
  expect_error(pub_chunks(z, "abstract", extract="foo"))

  # xml_text vs. as.character
  ## PLOS: lots of tags
  res <- pub_chunks(z, "abstract", extract="xml_text")
  expect_true(grepl("BackgroundThe health", res$abstract))
  res1 <- pub_chunks(z, "abstract", extract="as.character")
  expect_false(grepl("BackgroundThe health", res1$abstract))

  ## frontiers: abstract, p, sub, and italic tags
  res2 <- pub_chunks(zz, "abstract", extract="xml_text")
  expect_true(grepl("^Our current", res2$abstract))
  res3 <- pub_chunks(zz, "abstract", extract="as.character")
  expect_false(grepl("^Our current", res3$abstract))
  expect_true(grepl("^<abstract>\\n  <p>Our current", res3$abstract))
})

test_that("pub_chunks fails well", {
  skip_on_cran()

  expect_error(pub_chunks(), "\"x\" is missing")
  expect_error(pub_chunks('adfafsdf'), "does not exist")
  expect_error(pub_chunks(5), "method for numeric")
  expect_error(pub_chunks(mtcars), "method for data.frame")
})
