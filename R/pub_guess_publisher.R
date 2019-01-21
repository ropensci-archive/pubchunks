#' Guess the publisher from an XML document
#' 
#' @export
#' @param x an XML file, a character string of XML, or a 
#' `xml_document` object (as from `xml2::read_xml`)
#' @return a list, with two named character strings, one for 
#' `full_name` and the other a `short_name`
#' @examples
#' (x <- system.file("examples/pensoft_1.xml", package = "pubchunks"))
#' pub_guess_publisher(x)
#' 
#' (x <- system.file("examples/copernicus_2.xml", package = "pubchunks"))
#' pub_guess_publisher(x)
#' 
#' (x <- system.file("examples/peerj_1.xml", package = "pubchunks"))
#' pub_guess_publisher(x)
#' 
#' (x <- system.file("examples/hindawi_1.xml", package = "pubchunks"))
#' pub_guess_publisher(x)
#' 
#' (x <- system.file("examples/frontiers_1.xml", package = "pubchunks"))
#' pub_guess_publisher(x)
#' 
#' (x <- system.file("examples/elife_1.xml", package = "pubchunks"))
#' pub_guess_publisher(x)
#' 
#' (x <- system.file("examples/elsevier_1.xml", package = "pubchunks"))
#' pub_guess_publisher(x)
#' 
#' x <- system.file("examples/f1000research_1.xml", package = "pubchunks")
#' pub_guess_publisher(x)
#' 
#' x <- system.file("examples/plos_1.xml", package = "pubchunks")
#' pub_guess_publisher(x)
#' 
#' x <- system.file("examples/mdpi_1.xml", package = "pubchunks")
#' pub_guess_publisher(x)
#' 
#' x <- system.file("examples/pubmed_brief_1.xml", package = "pubchunks")
#' pub_guess_publisher(x)
#' 
#' x <- system.file("examples/pubmed_full_1.xml", package = "pubchunks")
#' pub_guess_publisher(x)
#' 
#' x <- system.file("examples/pubmed_full_2.xml", package = "pubchunks")
#' pub_guess_publisher(x)
#' 
#' x <- system.file("examples/pubmed_full_3.xml", package = "pubchunks")
#' pub_guess_publisher(x)
pub_guess_publisher <- function(x) {
  if (!class(x)[[1L]] %in% c("character", "xml_document")) {
    stop("x must be of class character or xml_document")
  }
  x <- check_xml(x)
  tmp <- falltxt(x, "//publisher/publisher-name")
  if (length(tmp) == 0 || all(is.na(tmp))) {
    tmp <- tryCatch(f1txt(x, "//prism:publisher"), 
      error = function(e) e, warning = function(w) w)
  }
  if (
    length(tmp) == 0 || all(is.na(tmp)) || 
    inherits(tmp, "error") || inherits(tmp, "warning")
  ) {
    tmp <- tryCatch(f1txt(x, "//ISSN"), 
      error = function(e) e, warning = function(w) w)
    if (is.character(tmp) && grepl("[0-9]{4}-[0-9]{4}", tmp)) {
      z <- rcrossref::cr_journals(tmp)
      tmp <- if (!is.null(z$data)) z$data$publisher else "unknown"
    } else {
      tmp <- "unknown"
    }
  }
  list(
    full_name = tmp,
    short_name = pull_name(tmp)
  )
}

pgp <- function(x, prov) {
  if (is.null(prov) || prov == "cached") {
    pub_guess_publisher(x)$short_name 
  } else {
    prov
  }
}

check_xml <- function(x) {
  switch(
    class(x)[[1L]],
    character = xml2::read_xml(x),
    xml_document = x
  )
}

pull_name <- function(z) {
  pubs <- "pensoft|copernicus|peerj|hindawi|frontiers|elife|elsevier|f1000 research|f1000research|public library|mdpi"
  tmp <- gsub("\\s", "", tolower(strextract(z, pubs, ignore.case = TRUE)))
  if (!length(tmp)) tmp <- tolower(gsub("\\s", "_", z))
  if (tmp == "publiclibrary") "plos" else tmp
}
