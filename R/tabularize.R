#' Tabularize chunks output
#' 
#' @export
#' @param x the output of [pub_chunks()]
#' @param bind (logical) whether to bind list of data.frames or not. 
#' ignored unless `list` input to `x`. default: `FALSE`
#' @return a data.frame or list
#' @examples
#' # one at a time
#' ## example 1, a file path
#' x <- system.file("examples/elife_1.xml", package = "pubchunks")
#' (res <- pub_chunks(x, c("doi", "title", "keywords")))
#' pub_tabularize(res)
#' 
#' ## example 2, a file path
#' y <- system.file("examples/frontiers_1.xml", package = "pubchunks")
#' (res2 <- pub_chunks(y, c("doi", "title", "keywords")))
#' pub_tabularize(res2)
#' 
#' # > 1, a list of file paths
#' x <- system.file("examples/elife_1.xml", package = "pubchunks")
#' y <- system.file("examples/frontiers_1.xml", package = "pubchunks")
#' (res <- pub_chunks(list(x, y), c("doi", "title", "keywords")))
#' pub_tabularize(res)
#' pub_tabularize(res, bind = TRUE)
#' 
#' \dontrun{
#' # using output of fulltext::ft_get()
#' if (requireNamespace("fulltext", quietly = TRUE)) {
#'   dois <- c('10.1371/journal.pone.0086169', '10.1371/journal.pone.0155491', 
#'     '10.7554/eLife.03032')
#'   x <- fulltext::ft_get(dois)
#'   (tmp <- pub_chunks(fulltext::ft_collect(x), sections=c("doi","title")))
#'   pub_tabularize(tmp)
#'   pub_tabularize(tmp, bind = TRUE)
#' }}
pub_tabularize <- function(x, bind = FALSE) {
  UseMethod("pub_tabularize")
}

#' @export
pub_tabularize.default <- function(x, bind = FALSE) {
  stop("no pub_tabularize method for ", class(x)[1L])
}

#' @export
pub_tabularize.pub_chunks <- function(x, bind = FALSE) {
  assert(x, "pub_chunks")
  fr <- attr(x, "from")
  switch(fr,
    character = pub_tab_one(x),
    file = pub_tab_one(x),
    xml_document = pub_tab_one(x),
    ft_data = lapply(x, pub_tab)
  )
}

#' @export
pub_tabularize.list <- function(x, bind = FALSE) {
  if (is.null(attr(x, "ft_data"))) attr(x, "ft_data") <- FALSE
  if (attr(x, "ft_data")) {
    res <- lapply(x, function(w) lapply(w, pub_tabularize))
    if (bind) rbl(lapply(res, rbl)) else res
  } else {
    res <- lapply(x, pub_tabularize)
    if (bind) rbl(res) else res
  }
}


# helpers ----
pub_tab <- function(x) {
  out <- lapply(x, pub_tab_one)
  rbl(out, idcol = TRUE)
}

pub_tab_one <- function(x) {
  x[sapply(x, length) == 0] <- NULL
  data.frame(unclass(x), stringsAsFactors = FALSE)
}
