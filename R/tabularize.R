#' Tabularize chunks output
#' 
#' @export
#' @param x the output of [pub_chunks()]
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
#' (res <- pub_chunks(list(x, y), c("doi", "title", "keywords")))
#' pub_tabularize(res)
#' 
#' \dontrun{
#' # using output of fulltext::ft_get()
#' if (requireNamespace("fulltext", quietly = TRUE)) {
#'   dois <- c('10.1371/journal.pone.0086169', '10.1371/journal.pone.0155491')
#'   x <- fulltext::ft_get(dois, from='plos')
#'   (tmp <- pub_chunks(fulltext::ft_collect(x), sections=c("doi","title")))
#'   pub_tabularize(tmp)
#' }}
pub_tabularize <- function(x) {
  assert(x, "pub_chunks")
  fr <- attr(x, "from")
  switch(fr, 
    character = pub_tab_one(x),
    list = pub_tab(x),
    ft_data = lapply(x, pub_tab)
  )
}

pub_tab <- function(x) {
  out <- lapply(x, pub_tab_one)
  rbl(out, idcol = TRUE)
}

pub_tab_one <- function(x) {
  x[sapply(x, length) == 0] <- NULL
  data.frame(unclass(x), stringsAsFactors = FALSE)
}
