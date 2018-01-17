#' Tabularize chunks output
#' 
#' @export
#' @param x input
#' @examples
#' # x <- system.file("examples/10_7554_eLife_03032.xml", 
#' #   package = "pubchunks")
#' # res <- pub_chunks(x, "elife", c("doi", "title", "keywords"))
#' # pub_tabularize(res)

pub_tabularize <- function(x){
  # each publisher
  # out <- lapply(x, function(a){
    # each article
    # out <- lapply(x, function(y){
      x[sapply(x, length) == 0] <- NULL
      x <- data.frame(x, stringsAsFactors = FALSE)
      # y
    # })
  # })
  # lapply(out, rbl)
  lapply(x, rbind_fill)
}
