#' @title Extract chunks of data from articles
#'
#' @description `pub_chunks` makes it easy to extract sections of an article.
#' You can extract just authors across all articles, or all references
#' sections, or the complete text of each article. Then you can pass the
#' output downstream for visualization and analysis.
#'
#' @export
#' @param x An object of class `fulltext::ft_data`, the output from a call to
#' [fulltext::ft_get()]
#' @param what What to get, can be one or more in a vector or list.
#' See Details.
#'
#' @details Options for the `what` parameter:
#' 
#' * front - Publisher, journal and article metadata elements
#' * body - Body of the article
#' * back - Back of the article, acknowledgments, author contributions,
#'  references
#' * title - Article title
#' * doi - Article DOI
#' * categories - Publisher's categories, if any
#' * authors - Authors
#' * keywords - Keywords
#' * abstract - Article abstract
#' * executive_summary - Article executive summary
#' * refs - References
#' * refs_dois - References DOIs - if available
#' * publisher - Publisher name
#' * journal_meta - Journal metadata
#' * article_meta - Article metadata
#' * acknowledgments - Acknowledgments
#' * permissions - Article permissions
#' * history - Dates, recieved, published, accepted, etc.
#'
#' Note that we currently only support PLOS, eLife, Entrez, and Elsevier
#' right now; more to come.
#'
#' @return A list of output, one for each thing requested
#' @examples \dontrun{
#' # new stuff
#' x <- system.file("examples/10_1016_s1569_1993_15_30039_4.xml", 
#'   package = "pubchunks")
#' x <- system.file("examples/10_1016_0021_8928_59_90156_x.xml", 
#'   package = "pubchunks")
#' pub_chunks(x, "elsevier", "abstract")
#' cat(pub_chunks(x, "elsevier", "abstract")[[1]])
#' pub_chunks(x, "elsevier", "title")
#' pub_chunks(x, "elsevier", "authors")
#' pub_chunks(x, "elsevier", "acknowledgments")
#' pub_chunks(x, "elsevier", "refs")
#' 
#' 
#' x <- ft_get('10.1371/journal.pone.0086169', from='plos')
#' pub_chunks(x, what="authors")
#'
#' library("rplos")
#' (dois <- searchplos(q="*:*", fl='id',
#'    fq=list('doc_type:full',"article_type:\"research article\""),
#'      limit=5)$data$id)
#' x <- ft_get(dois, from="plos")
#' x %>% pub_chunks("front")
#' x %>% pub_chunks("body")
#' x %>% pub_chunks("back")
#' x %>% pub_chunks("history")
#' x %>% pub_chunks(c("doi","history")) %>% ft_tabularize()
#' x %>% pub_chunks("authors")
#' x %>% pub_chunks(c("doi","categories"))
#' x %>% pub_chunks("all")
#' x %>% pub_chunks("publisher")
#' x %>% pub_chunks("acknowledgments")
#' x %>% pub_chunks("permissions")
#' x %>% pub_chunks("journal_meta")
#' x %>% pub_chunks("article_meta")
#'
#' # Coerce list output to a data.frame, where possible
#' (dois <- searchplos(q="*:*", fl='id',
#'    fq=list('doc_type:full',"article_type:\"research article\""),
#'      limit=5)$data$id)
#' x <- ft_get(dois, from="plos")
#' x %>% pub_chunks("publisher") %>% ft_tabularize()
#' x %>% pub_chunks("refs") %>% ft_tabularize()
#' x %>% pub_chunks(c("doi","publisher")) %>% ft_tabularize()
#' x %>% pub_chunks(c("doi","publisher","permissions")) %>% ft_tabularize()
#'
#' x <- ft_get(c("10.3389/fnagi.2014.00130",'10.1155/2014/249309',
#'   '10.1155/2014/162024'), from='entrez')
#' x %>% pub_chunks("doi") %>% ft_tabularize()
#' x %>% pub_chunks("authors") %>% ft_tabularize()
#' x %>% pub_chunks(c("doi","publisher","permissions")) %>% ft_tabularize()
#' x %>% pub_chunks("history") %>% ft_tabularize()
#'
#' x <- ft_get('10.3389/fnagi.2014.00130', from='entrez')
#' x %>% pub_chunks("keywords")
#'
#' # Piping workflow
#' opts <- list(fq=list('doc_type:full',"article_type:\"research article\""))
#' ft_search(query='ecology', from='plos', plosopts = opts)$plos$data$id %>%
#'  ft_get(from = "plos") %>%
#'  pub_chunks("publisher")
#'
#' # Via entrez
#' res <- ft_get(c("10.3389/fnagi.2014.00130",'10.1155/2014/249309',
#'    '10.1155/2014/162024'), from='entrez')
#' pub_chunks(res, what="abstract")
#' pub_chunks(res, what="title")
#' pub_chunks(res, what="keywords")
#' pub_chunks(res, what="publisher")
#'
#' (res <- ft_search(query='ecology', from='entrez'))
#' ft_get(res$entrez$data$doi, from='entrez') %>% pub_chunks("title")
#' ft_get(res$entrez$data$doi[1:4], from='entrez') %>%
#'   pub_chunks("acknowledgments")
#' ft_get(res$entrez$data$doi[1:4], from='entrez') %>%
#'   pub_chunks(c('title','keywords'))
#'
#' # From eLife
#' x <- ft_get(c('10.7554/eLife.04251', '10.7554/eLife.04986'), from='elife')
#' x %>% pub_chunks("abstract")
#' x %>% pub_chunks("publisher")
#' x %>% pub_chunks("journal_meta")
#' x %>% pub_chunks("acknowledgments")
#' x %>% pub_chunks("refs_dois")
#' x %>% pub_chunks(c("abstract", "executive_summary"))
#' }
pub_chunks <- function(x, from, what='all') {
  UseMethod("pub_chunks")
}

#' @export
pub_chunks.default <- function(x, from, what='all') {
  stop("no 'pub_chunks' method for ", class(x)[1L])
}

#' @export
pub_chunks.character <- function(x, from, what='all') {
  xml <- xml2::read_xml(x)
  get_what(data = xml, what, from)
}

#' @export
pub_chunks.xml_document <- function(x, from, what='all') {
  x
}

#' @export
pub_chunks.ft_data <- function(x, from, what='all') {
  what <- match.arg(unlist(what), c("all", sections()), TRUE)
  out <- list()
  for (i in seq_along(x)) {
    if (is.null(x[[i]]$found)) {
      out[[names(x[i])]] <- NULL
    } else {
      out[[names(x[i])]] <-
      lapply(x[[i]]$data$data, function(q){
        qparsed <- if (inherits(q, "xml_document")) q else xml2::read_xml(q)
        get_what(data = qparsed, what, names(x[i]))
      })
    }
  }
  out
}
