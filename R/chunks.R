#' @title Extract chunks of data from articles
#'
#' @description `pub_chunks` makes it easy to extract sections of an article.
#' You can extract just authors across all articles, or all references
#' sections, or the complete text of each article. Then you can pass the
#' output downstream for visualization and analysis.
#'
#' @export
#' @param x A file path for an XML file, or a character string of XML, 
#' or an object of class `fulltext::ft_data`, the output from a call to
#' [fulltext::ft_get()]
#' @param provider (character) the publisher. a single option only. see 
#' [pub_providers()] for options. required. If you select the wrong provider
#' for the XML you have you may or may not get what you need :)
#' @param sections (character) What elements to get, can be one or more in 
#' a vector or list. See [pub_sections()] for options. optional. Default is 
#' to get all sections. See Details.
#'
#' @details Options for the `sections` parameter:
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
#' Note that we currently only support PLOS, eLife, Entrez, and Elsevier;
#' more to come.
#'
#' @return A list, named by the section selected. sections not found or
#' not in accepted list return `NULL`
#' 
#' @examples \dontrun{
#' # a file path to an XML file
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
#' pub_chunks(x, "elsevier", c("title", "refs"))
#' 
#' # works the same with the xml already in a string
#' xml <- paste0(readLines(x), collapse = "")
#' pub_chunks(xml, "elsevier", "title")
#' 
#' # also works if you've already read in the XML (with xml2 pkg)
#' xml <- paste0(readLines(x), collapse = "")
#' xml <- xml2::read_xml(xml)
#' pub_chunks(xml, "elsevier", "title")
#' 
#' # using output of fulltext::ft_get()
#' if (requireNamespace("fulltext")) {
#'   x <- fulltext::ft_get('10.1371/journal.pone.0086169', from='plos')
#'   pub_chunks(fulltext::ft_collect(x), sections="authors")
#' }
#' 
#' }
pub_chunks <- function(x, provider, sections = 'all') {
  UseMethod("pub_chunks")
}

#' @export
pub_chunks.default <- function(x, provider, sections = 'all') {
  stop("no 'pub_chunks' method for ", class(x)[1L])
}

#' @export
pub_chunks.character <- function(x, provider, sections = 'all') {
  xml <- xml2::read_xml(x)
  get_what(data = xml, sections, provider)
}

#' @export
pub_chunks.xml_document <- function(x, provider, sections = 'all') {
  get_what(data = x, sections, provider)
}

#' @export
pub_chunks.ft_data <- function(x, provider, sections = 'all') {
  sections <- match.arg(unlist(sections), c("all", pub_sections()), TRUE)
  out <- list()
  for (i in seq_along(x)) {
    if (is.null(x[[i]]$found)) {
      out[[names(x[i])]] <- NULL
    } else {
      out[[names(x[i])]] <-
      lapply(x[[i]]$data$data, function(q){
        qparsed <- if (inherits(q, "xml_document")) q else xml2::read_xml(q)
        get_what(data = qparsed, sections, names(x[i]))
      })
    }
  }
  out
}
