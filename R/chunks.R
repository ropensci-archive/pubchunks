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
#' @param sections (character) What elements to get, can be one or more in 
#' a vector or list. See [pub_sections()] for options. optional. Default is 
#' to get all sections. See Details.
#' @param provider (character) a single publisher name. see 
#' [pub_providers()] for options. required. If you select the wrong provider
#' for the XML you have you may or may not get what you need :). By default
#' this is `NULL` and we use [pub_guess_publisher()] to guess the 
#' publisher; we may get it wrong. You can override our guessing by passing
#' in a name.
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
#' * aff - Affiliation (includes author names)
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
#' @return A list, named by the section selected. sections not found or
#' not in accepted list return `NULL` or zero length list
#' 
#' @examples
#' # a file path to an XML file
#' x <- system.file("examples/elsevier_1.xml", package = "pubchunks")
#' pub_chunks(x, "abstract")
#' cat(pub_chunks(x, "abstract")[[1]])
#' pub_chunks(x, "title")
#' pub_chunks(x, "authors")
#' pub_chunks(x, "acknowledgments")
#' pub_chunks(x, "refs")
#' pub_chunks(x, c("title", "refs"))
#' 
#' # works the same with the xml already in a string
#' xml <- paste0(readLines(x), collapse = "")
#' pub_chunks(xml, "title")
#' 
#' # also works if you've already read in the XML (with xml2 pkg)
#' xml <- paste0(readLines(x), collapse = "")
#' xml <- xml2::read_xml(xml)
#' pub_chunks(xml, "title")
#'
#' # Hindawi
#' x <- system.file("examples/hindawi_1.xml", package = "pubchunks")
#' pub_chunks(x, "abstract")
#' pub_chunks(x, "authors")
#' pub_chunks(x, "aff")
#' pub_chunks(x, "title")
#' pub_chunks(x, c("abstract", "title", "authors", "refs"))
#' 
#' # Pensoft
#' x <- system.file("examples/pensoft_1.xml", package = "pubchunks")
#' pub_chunks(x, "abstract")
#' pub_chunks(x, "aff")
#' pub_chunks(x, "title")
#' pub_chunks(x, c("abstract", "title", "authors", "refs"))
#' 
#' # Peerj
#' x <- system.file("examples/peerj_1.xml", package = "pubchunks")
#' pub_chunks(x, "abstract")
#' pub_chunks(x, "authors")
#' pub_chunks(x, "aff")
#' pub_chunks(x, "title")
#' pub_chunks(x, c("abstract", "title", "authors", "refs"))
#' 
#' # Copernicus
#' x <- system.file("examples/copernicus_1.xml", package = "pubchunks")
#' pub_chunks(x, c("doi", "abstract", "title", "authors", "refs"))
#' pub_chunks(x, "aff")
#' 
#' # Frontiers
#' x <- system.file("examples/frontiers_1.xml", package = "pubchunks")
#' pub_chunks(x, "authors")
#' pub_chunks(x, "aff")
#' pub_chunks(x, c("doi", "abstract", "title", "authors", "refs"))
#' 
#' # eLife
#' x <- system.file("examples/elife_1.xml", package = "pubchunks")
#' pub_chunks(x, "authors")
#' pub_chunks(x, "aff")
#' pub_chunks(x, c("doi", "title", "authors", "refs"))
#' 
#' \dontrun{
#' # using output of fulltext::ft_get()
#' if (requireNamespace("fulltext", quietly = TRUE)) {
#'   x <- fulltext::ft_get('10.1371/journal.pone.0086169', from='plos')
#'   pub_chunks(fulltext::ft_collect(x), sections="authors")
#' }
#' }
pub_chunks <- function(x, sections = 'all', provider = NULL) {
  UseMethod("pub_chunks")
}

#' @export
pub_chunks.default <- function(x, sections = 'all', provider = NULL) {
  stop("no 'pub_chunks' method for ", class(x)[1L])
}

#' @export
pub_chunks.character <- function(x, sections = 'all', provider = NULL) {
  xml <- xml2::read_xml(x)
  get_what(data = xml, sections, pgp(xml, provider))
}

#' @export
pub_chunks.xml_document <- function(x, sections = 'all', provider = NULL) {
  get_what(data = x, sections, pgp(x, provider))
}

#' @export
pub_chunks.ft_data <- function(x, sections = 'all', provider = NULL) {
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
