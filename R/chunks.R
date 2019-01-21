#' @title Extract chunks of data from articles
#'
#' @description `pub_chunks` makes it easy to extract sections of an article.
#' You can extract just authors across all articles, or all references
#' sections, or the complete text of each article. Then you can pass the
#' output downstream for visualization and analysis.
#'
#' @export
#' @param x one of the following:
#' 
#' - file path for an XML file
#' - a character string of XML, a list (of file paths, or XML in a character 
#'   string, or `xml_document` objects)
#' - or an object of class `fulltext::ft_data`, the output from a call to
#'   `fulltext::ft_get()`
#' 
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
#' not in accepted list return `NULL` or zero length list. A ".publisher"
#' list element gets attached to each list output, even when no 
#' data is found. When `fulltext::ft_get` output is passed in here, the 
#' list is named by the publisher, then within each publisher is a list 
#' of articles named by their identifiers (e.g. DOIs). 
#' 
#' @examples
#' # a file path to an XML file
#' x <- system.file("examples/elsevier_1.xml", package = "pubchunks")
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
#' pub_chunks(x, c("doi", "abstract", "title", "authors", "refs", "abstract"))
#' 
#' # eLife
#' x <- system.file("examples/elife_1.xml", package = "pubchunks")
#' pub_chunks(x, "authors")
#' pub_chunks(x, "aff")
#' pub_chunks(x, c("doi", "title", "authors", "refs"))
#' 
#' # f1000research
#' x <- system.file("examples/f1000research_1.xml", package = "pubchunks")
#' pub_chunks(x, "title")
#' pub_chunks(x, "aff")
#' pub_chunks(x, c("doi", "title", "authors", "keywords", "refs"))
#' 
#' # MDPI
#' x <- system.file("examples/mdpi_1.xml", package = "pubchunks")
#' x <- system.file("examples/mdpi_2.xml", package = "pubchunks")
#' pub_chunks(x, "title")
#' pub_chunks(x, "aff")
#' vv <- pub_chunks(x, c("doi", "title", "authors", "keywords", "refs", 
#'   "abstract", "categories"))
#' vv$doi
#' vv$title
#' vv$authors
#' vv$keywords
#' vv$refs
#' vv$abstract
#' vv$categories
#' 
#' # Many inputs at once
#' x <- system.file("examples/frontiers_1.xml", package = "pubchunks")
#' y <- system.file("examples/elife_1.xml", package = "pubchunks")
#' z <- system.file("examples/f1000research_1.xml", package = "pubchunks")
#' pub_chunks(list(x, y, z), c("doi", "title", "authors", "refs"))
#' 
#' # non-XML files/content are xxx?
#' # pub_chunks('foo bar')
#' 
#' # Pubmed brief XML files (abstract only)
#' x <- system.file("examples/pubmed_brief_1.xml", package = "pubchunks")
#' pub_chunks(x, "title")
#' 
#' # Pubmed full XML files
#' x <- system.file("examples/pubmed_full_1.xml", package = "pubchunks")
#' pub_chunks(x, "title")
#' 
#' \dontrun{
#' # using output of fulltext::ft_get()
#' if (requireNamespace("fulltext", quietly = TRUE)) {
#'   library("fulltext")
#' 
#'   # single
#'   x <- fulltext::ft_get('10.7554/eLife.03032')
#'   pub_chunks(fulltext::ft_collect(x), sections="authors")
#' 
#'   # many
#'   dois <- c('10.1371/journal.pone.0086169', '10.1371/journal.pone.0155491', 
#'     '10.7554/eLife.03032')
#'   x <- fulltext::ft_get(dois)
#'   pub_chunks(fulltext::ft_collect(x), sections="authors")
#' 
#'   # as.ft_data() function
#'   x <- ft_collect(as.ft_data())
#'   names(x)
#'   x$cached
#'   pub_chunks(x, "title")
#'   pub_chunks(x, "title") %>% pub_tabularize()
#' }
#' }
pub_chunks <- function(x, sections = "all", provider = NULL) {
  UseMethod("pub_chunks")
}

#' @export
pub_chunks.default <- function(x, sections = "all", provider = NULL) {
  stop("no 'pub_chunks' method for ", class(x)[1L])
}

#' @export
pub_chunks.character <- function(x, sections = "all", provider = NULL) {
  type <- if (file.exists(x)) "file" else "character"
  xml <- xml2::read_xml(x)
  pub <- pgp(xml, provider)
  out <- get_what(data = xml, sections, pub)
  pccat(out, type, sections, fetch_journal(pub, xml))
}

#' @export
pub_chunks.xml_document <- function(x, sections = "all", provider = NULL) {
  pub <- pgp(x, provider)
  out <- get_what(data = x, sections, pub)
  pccat(out, "xml_document", sections, fetch_journal(pub, x))
}

#' @export
pub_chunks.list <- function(x, sections = "all", provider = NULL) {
  tmp <- lapply(x, pub_chunks, sections = sections, provider = provider)
  structure(tmp, ft_data = FALSE)
}

#' @export
pub_chunks.ft_data <- function(x, sections = "all", provider = NULL) {
  sections <- match.arg(unlist(sections), c("all", pub_sections()), TRUE)
  out <- list()
  for (i in seq_along(x)) {
    if (is.null(x[[i]]$found)) {
      out[[names(x[i])]] <- NULL
    } else {
      out[[names(x[i])]] <-
      lapply(x[[i]]$data$data, function(q) {
        if (inherits(q, "xml_document")) {
          pub_chunks(q, sections, names(x[i]))
        } else if (inherits(q, "character") && length(q) == 1) {
          qq <- tryCatch(xml2::read_xml(q), error = function(e) e)
          if (inherits(qq, "error")) {
            pccat(list(.publisher = NA_character_), "empty", "empty", "empty")
          } else {
            pub_chunks(qq, sections, names(x[i]))
          }
        } else {
          pccat(list(.publisher = NA_character_), "empty", "empty", "empty")
        }
      })
    }
  }
  structure(out, ft_data = TRUE)
}


# helpers ------
#' @export
print.pub_chunks <- function(x, ...) {
  cat("<pub chunks>", sep = "\n")
  cat(paste0("  from: ", attr(x, "from")), sep = "\n")
  cat(sprintf("  publisher/journal: %s/%s", x$.publisher, 
    attr(x, "journal_title")), sep = "\n")
  cat(paste0("  sections: ", paste0(attr(x, "sections"), collapse = ", ")),
    sep = "\n")
  cat("  showing up to first 5: ", sep = "\n")
  print_one(x)
}

pccat <- function(x, from, sections, journal) {
  class(x) <- "pub_chunks"
  attr(x, "from") <- from
  attr(x, "sections") <- sections
  attr(x, "journal_title") <- journal
  return(x)
}

fetch_journal <- function(pub, w) {
  pat <- if (pub == "elsevier") "//xocs:srctitle" else "//journal-title"
  xml2::xml_text(xml2::xml_find_first(w, pat))
}

# Doesn't appear to be used
# countem <- function(x) {
#   fr <- attr(x, "from")
#   switch(fr,
#     character = length(x) - 1,
#     list = vapply(x, length, 1) - 1,
#     ft_data = lapply(x, function(z) vapply(z, length, 1) - 1)
#   )
# }

print_one <- function(x) {
  for (i in seq_along(x[1:min(5, length(x))])) {
    if (names(x)[i] != ".publisher") {
      val <- x[[i]] %||% ""
      cnt <- length(val)
      tryval <- tryCatch(val[[1]], error = function(e) e)
      if (inherits(tryval, "list")) {
        val <- "nested list"
      } else if (
        inherits(tryval, "error") || all(is.na(val)) || 
        all(is.null(val)) || all(!nzchar(val))) {

        cnt <- 0
        val <- ""
      } else {
        if (length(val) > 1) {
          if (inherits(val, "list")) val <- val[[1]]
          val <- paste0(val, collapse = ", ")
          if (nchar(val[1]) > 50) {
            val <- substring(val[1], 1, 50)
          }
        }
        if (nchar(val) > 50) {
          val <-  paste0(substring(val, 1, 50), " ...")
        }
      }
      cat(sprintf("   %s (n=%s): %s", names(x)[i], cnt, val), sep = "\n")
    }
  }
}
