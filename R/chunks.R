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
#' x <- system.file("examples/10_1016_s1569_1993_15_30039_4.xml", package = "pubchunks")
#' pub_chunks(x, "elsevier", "abstract")
#' cat(pub_chunks(x, "elsevier", "abstract")[[1]])
#' pub_chunks(x, "elsevier", "title")
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

sections <- function() {
  c("front","body","back","title","doi","categories",
    "authors","keywords",
    "abstract","executive_summary","refs","refs_dois",
    "publisher","journal_meta","article_meta",
    "acknowledgments","permissions","history")
}

get_what <- function(data, what, from){
  if ( any(what == "all") ) what <- sections()
  setNames(lapply(what, function(z){
    switch(z,
           front = front(data, from),
           body = body(data, from),
           back = back(data, from),
           title = title(data, from),
           abstract = abstract(data, from),
           executive_summary = exec_summary(data, from),
           doi = doi(data, from),
           categories = categories(data, from),
           authors = authors(data, from),
           keywords = keywords(data, from),
           refs_dois = refs_dois(data, from),
           refs = refs(data, from),
           publisher = publisher(data, from),
           journal_meta = journal_meta(data, from),
           article_meta = article_meta(data, from),
           acknowledgments = acknowledgments(data, from),
           permissions = permissions(data, from),
           history = history(data, from)
    )
  }), what)
}

title <- function(b, from){
  switch(from,
         elife = f1txt(b, "//title-group/article-title"),
         plos = f1txt(b, "//title-group/article-title"),
         entrez = f1txt(b, "//title-group/article-title"),
         elsevier = f1txt(b, "//dc:title")
  )
}

doi <- function(b, from){
  switch(from,
         elife = f1txt(b, "//article-id[@pub-id-type='doi']"),
         plos = f1txt(b, "//article-id[@pub-id-type='doi']"),
         entrez = f1txt(b, "//article-id[@pub-id-type='doi']"),
         elsevier = f1txt(b, "//dc:identifier")
  )
}

categories <- function(b, from){
  switch(from,
         elife = xml2::xml_text(xml2::xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
         plos = xml2::xml_text(xml2::xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
         entrez = xml2::xml_text(xml2::xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
         elsevier = falltxt(b, "//dcterms:subject")
  )
}

authors <- function(b, from){
  get_auth <- function(v){
    tmp <- xml2::xml_find_all(v, "//contrib[@contrib-type='author']")
    lapply(tmp, function(z){
      list(given_names = f1txt(z, "name/given-names"),
           surname = f1txt(z, "name/surname"))
    })
  }
  switch(
    from,
    elife = get_auth(b),
    plos = get_auth(b),
    entrez = get_auth(b),
    elsevier = falltxt(b, "//dc:creator")
  )
}

keywords <- function(b, from){
  switch(
    from,
    elife = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group[@kwd-group-type='author-keywords']/kwd")),
    plos = NULL,
    entrez = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group/kwd")),
    elsevier = falltxt(b, "//ce:keyword")
  )
}

body <- function(b, from){
  switch(
    from,
    elife = xml2::xml_text(xml2::xml_find_all(b, "//body//p")),
    plos = xml2::xml_text(xml2::xml_find_all(b, "//body//p")),
    elsevier = {
      xml2::xml_ns_strip(b)
      falltxt(b, "//body")
    }
  )
}

abstract <- function(b, from){
  switch(
    from,
    elife = xml2::xml_text(
      xml2::xml_find_all(
        xml2::xml_find_all(b, 
          '//abstract[@hwp:id="abstract-1"]', ns = xml2::xml_ns(b))[[1]], "p")[1]),
    plos = falltxt(b, "//abstract"),
    entrez = falltxt(b, "//abstract"),
    elsevier = f1txt(b, "//dc:description")
  )
}

exec_summary <- function(b, from){
  switch(
    from,
    elife = {
      tmp <- 
        xml2::xml_text(
          xml2::xml_find_all(b, 
            '//abstract[@abstract-type="executive-summary"]/p', 
            ns = xml2::xml_ns(b)))
      tmp[-length(tmp)]
    },
    plos = NULL,
    entrez = NULL,
    elsevier = NULL
  )
}

refs_dois <- function(b, from){
  switch(
    from,
    elife = falltxt(b, "//ref-list/ref//pub-id[@pub-id-type='doi']"),
    plos = NULL,
    entrez = NULL,
    elsevier = NULL
  )
}

refs <- function(b, from){
  switch(
    from,
    elife = NULL,
    plos = falltxt(b, "//ref-list/ref/mixed-citation"),
    entrez = falltxt(b, "//ref-list/ref"),
    elsevier = falltxt(b, "//ce:bib-reference")
  )
}

publisher <- function(b, from){
  switch(
    from,
    elife = falltxt(b, "//publisher"),
    plos = falltxt(b, "//publisher"),
    entrez = falltxt(b, "//publisher"),
    elsevier = f1txt(b, "//prism:publisher")
  )
}

journal_meta <- function(b, from){
  switch(
    from,
    elife = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    plos = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    entrez = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    elsevier = NULL
  )
}

article_meta <- function(b, from){
  switch(
    from,
    elife = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    plos = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    entrez = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    elsevier = NULL
  )
}

acknowledgments <- function(b, from){
  switch(from,
         elife = falltxt(b, "//ack/p"),
         plos = falltxt(b, "//ack/p"),
         entrez = falltxt(b, "//ack/p"),
         elsevier = f1txt(b, "//ce:acknowledgment")
  )
}

permissions <- function(b, from){
  switch(from,
         elife = getperms(b),
         plos = getperms(b),
         entrez = getperms(b),
         elsevier = {
            tmp <- xml2::xml_find_first(b, "//xocs:copyright-info")
            list(
              copyright_text = f1txt(tmp, "xocs:cp-license-lines"),
              copyright_notice = f1txt(tmp, "xocs:cp-notices")
            )
         }
  )
}

getperms <- function(v){
  tmp <- sapply(xml2::xml_children(xml2::xml_find_first(v, "//permissions")), xml_node_parse)
  tmp$license <- paste0(tmp$license[[1]], collapse = " ")
  lichref <- tryCatch(xml2::xml_attr(xml2::xml_find_all(v, "//permissions/license//ext-link"), "href"), error = function(e) e)
  tmp$license_url <- if (inherits(lichref, "simpleError") || length(lichref) == 0) NA else lichref
  lapply(tmp, strtrim)
}

front <- function(b, from){
  switch(from,
         elife = get_forb(b, "//front"),
         plos = get_forb(b, "//front"),
         entrez = get_forb(b, "//front"),
         elsevier = NULL
  )
}

back <- function(b, from){
  switch(from,
         elife = get_forb(b, "//back"),
         plos = get_forb(b, "//back"),
         entrez = get_forb(b, "//back"),
         elsevier = NULL
  )
}

get_forb <- function(x, fb) {
  tmp <- xml2::xml_children(xml2::xml_find_all(x, fb))
  lapply(tmp, function(z) {
    lapply(xml2::xml_children(z), xml_node_parse)
  })
}

history <- function(b, from){
  switch(from,
         elife = history2date(b),
         plos = history2date(b),
         entrez = history2date(b),
         elsevier = NULL
  )
}

history2date <- function(r){
  tmp <- xml2::xml_find_all(r, "//history/date")
  out <- lapply(tmp, function(rr){
    as.Date(paste0(sapply(c('day','month','year'), function(vv) f1txt(rr, vv)), collapse = "-"), "%d-%m-%Y")
  })
  stats::setNames(out, sapply(tmp, xml2::xml_attr, attr = "date-type"))
}

f1txt <- function(x, xpath) {
  vapply(xml2::xml_text(xml2::xml_find_first(x, xpath)), strtrim, "", 
    USE.NAMES = FALSE)
}

falltxt <- function(x, xpath) {
  vapply(xml2::xml_text(xml2::xml_find_all(x, xpath)), strtrim, "", 
    USE.NAMES = FALSE)
}

is_ft_data <- function(x) {
  if (!inherits(x, "ft_data")) stop("Input to x must be of class ft_data", 
    call. = FALSE)
}
