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
