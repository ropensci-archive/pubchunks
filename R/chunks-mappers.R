title <- function(b, from, extract){
  switch(
    from,
    elife = f1txt(b, "//title-group/article-title", extract),
    plos = f1txt(b, "//title-group/article-title", extract),
    entrez = f1txt(b, "//title-group/article-title", extract),
    elsevier = f1txt(b, "//dc:title", extract),
    hindawi = f1txt(b, "//article-title", extract),
    pensoft = f1txt(b, "//article-title", extract),
    peerj = f1txt(b, "//article-title", extract),
    copernicus = f1txt(b, "//article-title", extract),
    frontiers = f1txt(b, "//article-title", extract),
    f1000research = f1txt(b, "//article-title", extract),
    f1txt(b, "//article-title", extract) %|na|% f1txt(b, "//Article//Title", extract)
  )
}

doi <- function(b, from, extract){
  switch(
    from,
    elife = f1txt(b, "//article-id[@pub-id-type='doi']", extract),
    plos = f1txt(b, "//article-id[@pub-id-type='doi']", extract),
    entrez = f1txt(b, "//article-id[@pub-id-type='doi']", extract),
    elsevier = f1txt(b, "//dc:identifier", extract),
    copernicus = f1txt(b, "//article-id[@pub-id-type='doi']", extract),
    frontiers = f1txt(b, "//article-id[@pub-id-type='doi']", extract),
    f1000research = f1txt(b, "//article-id[@pub-id-type='doi']", extract),
    f1txt(b, "//article-id[@pub-id-type='doi']", extract) %|na|%
      f1txt(b, "//ArticleId[@IdType='doi']", extract)
  )
}

categories <- function(b, from, extract){
  switch(
    from,
    elife = xml2::xml_text(xml2::xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
    plos = xml2::xml_text(xml2::xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
    entrez = xml2::xml_text(xml2::xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
    elsevier = falltxt(b, "//dcterms:subject"),
    frontiers = xml2::xml_text(xml2::xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
    f1000research = xml2::xml_text(xml2::xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
    {
      bb <- xml2::xml_find_all(b, "//article-categories")
      if (!length(bb) == 0) xml2::xml_text(xml2::xml_find_all(bb[[1]], "//subject")) else NULL
    }
  )
}

authors <- function(b, from, extract){
  get_auth <- function(v, extract) {
    tmp <- xml2::xml_find_all(v, "//contrib[@contrib-type='author']")
    lapply(tmp, function(z){
      list(given_names = f1txt(z, "name/given-names", extract),
           surname = f1txt(z, "name/surname", extract))
    })
  }
  switch(
    from,
    elife = get_auth(b, extract),
    plos = get_auth(b, extract),
    entrez = get_auth(b, extract),
    elsevier = falltxt(b, "//dc:creator", extract),
    hindawi = get_auth(b, extract),
    pensoft = get_auth(b, extract),
    peerj = get_auth(b, extract),
    copernicus = get_auth(b, extract),
    frontiers = get_auth(b, extract),
    f1000research = get_auth(b, extract),
    get_auth(b, extract) %|na|% xml2::xml_find_all(b, "//AuthorList//Author")
  )
}

aff <- function(b, from, extract){
  get_aff <- function(v, extract) {
    # affiliations
    affs <- xml2::xml_find_all(v, "//aff")
    affs_ <- merge_node_groups(affs, extract)
    
    # authors
    auths <- xml2::xml_find_all(v, "//contrib[@contrib-type='author']")
    auths_ <- lapply(auths, function(z){
      list(
        given_names = f1txt(z, "name/given-names", extract),
        surname = f1txt(z, "name/surname", extract),
        id = xml2::xml_attr(
          xml2::xml_find_first(z, "xref[@ref-type=\"aff\"]"), "rid")
      )
    })
    apply(merge(rbl(affs_), rbl(auths_), by = "id"), 1, as.list)
  }
  switch(
    from,
    elife = get_aff(b, extract),
    plos = get_aff(b, extract),
    entrez = get_aff(b, extract),
    elsevier = NULL,
    hindawi = get_aff(b, extract),
    pensoft = get_aff(b, extract),
    peerj = get_aff(b, extract),
    copernicus = NULL,
    frontiers = get_aff(b, extract),
    f1000research = get_aff(b, extract),
    get_aff(b, extract)
  )
}

merge_node_groups <- function(x, extract) {
  lapply(x, function(z) {
    nms <- xml2::xml_name(xml2::xml_children(z))
    nms <- grep("sup", nms, invert = TRUE, value = TRUE)
    nms <- unique(nms)
    c(
      stats::setNames(
        lapply(nms, function(w) paste0(falltxt(z, w, extract), collapse = ", ")), 
        nms),
      id = xml2::xml_attr(z, "id")
    )
  })
}

keywords <- function(b, from, extract){
  switch(
    from,
    elife = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group[@kwd-group-type='author-keywords']/kwd")),
    plos = NULL,
    entrez = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group/kwd")),
    elsevier = falltxt(b, "//ce:keyword"),
    f1000research = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group/kwd")),
    mdpi = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group/kwd")),
    xml2::xml_text(xml2::xml_find_all(b, "//kwd-group/kwd"))
  )
}

body <- function(b, from, extract){
  switch(
    from,
    elife = xml2::xml_text(xml2::xml_find_all(b, "//body//p")),
    plos = xml2::xml_text(xml2::xml_find_all(b, "//body//p")),
    elsevier = {
      xml2::xml_ns_strip(b)
      falltxt(b, "//body", extract)
    },
    f1000research = xml2::xml_text(xml2::xml_find_all(b, "//body//p")),
    xml2::xml_text(xml2::xml_find_all(b, "//body//p"))
  )
}

abstract <- function(b, from, extract){
  switch(
    from,
    elife = exfun(extract)(
      xml2::xml_find_all(
        xml2::xml_find_all(b, 
          '//abstract[@hwp:id="abstract-1"]', ns = xml2::xml_ns(b))[[1]], "p")[1]),
    plos = falltxt(b, "//abstract", extract),
    entrez = falltxt(b, "//abstract", extract),
    elsevier = f1txt(b, "//dc:description", extract),
    hindawi = f1txt(b, "//abstract", extract),
    pensoft = f1txt(b, "//abstract", extract),
    peerj = f1txt(b, "//abstract", extract),
    copernicus = f1txt(b, "//abstract", extract),
    frontiers = f1txt(b, "//abstract", extract),
    f1000research = f1txt(b, "//abstract", extract),
    f1txt(b, "//abstract", extract)
  )
}

exec_summary <- function(b, from, extract){
  switch(
    from,
    elife = {
      tmp <- 
        exfun(extract)(
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

refs_dois <- function(b, from, extract){
  switch(
    from,
    elife = falltxt(b, "//ref-list/ref//pub-id[@pub-id-type='doi']", extract),
    plos = NULL,
    entrez = NULL,
    elsevier = NULL,
    f1000research = falltxt(b, "//ref-list/ref//pub-id[@pub-id-type='doi']", extract)
  )
}

refs <- function(b, from, extract){
  switch(
    from,
    elife = refs_reflist(b, "//ref-list/ref"),
    plos = falltxt(b, "//ref-list/ref/mixed-citation", extract),
    entrez = falltxt(b, "//ref-list/ref", extract),
    elsevier = refs_txt(b, "//ce:bib-reference"),
    hindawi = refs_reflist(b, "//ref-list/ref"),
    pensoft = refs_pensoft(b, "//ref-list/ref"),
    peerj = refs_reflist(b, "//ref-list/ref"),
    copernicus = falltxt(b, "//ref-list/ref", extract),
    frontiers = refs_reflist(b, "//ref-list/ref", typex = "citation"),
    f1000research = refs_reflist(b, "//ref-list/ref", typex = "mixed_citation"),
    mdpi = refs_reflist(b, "//ref-list/ref"),
    falltxt(b, "//ref-list/ref", extract)
  )
}

publisher <- function(b, from, extract){
  switch(
    from,
    elife = falltxt(b, "//publisher/publisher-name", extract),
    plos = falltxt(b, "//publisher/publisher-name", extract),
    entrez = falltxt(b, "//publisher/publisher-name", extract),
    elsevier = f1txt(b, "//prism:publisher", extract),
    copernicus = f1txt(b, "//publisher/publisher-name", extract),
    frontiers = f1txt(b, "//publisher/publisher-name", extract),
    f1000research = f1txt(b, "//publisher/publisher-name", extract),
    f1txt(b, "//publisher/publisher-name", extract)
  )
}

journal_meta <- function(b, from, extract){
  switch(
    from,
    elife = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    plos = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    entrez = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    elsevier = NULL,
    copernicus = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    frontiers = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    f1000research = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse)
  )
}

article_meta <- function(b, from, extract){
  switch(
    from,
    elife = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    plos = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    entrez = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    elsevier = NULL,
    copernicus = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    frontiers = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    f1000research = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse)
  )
}

acknowledgments <- function(b, from, extract){
  switch(from,
         elife = falltxt(b, "//ack/p", extract),
         plos = falltxt(b, "//ack/p", extract),
         entrez = falltxt(b, "//ack/p", extract),
         elsevier = f1txt(b, "//ce:acknowledgment", extract),
         falltxt(b, "//ack/p", extract)
  )
}

permissions <- function(b, from, extract){
  switch(from,
         elife = getperms(b),
         plos = getperms(b),
         entrez = getperms(b),
         elsevier = {
            tmp <- xml2::xml_find_first(b, "//xocs:copyright-info")
            list(
              copyright_text = f1txt(tmp, "xocs:cp-license-lines", extract),
              copyright_notice = f1txt(tmp, "xocs:cp-notices", extract)
            )
         },
         copernicus = getperms(b),
         frontiers = getperms(b),
         f1000research = getperms(b),
         getperms(b)
  )
}

getperms <- function(v){
  tmp <- sapply(xml2::xml_children(xml2::xml_find_first(v, "//permissions")), xml_node_parse)
  tmp$license <- paste0(tmp$license[[1]], collapse = " ")
  lichref <- tryCatch(xml2::xml_attr(xml2::xml_find_all(v, "//permissions/license//ext-link"), "href"), error = function(e) e)
  tmp$license_url <- if (inherits(lichref, "simpleError") || length(lichref) == 0) NA else lichref
  lapply(tmp, strtrim)
}

front <- function(b, from, extract){
  switch(from,
         elife = get_forb(b, "//front", extract),
         plos = get_forb(b, "//front", extract),
         entrez = get_forb(b, "//front", extract),
         elsevier = NULL,
         copernicus = get_forb(b, "//front", extract),
         frontiers = get_forb(b, "//front", extract),
         f1000research = get_forb(b, "//front", extract),
         get_forb(b, "//front", extract)
  )
}

back <- function(b, from, extract){
  switch(from,
         elife = get_forb(b, "//back", extract),
         plos = get_forb(b, "//back", extract),
         entrez = get_forb(b, "//back", extract),
         elsevier = NULL,
         copernicus = get_forb(b, "//back", extract),
         frontiers = get_forb(b, "//back", extract),
         f1000research = get_forb(b, "//back", extract),
         get_forb(b, "//back", extract)
  )
}

get_forb <- function(x, fb, extract) {
  tmp <- xml2::xml_children(xml2::xml_find_all(x, fb))
  lapply(tmp, function(z) {
    lapply(xml2::xml_children(z), xml_node_parse, extract=extract)
  })
}

history <- function(b, from, extract){
  switch(from,
         elife = history2date(b),
         plos = history2date(b),
         entrez = history2date(b),
         elsevier = NULL,
         frontiers = history2date(b),
         f1000research = history2date(b),
         history2date(b)
  )
}

history2date <- function(r){
  tmp <- xml2::xml_find_all(r, "//history/date")
  out <- lapply(tmp, function(rr){
    as.Date(paste0(sapply(c('day','month','year'), function(vv) f1txt(rr, vv)), collapse = "-"), "%d-%m-%Y")
  })
  stats::setNames(out, sapply(tmp, xml2::xml_attr, attr = "date-type"))
}

f1txt <- function(x, xpath, extract="xml_text") {
  vapply(exfun(extract)(xml2::xml_find_first(x, xpath)), strtrim, "", 
    USE.NAMES = FALSE)
}

falltxt <- function(x, xpath, extract="xml_text") {
  vapply(exfun(extract)(xml2::xml_find_all(x, xpath)), strtrim, "", 
    USE.NAMES = FALSE)
}

exfun <- function(x) {
  fun <- switch(x, xml_text = "xml2::xml_text", as.character = "as.character",
    stop("'extract' must be one of 'xml_text' or 'as.character'"))
  eval(parse(text=fun))
}

refs_txt <- function(x, xpath) {
  b <- xml2::xml_find_all(x, xpath)
  lapply(b, function(z) {
    auths <- xml2::xml_find_all(z, ".//sb:author")
    if (length(auths) == 0) {
      txt <- xml2::xml_text(xml2::xml_find_first(z, ".//ce:other-ref"))
      return(txt)
    }
    auths_others <- auths[-1]
    auths_others <- lapply(auths_others, function(w) {
      paste0(vapply(xml2::xml_children(w), xml2::xml_text, ""), collapse = " ")
    })
    auths1_child <- xml2::xml_children(auths[1])
    auth1 <- paste0(c(xml2::xml_text(auths1_child[2]), xml2::xml_text(auths1_child[1])), collapse=" ")
    auths <- paste0(c(auth1, unlist(auths_others)), collapse=", ")
    title <- xml2::xml_text(xml2::xml_find_first(z, ".//sb:contribution//sb:maintitle"))
    journal <- xml2::xml_text(xml2::xml_find_first(z, ".//sb:host//sb:maintitle"))
    vol <- xml2::xml_text(xml2::xml_find_first(z, ".//sb:host//sb:volume-nr"))
    date <- xml2::xml_text(xml2::xml_find_first(z, ".//sb:host//sb:date"))
    pages <- paste0(vapply(xml2::xml_children(xml2::xml_find_first(z, ".//sb:host//sb:pages")), xml2::xml_text, ""), collapse = "-")
    sprintf("%s. %s. %s. %s %s, %s", auths, date, title, journal, vol, pages)
  })
}

refs_reflist <- function(x, xpath, typex = "element_citation") {
  b <- xml2::xml_find_all(x, xpath)
  lapply(b, function(z) {
    auths <- xml2::xml_find_all(z, ".//person-group/name")
    if (length(auths) == 0) {
      txt <- xml2::xml_text(suppressWarnings(xml2::xml_find_first(z, ".//ce:other-ref")))
      return(txt)
    }
    auths_others <- auths[-1]
    auths_others <- lapply(auths_others, function(w) {
      paste0(vapply(xml2::xml_children(w), xml2::xml_text, ""), collapse = " ")
    })
    auths1_child <- xml2::xml_children(auths[1])
    auth1 <- paste0(c(xml2::xml_text(auths1_child[1]), xml2::xml_text(auths1_child[2])), collapse=" ")
    auths <- paste0(c(auth1, unlist(auths_others)), collapse=", ")
    date <- xml2::xml_text(xml2::xml_find_first(z, ".//year"))

    type <- switch(
      typex,
      element_citation = 
        xml2::xml_attr(xml2::xml_find_first(z, ".//element-citation"), "publication-type"),
      citation = 
        xml2::xml_attr(xml2::xml_find_first(z, ".//citation"), "citation-type"),
      mixed_citation = 
        xml2::xml_attr(xml2::xml_find_first(z, ".//mixed-citation"), "publication-type")
    )

    source <- xml2::xml_text(xml2::xml_find_first(z, ".//source"))

    if (type == "book") {
      publisher <- xml2::xml_text(xml2::xml_find_first(z, ".//publisher-name"))
      sprintf("%s %s. %s. %s", auths, date, source, publisher)
    } else if (type == "journal") {
      article_title <- xml2::xml_text(xml2::xml_find_first(z, ".//article-title"))
      vol <- xml2::xml_text(xml2::xml_find_first(z, ".//volume"))
      pages <- paste0(
        stats::na.omit(vapply(c("fpage", "lpage"), function(w) xml2::xml_text(xml2::xml_find_first(z, paste0(".//", w))), "")),
        collapse = "-")
      doi <- xml2::xml_text(xml2::xml_find_first(z, ".//pub-id[@pub-id-type=\"doi\"]")) %|na|% ""
      if (nzchar(doi)) doi <- sprintf("(https://doi.org/%s)", doi)
      sprintf("%s %s. %s %s %s, %s %s", auths, date, article_title, source, vol, pages, doi)
    }
  })
}

refs_pensoft <- function(x, xpath) {
  b <- xml2::xml_find_all(x, xpath)
  lapply(b, function(z) {
    auths <- xml2::xml_find_all(z, ".//person-group/name")
    if (length(auths) == 0) {
      txt <- xml2::xml_text(xml2::xml_find_first(z, ".//ce:other-ref"))
      return(txt)
    }
    auths_others <- auths[-1]
    auths_others <- lapply(auths_others, function(w) {
      paste0(vapply(xml2::xml_children(w), xml2::xml_text, ""), collapse = " ")
    })
    auths1_child <- xml2::xml_children(auths[1])
    if (length(auths1_child) == 1) {
      auth1 <- xml2::xml_text(auths1_child[1])
    } else {
      auth1 <- paste0(c(xml2::xml_text(auths1_child[1]), xml2::xml_text(auths1_child[2])), collapse=" ")
    }
    auths <- paste0(c(auth1, unlist(auths_others)), collapse=", ")
    date <- xml2::xml_text(xml2::xml_find_first(z, ".//year"))
    source <- xml2::xml_text(xml2::xml_find_first(z, ".//source"))
    article_title <- xml2::xml_text(xml2::xml_find_first(z, ".//article-title"))
    if (length(xml2::xml_find_first(z, ".//publisher-name")) > 0) {
      publisher <- xml2::xml_text(xml2::xml_find_first(z, ".//publisher-name"))
      sprintf("%s %s. %s %s", auths, date, article_title, publisher)
    } else {
      vol <- xml2::xml_text(xml2::xml_find_first(z, ".//volume"))
      pages <- paste0(
        na.omit(vapply(c("fpage", "lpage"), function(w) xml2::xml_text(xml2::xml_find_first(z, paste0(".//", w))), "")),
        collapse = "-")
      doi <- xml2::xml_text(xml2::xml_find_first(z, ".//ext-link")) %|na|% ""
      if (nzchar(doi)) doi <- sprintf("(https://doi.org/%s)", doi)
      sprintf("%s %s. %s %s %s, %s %s", auths, date, article_title, source, vol, pages, doi)
    }
  })
}
