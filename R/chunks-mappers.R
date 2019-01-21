title <- function(b, from){
  switch(
    from,
    elife = f1txt(b, "//title-group/article-title"),
    plos = f1txt(b, "//title-group/article-title"),
    entrez = f1txt(b, "//title-group/article-title"),
    elsevier = f1txt(b, "//dc:title"),
    hindawi = f1txt(b, "//article-title"),
    pensoft = f1txt(b, "//article-title"),
    peerj = f1txt(b, "//article-title"),
    copernicus = f1txt(b, "//article-title"),
    frontiers = f1txt(b, "//article-title"),
    f1000research = f1txt(b, "//article-title"),
    f1txt(b, "//article-title") %|na|% f1txt(b, "//Article//Title")
  )
}

doi <- function(b, from){
  switch(
    from,
    elife = f1txt(b, "//article-id[@pub-id-type='doi']"),
    plos = f1txt(b, "//article-id[@pub-id-type='doi']"),
    entrez = f1txt(b, "//article-id[@pub-id-type='doi']"),
    elsevier = f1txt(b, "//dc:identifier"),
    copernicus = f1txt(b, "//article-id[@pub-id-type='doi']"),
    frontiers = f1txt(b, "//article-id[@pub-id-type='doi']"),
    f1000research = f1txt(b, "//article-id[@pub-id-type='doi']"),
    f1txt(b, "//article-id[@pub-id-type='doi']") %|na|% f1txt(b, "//ArticleId[@IdType='doi']")
  )
}

categories <- function(b, from){
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
    elsevier = falltxt(b, "//dc:creator"),
    hindawi = get_auth(b),
    pensoft = get_auth(b),
    peerj = get_auth(b),
    copernicus = get_auth(b),
    frontiers = get_auth(b),
    f1000research = get_auth(b),
    get_auth(b) %|na|% xml2::xml_find_all(b, "//AuthorList//Author")
  )
}

aff <- function(b, from){
  get_aff <- function(v){
    # affiliations
    affs <- xml2::xml_find_all(v, "//aff")
    affs_ <- merge_node_groups(affs)
    
    # authors
    auths <- xml2::xml_find_all(v, "//contrib[@contrib-type='author']")
    auths_ <- lapply(auths, function(z){
      list(
        given_names = f1txt(z, "name/given-names"),
        surname = f1txt(z, "name/surname"),
        id = xml2::xml_attr(
          xml2::xml_find_first(z, "xref[@ref-type=\"aff\"]"), "rid")
      )
    })
    apply(merge(rbl(affs_), rbl(auths_), by = "id"), 1, as.list)
  }
  switch(
    from,
    elife = get_aff(b),
    plos = get_aff(b),
    entrez = get_aff(b),
    elsevier = NULL,
    hindawi = get_aff(b),
    pensoft = get_aff(b),
    peerj = get_aff(b),
    copernicus = NULL,
    frontiers = get_aff(b),
    f1000research = get_aff(b),
    get_aff(b)
  )
}

merge_node_groups <- function(x) {
  lapply(x, function(z) {
    nms <- xml2::xml_name(xml2::xml_children(z))
    nms <- grep("sup", nms, invert = TRUE, value = TRUE)
    nms <- unique(nms)
    c(
      stats::setNames(
        lapply(nms, function(w) paste0(falltxt(z, w), collapse = ", ")), 
        nms),
      id = xml2::xml_attr(z, "id")
    )
  })
}

keywords <- function(b, from){
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

body <- function(b, from){
  switch(
    from,
    elife = xml2::xml_text(xml2::xml_find_all(b, "//body//p")),
    plos = xml2::xml_text(xml2::xml_find_all(b, "//body//p")),
    elsevier = {
      xml2::xml_ns_strip(b)
      falltxt(b, "//body")
    },
    f1000research = xml2::xml_text(xml2::xml_find_all(b, "//body//p")),
    xml2::xml_text(xml2::xml_find_all(b, "//body//p"))
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
    elsevier = f1txt(b, "//dc:description"),
    hindawi = f1txt(b, "//abstract"),
    pensoft = f1txt(b, "//abstract"),
    peerj = f1txt(b, "//abstract"),
    copernicus = f1txt(b, "//abstract"),
    frontiers = f1txt(b, "//abstract"),
    f1000research = f1txt(b, "//abstract"),
    f1txt(b, "//abstract")
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
    elsevier = NULL,
    f1000research = falltxt(b, "//ref-list/ref//pub-id[@pub-id-type='doi']")
  )
}

refs <- function(b, from){
  switch(
    from,
    elife = falltxt(b, "//ref-list/ref"),
    plos = falltxt(b, "//ref-list/ref/mixed-citation"),
    entrez = falltxt(b, "//ref-list/ref"),
    elsevier = falltxt(b, "//ce:bib-reference"),
    hindawi = falltxt(b, "//ref-list/ref"),
    pensoft = falltxt(b, "//ref-list/ref"),
    peerj = falltxt(b, "//ref-list/ref"),
    copernicus = falltxt(b, "//ref-list/ref"),
    frontiers = falltxt(b, "//ref-list/ref"),
    f1000research = falltxt(b, "//ref-list/ref"),
    falltxt(b, "//ref-list/ref")
  )
}

publisher <- function(b, from){
  switch(
    from,
    elife = falltxt(b, "//publisher/publisher-name"),
    plos = falltxt(b, "//publisher/publisher-name"),
    entrez = falltxt(b, "//publisher/publisher-name"),
    elsevier = f1txt(b, "//prism:publisher"),
    copernicus = f1txt(b, "//publisher/publisher-name"),
    frontiers = f1txt(b, "//publisher/publisher-name"),
    f1000research = f1txt(b, "//publisher/publisher-name"),
    f1txt(b, "//publisher/publisher-name")
  )
}

journal_meta <- function(b, from){
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

article_meta <- function(b, from){
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

acknowledgments <- function(b, from){
  switch(from,
         elife = falltxt(b, "//ack/p"),
         plos = falltxt(b, "//ack/p"),
         entrez = falltxt(b, "//ack/p"),
         elsevier = f1txt(b, "//ce:acknowledgment"),
         falltxt(b, "//ack/p")
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

front <- function(b, from){
  switch(from,
         elife = get_forb(b, "//front"),
         plos = get_forb(b, "//front"),
         entrez = get_forb(b, "//front"),
         elsevier = NULL,
         copernicus = get_forb(b, "//front"),
         frontiers = get_forb(b, "//front"),
         f1000research = get_forb(b, "//front"),
         get_forb(b, "//front")
  )
}

back <- function(b, from){
  switch(from,
         elife = get_forb(b, "//back"),
         plos = get_forb(b, "//back"),
         entrez = get_forb(b, "//back"),
         elsevier = NULL,
         copernicus = get_forb(b, "//back"),
         frontiers = get_forb(b, "//back"),
         f1000research = get_forb(b, "//back"),
         get_forb(b, "//back")
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

f1txt <- function(x, xpath) {
  vapply(xml2::xml_text(xml2::xml_find_first(x, xpath)), strtrim, "", 
    USE.NAMES = FALSE)
}

falltxt <- function(x, xpath) {
  vapply(xml2::xml_text(xml2::xml_find_all(x, xpath)), strtrim, "", 
    USE.NAMES = FALSE)
}
