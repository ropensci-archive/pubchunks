get_what <- function(data, what, from, extract="xml_text"){
  if (!extract %in% c("xml_text", "as.character")) {
    stop("'extract' must be one of 'xml_text' or 'as.character'")
  }
  if ( any(what == "all") ) what <- pub_sections()
  res <- stats::setNames(lapply(what, function(z) {
    vv <- tryCatch(switch(z,
           front = front(data, from, extract),
           body = body(data, from, extract),
           back = back(data, from, extract),
           title = title(data, from, extract),
           abstract = abstract(data, from, extract),
           executive_summary = exec_summary(data, from, extract),
           doi = doi(data, from, extract),
           categories = categories(data, from, extract),
           authors = authors(data, from, extract),
           aff = aff(data, from, extract),
           keywords = keywords(data, from, extract),
           refs_dois = refs_dois(data, from, extract),
           refs = refs(data, from, extract),
           publisher = publisher(data, from, extract),
           journal_meta = journal_meta(data, from, extract),
           article_meta = article_meta(data, from, extract),
           acknowledgments = acknowledgments(data, from, extract),
           permissions = permissions(data, from, extract),
           history = history(data, from, extract)
    ), error = function(e) e, warning = function(w) w)
    if (inherits(vv, c("error", "warning"))) NULL else vv
  }), what)
  res$.publisher <- from
  return(res)
}
