ft_compact <- function (l) Filter(Negate(is.null), l)

pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

is_doi <- function(x) {
  grepl("[0-9]+\\.[0-9]+/.+", x)
}

is_or <- function(x, clazzes) {
  if (!class(x) %in% clazzes) stop("Input to x must be one of class ", paste0(clazzes, collapse = ", "), call. = FALSE)
}

strextract <- function(str, pattern, ...) regmatches(str, regexpr(pattern, str, ...))
strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)

xml_node_parse <- function(x) {
  as.list(stats::setNames(strtrim(xml2::xml_text(x)), xml2::xml_name(x)))
}

# Modified from plyr::try_default
try_default_ <- function(expr, default, quiet = FALSE) {
  result <- default
  if (quiet) {
    tryCatch(result <- expr, error = function(e) NULL)
  }
  else {
    try(result <- expr)
  }
  result
}

# Modified from plyr::tryNULL
try_NULL <- function(expr) try_default_(expr, NULL, quiet = TRUE)

move_col <- function(x, y) x[ c(names(x)[-grep(y, names(x))], y) ]

names_lower <- function(x) {
  stats::setNames(x, tolower(names(x))) 
}

`%||%` <- function(x, y) if (is.null(x)) y else x
`%<|>%` <- function(x, y) if (length(x) == 0) y else x

rbl <- function(x, idcol = FALSE) {
  (xxxxx <- data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE, idcol = idcol)
  ))
}

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}
