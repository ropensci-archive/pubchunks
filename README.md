

pubchunks
=========

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Build Status](https://api.travis-ci.org/ropensci/pubchunks.svg)](https://travis-ci.org/ropensci/pubchunks)
[![codecov](https://codecov.io/gh/ropensci/pubchunks/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/pubchunks)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/pubchunks)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/pubchunks)](https://cran.r-project.org/package=pubchunks)

## Get chunks of XML articles


## Package API

 - pub_tabularize
 - pub_guess_publisher
 - pub_sections
 - pub_chunks
 - pub_providers

The main workhorse function is `pub_chunks()`. It allows you to pull out sections of articles from many different publishers (see next section below) WITHOUT having to know how to parse/navigate XML. XML has a steep learning curve, and can require quite a bit of Googling to sort out how to get to different parts of an XML document. 

The other main function is `pub_tabularize()` - which takes the output of `pub_chunks()` and coerces into a data.frame for easier downstream processing.

## Supported publishers/sources

- eLife
- PLOS
- Entrez/Pubmed
- Elsevier
- Hindawi
- Pensoft
- PeerJ
- Copernicus
- Frontiers

If you know of other publishers or sources that provide XML let us know by [opening an issue](https://github.com/ropensci/pubchunks/issues).

We'll continue adding additional publishers.


## Installation

Stable version


```r
install.packages("pubchunks")
```

Development version from GitHub


```r
devtools::install_github("ropensci/pubchunks")
```

Load library


```r
library('pubchunks')
```

## Working with files


```r
x <- system.file("examples/10_1016_0021_8928_59_90156_x.xml", 
  package = "pubchunks")
```


```r
pub_chunks(x, "abstract")
#> <pub chunks>
#>   from: character
#>   count: 1
#>   sections: abstract
pub_chunks(x, "title")
#> <pub chunks>
#>   from: character
#>   count: 1
#>   sections: title
pub_chunks(x, "authors")
#> <pub chunks>
#>   from: character
#>   count: 1
#>   sections: authors
pub_chunks(x, c("title", "refs"))
#> <pub chunks>
#>   from: character
#>   count: 2
#>   sections: title, refs
```

## Working with the xml already in a string


```r
xml <- paste0(readLines(x), collapse = "")
pub_chunks(xml, "title")
#> <pub chunks>
#>   from: character
#>   count: 1
#>   sections: title
```

## Working with xml2 class object


```r
xml <- paste0(readLines(x), collapse = "")
xml <- xml2::read_xml(xml)
pub_chunks(xml, "title")
#> <pub chunks>
#>   from: xml_document
#>   count: 
#>   sections: title
```

## Working with output of fulltext::ft_get()


```r
install.packages("fulltext")
```


```r
library("fulltext")
x <- fulltext::ft_get('10.1371/journal.pone.0086169', from='plos')
pub_chunks(fulltext::ft_collect(x), sections="authors")
#> <pub chunks>
#>   from: ft_data
#>   count: c(`10.1371/journal.pone.0086169` = 1)
#>   sections: authors
```

## Coerce pub_chunks output into data.frame's


```r
pub_tabularize()
#> Error in assert(x, "pub_chunks"): argument "x" is missing, with no default
```


## Meta

* Please [report any issues or bugs](https://github.com/ropensci/pubchunks/issues).
* License: MIT
* Get citation information for `pubchunks`: `citation(package = 'pubchunks')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
