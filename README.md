

pubchunks
=========

[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
[![Build Status](https://api.travis-ci.org/ropensci/pubchunks.svg)](https://travis-ci.org/ropensci/pubchunks)

__Get chunks of XML articles__

... still working through whether this will be a package or not and how it would work ...


## Installation

Development version from GitHub


```r
# get latest fulltext
devtools::install_github("ropensci/fulltext")
devtools::install_github("ropensci/pubchunks")
```

Load library


```r
library('pubchunks')
```

## First, get some full text

Using `fulltext`


```r
library(fulltext)
x <- ft_search(query = 'ecology', from = 'crossref')
```

Then extract chunks


```r
x <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'), from = "elife")
x %>% ft_collect() %>% ft_chunks("publisher") %>% ft_tabularize()
```

Get multiple fields at once


```r
x %>% ft_collect() %>% ft_chunks(c("doi","publisher")) %>% ft_tabularize()
```

Use `dplyr` to data munge


```r
library("dplyr")
x %>%
  ft_collect() %>% 
  ft_chunks(c("doi", "publisher", "permissions")) %>%
  ft_tabularize() %>%
  .$elife %>%
  select(-permissions.license, -permissions.license_url)
```

## Contributors

* Scott Chamberlain <http://github.com/sckott>

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/pubchunks/issues).
* License: MIT
* Get citation information for `pubchunks`: `citation(package = 'pubchunks')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)