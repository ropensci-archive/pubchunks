

pubchunks
=========

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran checks](https://cranchecks.info/badges/worst/pubchunks)](https://cranchecks.info/pkgs/pubchunks)
[![R-check](https://github.com/ropensci/pubchunks/workflows/R-check/badge.svg)](https://github.com/ropensci/pubchunks/actions?query=workflow%3AR-check)
[![codecov](https://codecov.io/gh/ropensci/pubchunks/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/pubchunks)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/pubchunks)](https://github.com/r-hub/cranlogs.app)
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
- F1000 Research

If you know of other publishers or sources that provide XML let us know by [opening an issue](https://github.com/ropensci/pubchunks/issues).

We'll continue adding additional publishers.


## Installation

Stable version


```r
install.packages("pubchunks")
```

Development version from GitHub


```r
remotes::install_github("ropensci/pubchunks")
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
#>   from: file
#>   publisher/journal: elsevier/Journal of Applied Mathematics and Mechanics
#>   sections: abstract
#>   showing up to first 5: 
#>    abstract (n=1): Abstract
#>                
#>                   This pa ...
pub_chunks(x, "title")
#> <pub chunks>
#>   from: file
#>   publisher/journal: elsevier/Journal of Applied Mathematics and Mechanics
#>   sections: title
#>   showing up to first 5: 
#>    title (n=1): On the driving of a piston with a rigid collar int ...
pub_chunks(x, "authors")
#> <pub chunks>
#>   from: file
#>   publisher/journal: elsevier/Journal of Applied Mathematics and Mechanics
#>   sections: authors
#>   showing up to first 5: 
#>    authors (n=1): Chetaev, D.N
pub_chunks(x, c("title", "refs"))
#> <pub chunks>
#>   from: file
#>   publisher/journal: elsevier/Journal of Applied Mathematics and Mechanics
#>   sections: title, refs
#>   showing up to first 5: 
#>    title (n=1): On the driving of a piston with a rigid collar int ...
#>    refs (n=6): Watson G.N.. 1949. Teoriia besselevykh funktsii. N
```

The output of `pub_chunks()` is a list with an S3 class `pub_chunks` to make 
internal work in the package easier. You can easily see the list structure 
by using `unclass()`.

## Working with the xml already in a string


```r
xml <- paste0(readLines(x), collapse = "")
pub_chunks(xml, "title")
#> <pub chunks>
#>   from: character
#>   publisher/journal: elsevier/Journal of Applied Mathematics and Mechanics
#>   sections: title
#>   showing up to first 5: 
#>    title (n=1): On the driving of a piston with a rigid collar int ...
```

## Working with xml2 class object


```r
xml <- paste0(readLines(x), collapse = "")
xml <- xml2::read_xml(xml)
pub_chunks(xml, "title")
#> <pub chunks>
#>   from: xml_document
#>   publisher/journal: elsevier/Journal of Applied Mathematics and Mechanics
#>   sections: title
#>   showing up to first 5: 
#>    title (n=1): On the driving of a piston with a rigid collar int ...
```

## Working with output of fulltext::ft_get()


```r
install.packages("fulltext")
```


```r
library("fulltext")
x <- fulltext::ft_get('10.1371/journal.pone.0086169')
pub_chunks(fulltext::ft_collect(x), sections="authors")
#> $plos
#> $plos$`10.1371/journal.pone.0086169`
#> <pub chunks>
#>   from: xml_document
#>   publisher/journal: plos/PLoS ONE
#>   sections: authors
#>   showing up to first 5: 
#>    authors (n=4): nested list
#> 
#> 
#> attr(,"ft_data")
#> [1] TRUE
```

## Coerce pub_chunks output into data.frame's


```r
x <- system.file("examples/elife_1.xml", package = "pubchunks")
res <- pub_chunks(x, c("doi", "title", "keywords"))
pub_tabularize(res)
#>                   doi                                          title
#> 1 10.7554/eLife.03032 MicroRNA-mediated repression of nonsense mRNAs
#> 2 10.7554/eLife.03032 MicroRNA-mediated repression of nonsense mRNAs
#> 3 10.7554/eLife.03032 MicroRNA-mediated repression of nonsense mRNAs
#> 4 10.7554/eLife.03032 MicroRNA-mediated repression of nonsense mRNAs
#> 5 10.7554/eLife.03032 MicroRNA-mediated repression of nonsense mRNAs
#> 6 10.7554/eLife.03032 MicroRNA-mediated repression of nonsense mRNAs
#>                       keywords .publisher
#> 1                     microRNA      elife
#> 2            nonsense mutation      elife
#> 3 nonsense-mediated mRNA decay      elife
#> 4                          APC      elife
#> 5             intron retention      elife
#> 6  premature termination codon      elife
```

## Get a random XML article


```r
library(rcrossref)
library(dplyr)

res <- cr_works(filter = list(
    full_text_type = "application/xml", 
    license_url="http://creativecommons.org/licenses/by/4.0/"))
links <- bind_rows(res$data$link) %>% filter(content.type == "application/xml")
download.file(links$URL[1], (i <- tempfile(fileext = ".xml")))
pub_chunks(i)
#> <pub chunks>
#>   from: file
#>   publisher/journal: unknown/NA
#>   sections: all
#>   showing up to first 5: 
#>    front (n=0): 
#>    body (n=0): 
#>    back (n=0): 
#>    title (n=0): 
#>    doi (n=0):
download.file(links$URL[13], (j <- tempfile(fileext = ".xml")))
pub_chunks(j)
#> <pub chunks>
#>   from: file
#>   publisher/journal: hindawi/BioMed Research International
#>   sections: all
#>   showing up to first 5: 
#>    front (n=2): nested list
#>    body (n=49): Oxidative stress and Reactive Oxygen Species (ROS)
#>    back (n=4): nested list
#>    title (n=1): Selected Enzyme Inhibitory Effects of Euphorbia ch ...
#>    doi (n=1): 10.1155/2018/1219367
download.file(links$URL[20], (k <- tempfile(fileext = ".xml")))
pub_chunks(k)
#> <pub chunks>
#>   from: file
#>   publisher/journal: hindawi/Case Reports in Pathology
#>   sections: all
#>   showing up to first 5: 
#>    front (n=2): nested list
#>    body (n=16): Bonnetti et al. first noted in 1992 an unusual cel
#>    back (n=3): nested list
#>    title (n=1): An Inguinal Perivascular Epithelioid Cell Tumor Me ...
#>    doi (n=1): 10.1155/2018/5749421
```




## Meta

* Please [report any issues or bugs](https://github.com/ropensci/pubchunks/issues).
* License: MIT
* Get citation information for `pubchunks`: `citation(package = 'pubchunks')`
* Please note that this package is released with a [Contributor Code of Conduct](https://ropensci.org/code-of-conduct/). By contributing to this project, you agree to abide by its terms.
