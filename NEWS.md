pubchunks 0.3.0
===============

### MINOR IMPROVEMENTS

* improvements to `pub_chunks()` with `sections="refs"` (fetching references from an article). all reference extraction was simply extracting the entire reference, which lost all white space. now there are custom reference parsers for the various types of reference formats. there still are outstanding issues, so do please point out where ref. extraction is not correct (#2)
* fix pub_guess_publisher examples error (#10)

pubchunks 0.2.2
===============

### MINOR IMPROVEMENTS

* fixed failing example (#9)

pubchunks 0.2.0
===============

### MINOR IMPROVEMENTS

* most section options in `pub_chunks()` now have defaults for extracting the section, and return NULL/empty list when not found (#3) (#4)
* improvements to `print.pub_chunks` so that the printed object contains more information (publisher/journal title) and more accurate ('character' used to include xml as character string and file paths, but are separated now). in addition, we state that the first 5 sections are printed so the user knows there could be more (#8)
* fix `pub_tabularize()` to accept list outputs from `pub_chunks()` (#5)

pubchunks 0.1.0
===============

### NEW FEATURES

* released to CRAN
