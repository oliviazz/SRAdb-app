-Author: Olivia Zhang 
# SRAdb Web Application:

<iframe src="//www.slideshare.net/slideshow/embed_code/key/4MUmDXJ75RG9Nz" width="425" height="355" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="//www.slideshare.net/seandavi/shinysradb-an-r-package-using-shiny-to-wrap-the-sradb-bioconductor-package" title="ShinySRAdb: an R package using shiny to wrap the SRAdb Bioconductor package" target="_blank">ShinySRAdb: an R package using shiny to wrap the SRAdb Bioconductor package</a> </strong> from <strong><a href="//www.slideshare.net/seandavi" target="_blank">Sean Davis</a></strong> </div>

## Installation

```
library(devtools)
install_github('seandavi/SRAdb-app')
```

## Usage

```
library(ShinySRAdb)
ShinySRAdb()
```

## Dependencies

- SRAdb
- shinythemes
- shinyFiles
- DT
- RgraphViz
- R.utils

## Functions:

- Query Sequence Repository Archive Using Advanced Text Search
- Get info about SRA and FASTQ data files 
- Perform FASTQ Dump for runs
- Export Table Results as Excel
- Create Entity Graph Visualizations for SRA Data 

