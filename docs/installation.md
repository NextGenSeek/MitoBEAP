# Installation

## Install from GitHub

```r
install.packages("remotes")
remotes::install_github("NextGenSeek/MitoBEAP")
````

## Install Required Dependencies

If prompted, allow installation of required packages:

* dplyr
* ggplot2
* ggrepel
* tidyr
* readr
* stringr
* RColorBrewer
* tibble
* patchwork
* knitr
* rmarkdown

## Load Package

```r id="1mp9g6"
library(MitoBEAP)
```

## Check Installation

```r id="1pgrv4"
packageVersion("MitoBEAP")
```

## Update to Latest Version

```r id="3v3m2z"
remotes::install_github("NextGenSeek/MitoBEAP", force = TRUE)
```

## Troubleshooting

### Installation fails on Mac

Install Xcode command line tools:

```bash
xcode-select --install
```

### Old package version loaded

Restart R session and reload:

```r id="8tng6z"
.rs.restartR()
library(MitoBEAP)
```
