# Input Files

## Main Input Type

MitoBEAP is designed for sequencing count tables generated from mitochondrial editing experiments.

A common workflow uses VarScan2 readcount output exported as `.csv` files.

## Alternative CSV Input 

Custom `.csv` files can also be used when they contain the following columns:

* chrom
* position
* ref_base
* depth
* base
* reads
* percentage
  
This is a slightly different workflow, check step 2B in the [docs/quickstart](docs/quickstart) or check the Vignette

## Sample Metadata File

Many functions use a metadata file called `SampleList`.

Required columns:

* FileName
* SampleName
* Condition

Optional column for grouped-analysis column:

* CombinedName

Load with:

```r
SampleList <- read.delim("SampleList.csv", sep = ",")
```


