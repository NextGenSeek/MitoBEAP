# Quick Start

This example shows a standard MitoBEAP workflow using sequencing count files.

## 1. Load the package

```r
library(MitoBEAP)
````

## 2A. Workflow for VarScan2 output files as input files

Load raw count files from your input folder

```r
RawCountFiles <- list.files("./InputFolder", pattern = "*.csv", full.names = TRUE)
lapply(RawCountFiles, LoadRawCount)
```

## 2B. Alternative workflow for custom csv input files
Apart from VarScan2 result files, the user can also use different input files, as long as they contain the columns;
chrom, position, ref_base, depth, base, reads, percentage

```r
allFiles <- list.files("./InputFolder", pattern = "*.csv", full.names = TRUE)
filterCGEdits(allFiles) for cytosine base editors
filterATEdits(allFiles) for adenine base editors
```

## 3. Apply a minimum depth threshold

```r
depth <- 100
```

A depth threshold helps reduce noise from low-coverage positions.

## 4. Optional: Restrict analysis to a region

```r
RegionSelection(RawCountFiles)

RegionSelection(
  RawCountFiles,
  position_range = c(1000, 5000),
  depth_filter = TRUE,
  min_depth = 100)
```

## 5. Create a SampleList file

Required columns:

* FileName
* SampleName
* Condition

Load it with:

```r
SampleList <- read.delim("SampleList.csv", sep = ",")
head(SampleList)
```

## 6. Run editing analysis

For cytosine base editors:

```r
MinXFiles <- list.files("./minimum", full.names = TRUE)
OntargetPosition <- 1561
lapply(MinXFiles, OnTargetCalcCBE)
```

For adenine base editors:

```r
MinXFiles <- list.files("./minimum", full.names = TRUE)
OntargetPosition <- 1561
lapply(MinXFiles, OnTargetCalcABE)
```

## 7. Generate summary tables

```r
CoverageFiles <- list.files("./Coverage", pattern = "*.txt", full.names = TRUE)
Coverage <- as.data.frame(CoverageCalc(CoverageFiles))

MeanFiles <- list.files("./mean", pattern = "*mean.txt", full.names = TRUE)
Mean <- as.data.frame(MeanCalc(MeanFiles))

OnTargetFiles <- list.files("./OnTarget", pattern = "*ontarget.txt", full.names = TRUE)
OnTarget <- as.data.frame(CombineOnTarget(OnTargetFiles))

CreateOverview()
```

## 8. Create your first plot

```r
CreateBarChart(data_type = "OnTarget", colour = "lightgreen")
```

Other common plots:

```r
AdjScatter()
OnVsOffTargetAdj()
Bystander()
bystander_only_relevant()
plot_OffTarget_histograms()
```
