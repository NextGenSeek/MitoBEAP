# Outputs

MitoBEAP produces summary tables and publication-ready plots.

## Summary Tables

Create combined overview tables:

```r
CreateOverview()
````

This produces summary files in:

```text
Overview/
```

## Spreadsheet Export

Generate an Excel-compatible overview workbook:

```r
CreateSpreadsheet()
```

## Depth + Heteroplasmy Table

Create a table containing editing percentages and depth values:

```r
DepthFile()
```

Limit to a region:

```r
DepthFile(fromP = 1, toP = 500)
```

Useful for Prism, Excel, heatmaps, or custom plotting.

## Plot Functions

## On-target / Off-target Bar Charts

```r
CreateBarChart(data_type = "OnTarget", colour = "lightgreen")
CreateBarChart(data_type = "OffTarget", colour = "skyblue")
AdjCreateBarChart(colour = "skyblue")
```

## Scatter Plots

```r
DdCBE_df()
AdjScatter(labelPercentage = 10)
```

With thresholds:

```r
DdCBE_df(max_threshold = 90, controls = 3)
AdjScatter(min_threshold = 1.2, labelPercentage = 10)
```

## On-target vs Off-target Comparison

```r
OnVsOffTargetAdj()
```

## Replicate Summary

```r
off_target_count_summary()
```

## Bystander Editing Plots

```r
Bystander(
  BystanderDistance = 10,
  title = "Bystander effect"
)

bystander_only_relevant(
  BystanderDistance = 10,
  title = "Bystander effect"
)
```

## Off-target Histograms

```r
plot_OffTarget_histograms(
  bw = 0.1,
  min_pct = 0.1,
  max_pct = 5
)
```

## Typical Outputs

```text
Overview/
Plots/
```

## Key Metrics Generated

* On-target editing efficiency
* Off-target burden
* Bystander editing
* Coverage-adjusted summaries
* Replicate comparisons

```
