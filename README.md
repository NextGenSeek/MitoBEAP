# MitoBEAP
MitoBEAP is an R package designed to analyze mitochondrial base editing data from DdCBE experiments. It calculates both on-target and off-target editing efficiencies, visualizes editing patterns, and produces reports for submission to data repositories.

<p align="center">
  <img src="images/HeatmapBystanderEffect.png" width="45%">
  <img src="images/OnOffTarget_Adj.png" width="29%">
</p>

# How to Install MitoBEAP

## Option 1 : 
Install 'remotes' package if needed:
install.packages("remotes")

Install MitoBEAP from GitHub:
remotes::install_github("NextGenSeek/MitoBEAP")

## Option 2 : 
Install 'devtools' if needed:
install.packages("devtools")

Install MitoBEAP from GitHub:
devtools::install_github("NextGenSeek/MitoBEAP")

## Dependencies:
All dependencies are listed in the DESCRIPTION file and installed automatically via devtools.

# How to use MitoBEAP
Check the vignette

# Example files
The folder ExampleFiles contains the files that were used in the paper: Development of the Mitochondrial Base Editor Analysis Package (MitoBEAP) by Mutti et al.

# Citations
If you use this tool, please cite: Mutti et al. 
