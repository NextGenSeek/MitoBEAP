#' CodonAllFile
#'
#' @description File that can be used for further processing
#' @return creates csv file for each sample
#' @export
CodonAllFile = function() {

  AdjCodon <- Adj

  # Rename
  colnames(AdjCodon)[colnames(AdjCodon) == "RefSeq"] <- "ref_base"
  colnames(AdjCodon)[colnames(AdjCodon) == "HighestMM"] <- "MutBase"
  colnames(AdjCodon)[colnames(AdjCodon) == "AdjPercentage"] <- "percentage"

  # Get unique sample names
  unique_samples <- unique(AdjCodon$SampleName)

  # Iterate over each sample
  for (sample in unique_samples) {
    # Subset dataframe for current sample
    subset_Adj <- AdjCodon[AdjCodon$SampleName == sample, ]

    the_dir <- "./Overview/CodonFile" #Name the new desired directory
    check_create_dir(the_dir)

    # Save as CSV with sample name
    filename <- paste0(the_dir,"/",sample, ".csv")
    write.csv(subset_Adj, file = filename, row.names = FALSE)

    cat(sprintf("File saved: %s\n", filename))  # Optional: Print confirmation message
  }

}
