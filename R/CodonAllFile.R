#' CodonAllFile
#'
#' @description File that can be used for further processing
#' @param Adj Data frame containing adjusted mutation percentages.
#'   Defaults to the global `Adj` object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). Output files are written to the `Overview/CodonFile`
#'   subdirectory within this directory.
#' @return creates csv file for each sample
#' @export
CodonAllFile <- function(
    Adj = NULL,
    out_dir_base = "."
) {

  # Use supplied Adj; fall back to the global object for backward compatibility
  if (is.null(Adj)) {
    if (!exists("Adj", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'Adj' must be supplied or exist in the global environment.")
    }
    Adj <- get("Adj", envir = .GlobalEnv, inherits = FALSE)
  }

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

    the_dir <- file.path(out_dir_base, "Overview", "CodonFile") #Name the new desired directory
    check_create_dir(the_dir)

    # Save as CSV with sample name
    filename <- file.path(the_dir, paste0(sample, ".csv"))
    write.csv(subset_Adj, file = filename, row.names = FALSE)

    cat(sprintf("File saved: %s\n", filename))  # Optional: Print confirmation message
  }

}
