#' CodonFile
#'
#' This function allows you calculate off target effects after removal of presumable polymorphisms
#'
#' @param Adj Data frame containing adjusted editing percentages.
#'   Defaults to the global `Adj` object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). Output files are written to the `Overview/CodonFile`
#'   subdirectory within this directory.
#' @export
#' @examples
#' \dontrun{
#' # Usage example
#' CodonFile()
#' }

# Create database with control samples and adjust for control heteroplasmy level

CodonFile <- function(
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

cols_to_remove <- c( "X4", "X5", "X6", "X8","percentage")
AdjCodon <- Adj[, !(names(Adj) %in% cols_to_remove)]

# Rename
colnames(AdjCodon)[colnames(AdjCodon) == "X3"] <- "ref_base"
colnames(AdjCodon)[colnames(AdjCodon) == "X7"] <- "MutBase"
colnames(AdjCodon)[colnames(AdjCodon) == "AdjPercentage"] <- "percentage"

# Get unique sample names
unique_samples <- unique(AdjCodon$FileName)

# Iterate over each sample
for (sample in unique_samples) {
  # Subset dataframe for current sample
  subset_Adj <- AdjCodon[AdjCodon$FileName == sample, ]

  # Get the corresponding SampleName
  sample_name <- unique(subset_Adj$SampleName) # Extract unique SampleName

  the_dir <- file.path(out_dir_base, "Overview", "CodonFile") # Name the new desired directory
  check_create_dir(the_dir)

  # Save as CSV with FileName
  filename1 <- file.path(the_dir, paste0(sample, ".csv"))
  write.csv(subset_Adj, file = filename1, row.names = FALSE)

  # Save as CSV with SampleName (ensure it's a valid filename)
  filename2 <- file.path(
    the_dir,
    paste0(gsub("[^A-Za-z0-9_]", "_", sample_name), ".csv")
  ) # Replace invalid characters
  write.csv(subset_Adj, file = filename2, row.names = FALSE)

  cat(sprintf("Files saved: %s, %s\n", filename1, filename2))  # Optional: Print confirmation message
}

}
