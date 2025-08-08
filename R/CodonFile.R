#' CodonFile
#'
#' This function allows you calculate off target effects after removal of presumable polymorphisms
#'
#' @export
#' @examples
#' \dontrun{
#' # Usage example
#' CodonFile()
#' }

# Create database with control samples and adjust for control heteroplasmy level

CodonFile = function() {

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

  the_dir <- "./Overview/CodonFile" # Name the new desired directory
  check_create_dir(the_dir)

  # Save as CSV with FileName
  filename1 <- paste0(the_dir, "/", sample, ".csv")
  write.csv(subset_Adj, file = filename1, row.names = FALSE)

  # Save as CSV with SampleName (ensure it's a valid filename)
  filename2 <- paste0(the_dir, "/", gsub("[^A-Za-z0-9_]", "_", sample_name), ".csv") # Replace invalid characters
  write.csv(subset_Adj, file = filename2, row.names = FALSE)

  cat(sprintf("Files saved: %s, %s\n", filename1, filename2))  # Optional: Print confirmation message
}

}
