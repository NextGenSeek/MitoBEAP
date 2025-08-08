#' CreateOverview
#'
#' Combines all processed data (`All_mean`, `OnTarget`, `Coverage`, and `SampleList`) into one overview table.
#' The table includes on-target %, off-target %, average coverage, sample names, and ordering.
#' The result is saved to "All_Ontarget_mean_Coverage.csv" in the "Overview" directory.
#'
#' @return No return value. Writes a combined summary CSV to the "Overview" directory.
#' @export
#'
#' @examples
#' \dontrun{
#' CreateOverview()
#' }

CreateOverview = function() {
  # Check that required global objects exist
  required_objects <- c("All_mean", "OnTarget", "Coverage", "SampleList")
  missing <- required_objects[!sapply(required_objects, exists, envir = .GlobalEnv)]
  if (length(missing) > 0) {
    stop("Error: The following required objects are missing from the global environment: ", paste(missing, collapse = ", "))
  }

  # Reference objects safely
  All_mean <- get("All_mean", envir = .GlobalEnv)
  OnTarget <- get("OnTarget", envir = .GlobalEnv)
  Coverage <- get("Coverage", envir = .GlobalEnv)
  SampleList <- get("SampleList", envir = .GlobalEnv)

  # Clean and merge fields
  All_mean$FileName <- gsub("_mean", "", All_mean$FileName)
  All_mean$`Off target %` <- gsub("^1_", "", All_mean$`Off target %`)
  OnTarget$FileName <- gsub("_ontarget", "", OnTarget$FileName)
  Coverage$FileName <- gsub("_coverage", "", Coverage$FileName)

  # Merge data
  idx3 <- match(All_mean$FileName, OnTarget$FileName)
  All_mean$`On target %` <- OnTarget$`On target %`[idx3]

  idx4 <- match(All_mean$FileName, Coverage$FileName)
  All_mean$Coverage <- Coverage$Coverage[idx4]

  idx5 <- match(All_mean$FileName, SampleList$FileName)
  All_mean$RealName <- SampleList$SampleName[idx5]
  All_mean$Order <- SampleList$Order[idx5]

# Install the 'splitstackshape' package if needed: install.packages("splitstackshape")
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Please install the 'dplyr' package to use the 'arrange' function.")
}

  # Sort and export
  All_mean <- dplyr::arrange(All_mean, Order)

  # Create output directory if needed
  the_dir <- "./Overview"
  check_create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  check_create_dir(the_dir)

write_csv(All_mean,file = paste0(the_dir,"/", "All_Ontarget_mean_Coverage.csv", sep=""))

# Clean up temporary folders
unlink("./minimum", recursive = TRUE)
unlink("./Coverage", recursive = TRUE)
unlink("./mean", recursive = TRUE)
unlink("./OnTarget", recursive = TRUE)
unlink("./percentages", recursive = TRUE)

}
