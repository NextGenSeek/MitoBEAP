#' AdjOffTarget
#'
#' Calculates mean adjusted off-target heteroplasmy percentages per sample,
#' and merges with corresponding on-target percentages and average coverage.
#'
#' @return No return value. Saves a CSV summary file in `./Overview/Adjusted`.
#' @keywords off-target, adjusted, heteroplasmy, summary
#' @export
#' @name AdjOffTarget
#'
#' @examples
#' \dontrun{
#' AdjOffTarget()
#' }
utils::globalVariables(c(
  "Adj", "SampleList", "SampleName", "AdjPercentage", "OntargetPosition",
  "position", "legend", "FileName", "CombinedName", "Condition", "count_non_missing",
  "avg_count", "sd_count", "make_plot", "base", "reads", "depth", "type",
  "Order", "Off target %", "On target %", "RealName"
))
AdjOffTarget = function() {

  required_objects <- c("Adj", "SampleList", "OnTarget", "Coverage")
  missing <- required_objects[!sapply(required_objects, exists, envir = .GlobalEnv)]
  if (length(missing) > 0) {
    stop("Error: The following required objects are missing from the global environment: ", paste(missing, collapse = ", "))
  }

  Adj <- get("Adj", envir = .GlobalEnv)
  SampleList <- get("SampleList", envir = .GlobalEnv)
  OnTarget <- get("OnTarget", envir = .GlobalEnv)
  Coverage <- get("Coverage", envir = .GlobalEnv)

  # Compute per-sample mean off-target percentage
  offTarget_percentages <- Adj %>%
    dplyr::group_by(SampleName) %>%
    dplyr::summarise(`Off target %` = mean(AdjPercentage, na.rm = TRUE), .groups = "drop")

  # Clean formatting (if needed)
  offTarget_percentages$`Off target %` <- gsub("^1_", "", offTarget_percentages$`Off target %`)


#OnTarget$Sample <- gsub(pattern = "_ontarget", "", x=OnTarget$Sample)
#Coverage$Sample <- gsub(pattern = "_coverage", "", x=Coverage$Sample)
idx3 <- match(offTarget_percentages$SampleName, SampleList$SampleName)
offTarget_percentages$FileName <- SampleList$FileName [idx3]

idx3 <- match(offTarget_percentages$FileName, OnTarget$FileName)
offTarget_percentages$`On target %` <- OnTarget$`On target %` [idx3]

idx4 <- match(offTarget_percentages$FileName, Coverage$FileName)
offTarget_percentages$Coverage <- Coverage$Coverage [idx4]

idx5 <- match(offTarget_percentages$FileName, SampleList$FileName)
offTarget_percentages$RealName <- SampleList$SampleName [idx5]

# Output directory
the_dir <- "./Overview/Adjusted"
check_create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}
check_create_dir(the_dir)

# Write the means to a file
write.csv(offTarget_percentages, file = paste0(the_dir, "/", "Adj_Off_Target_mean_coverage.csv"), row.names = FALSE)


#.GlobalEnv$offTarget_percentages <- offTarget_percentages

}
