#' AdjOffTarget
#'
#' Calculates mean adjusted off-target heteroplasmy percentages per sample,
#' and merges with corresponding on-target percentages and average coverage.
#'
#' @param Adj Data frame containing adjusted editing percentages.
#'   Defaults to the global `Adj` object if not supplied.
#' @param SampleList Data frame containing sample metadata.
#'   Defaults to the global `SampleList` object if not supplied.
#' @param OnTarget Data frame containing on-target editing values.
#'   Defaults to the global `OnTarget` object if not supplied.
#' @param Coverage Data frame containing coverage values.
#'   Defaults to the global `Coverage` object if not supplied.
#' @return A data frame containing the adjusted off-target percentage,
#'   on-target percentage, and coverage for each sample. The same data are
#'   also written to `./Overview/Adjusted/Adj_Off_Target_mean_coverage.csv`.
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
AdjOffTarget <- function(
    Adj = NULL,
    SampleList = NULL,
    OnTarget = NULL,
    Coverage = NULL
) {

  # Use supplied objects; fall back to global objects for backward compatibility
  if (is.null(Adj)) {
    if (!exists("Adj", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'Adj' must be supplied or exist in the global environment.")
    }
    Adj <- get("Adj", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(OnTarget)) {
    if (!exists("OnTarget", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'OnTarget' must be supplied or exist in the global environment.")
    }
    OnTarget <- get("OnTarget", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(Coverage)) {
    if (!exists("Coverage", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'Coverage' must be supplied or exist in the global environment.")
    }
    Coverage <- get("Coverage", envir = .GlobalEnv, inherits = FALSE)
  }

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
write.csv(offTarget_percentages,
          file = paste0(the_dir, "/", "Adj_Off_Target_mean_coverage.csv"),
          row.names = FALSE
          )

return(offTarget_percentages)

}
