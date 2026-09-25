#' CreateOverview
#'
#' Combines all processed data (`All_mean`, `OnTarget`, `Coverage`, and `SampleList`) into one overview table.
#' The table includes on-target %, off-target %, average coverage, sample names, and ordering.
#' The result is saved to "All_Ontarget_mean_Coverage.csv" in the "Overview" directory.
#'
#' @param cleanup Logical. If `TRUE`, intermediate output directories
#'   (`minimum`, `Coverage`, `mean`, `OnTarget`, and `percentages`) are
#'   deleted after the overview file is created. Default is `FALSE`.
#' @param All_mean Data frame containing the combined off-target mean values.
#'   Defaults to the global `All_mean` object if not supplied.
#' @param OnTarget Data frame containing on-target editing values.
#'   Defaults to the global `OnTarget` object if not supplied.
#' @param Coverage Data frame containing coverage values.
#'   Defaults to the global `Coverage` object if not supplied.
#' @param SampleList Data frame containing sample metadata.
#'   Defaults to the global `SampleList` object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). The overview file is written to the `Overview`
#'   subdirectory, and any optional cleanup is performed on intermediate
#'   subdirectories within this directory.
#' @param coverage_ratio_warning Numeric coverage ratio above which a warning
#'   is issued when the highest-coverage sample exceeds the lowest-coverage
#'   sample by this factor. Default is 10. Set to `NULL` to disable the check.
#' @return Invisibly returns the combined overview data frame. The same data
#'   are written to `Overview/All_Ontarget_mean_Coverage.csv` within
#'   `out_dir_base`.
#' @export
#'
#' @examples
#' \dontrun{
#' CreateOverview()
#' }

CreateOverview <- function(
    All_mean = NULL,
    OnTarget = NULL,
    Coverage = NULL,
    SampleList = NULL,
    cleanup = FALSE,
    out_dir_base = ".",
    coverage_ratio_warning = 10
) {

  # Use supplied objects; fall back to global objects for backward compatibility
  if (is.null(All_mean)) {
    if (!exists("All_mean", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'All_mean' must be supplied or exist in the global environment.")
    }
    All_mean <- get("All_mean", envir = .GlobalEnv, inherits = FALSE)
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

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv, inherits = FALSE)
  }

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

  # Warn when sequencing coverage differs substantially across samples
  if (!is.null(coverage_ratio_warning)) {

    if (!is.numeric(coverage_ratio_warning) ||
        length(coverage_ratio_warning) != 1 ||
        is.na(coverage_ratio_warning) ||
        coverage_ratio_warning <= 1) {
      stop(
        "'coverage_ratio_warning' must be a single numeric value greater than 1, or NULL."
      )
    }

    coverage_values <- as.numeric(All_mean$Coverage)
    coverage_values <- coverage_values[
      is.finite(coverage_values) & coverage_values > 0
    ]

    if (length(coverage_values) >= 2) {

      coverage_ratio <- max(coverage_values) / min(coverage_values)

      if (coverage_ratio > coverage_ratio_warning) {
        warning(
          sprintf(
            paste0(
              "Sequencing coverage differs by %.1f-fold across samples ",
              "(minimum %.1fx; maximum %.1fx). ",
              "This exceeds the coverage warning threshold of %.1f-fold. ",
              "Differences in sequencing depth may affect comparisons of ",
              "low-frequency editing events."
            ),
            coverage_ratio,
            min(coverage_values),
            max(coverage_values),
            coverage_ratio_warning
          ),
          call. = FALSE
        )
      }
    }
  }

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
  the_dir <- file.path(out_dir_base, "Overview")
  check_create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  check_create_dir(the_dir)

  readr::write_csv(
    All_mean,
    file = file.path(the_dir, "All_Ontarget_mean_Coverage.csv")
  )

# Optionally clean up intermediate folders
  if (isTRUE(cleanup)) {
    unlink(file.path(out_dir_base, "minimum"), recursive = TRUE)
    unlink(file.path(out_dir_base, "Coverage"), recursive = TRUE)
    unlink(file.path(out_dir_base, "mean"), recursive = TRUE)
    unlink(file.path(out_dir_base, "OnTarget"), recursive = TRUE)
    unlink(file.path(out_dir_base, "percentages"), recursive = TRUE)
  }

return(invisible(All_mean))
}
