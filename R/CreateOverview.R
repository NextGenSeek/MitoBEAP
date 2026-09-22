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
#' @return Invisibly returns the combined overview data frame. The same data
#'   are written to "Overview/All_Ontarget_mean_Coverage.csv".
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
    cleanup = FALSE
) {

  # Use supplied objects; fall back to global objects for backward compatibility
  if (is.null(All_mean)) {
    if (!exists("All_mean", envir = .GlobalEnv)) {
      stop("Error: 'All_mean' must be supplied or exist in the global environment.")
    }
    All_mean <- get("All_mean", envir = .GlobalEnv)
  }

  if (is.null(OnTarget)) {
    if (!exists("OnTarget", envir = .GlobalEnv)) {
      stop("Error: 'OnTarget' must be supplied or exist in the global environment.")
    }
    OnTarget <- get("OnTarget", envir = .GlobalEnv)
  }

  if (is.null(Coverage)) {
    if (!exists("Coverage", envir = .GlobalEnv)) {
      stop("Error: 'Coverage' must be supplied or exist in the global environment.")
    }
    Coverage <- get("Coverage", envir = .GlobalEnv)
  }

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv)
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

  readr::write_csv(All_mean,file = paste0(the_dir,"/", "All_Ontarget_mean_Coverage.csv", sep=""))

# Optionally clean up intermediate folders
if (isTRUE(cleanup)) {
  unlink("./minimum", recursive = TRUE)
  unlink("./Coverage", recursive = TRUE)
  unlink("./mean", recursive = TRUE)
  unlink("./OnTarget", recursive = TRUE)
  unlink("./percentages", recursive = TRUE)
}

return(invisible(All_mean))
}
