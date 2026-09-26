#' DepthFile
#'
#' Creates an overview file containing the depth and heteroplasmy percentage per position
#' for each sample in the specified position range.
#'
#' @param fromP Integer, starting position for the range of interest. Default is 1.
#' @param toP Integer, ending position for the range of interest. If not supplied, the max position found is used.
#' @param SampleList Data frame containing sample metadata, including
#'   `FileName` and `SampleName`. Defaults to the global `SampleList`
#'   object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). Input files are read from the `AllPositions`
#'   subdirectory and output is written to the `Overview` subdirectory.
#' @keywords depth, heteroplasmy, overview
#' @export
#' @examples
#' \dontrun{
#' DepthFile(fromP = 300, toP = 500)
#' }

DepthFile <- function(
    fromP = 1,
    toP = NULL,
    SampleList = NULL,
    out_dir_base = "."
) {

  # Use supplied SampleList; fall back to the global object for backward compatibility
  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv, inherits = FALSE)
  }

  # Read all files from AllPositions folder
  all_positions_dir <- file.path(out_dir_base, "AllPositions")

  AllReportFiles <- list.files(
    all_positions_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  if (length(AllReportFiles) == 0) {
    stop("Error: No CSV files found in '", all_positions_dir, "'.")
  }

  df7 <- AllReportFiles %>%
    purrr::set_names(
      nm = basename(.) %>% tools::file_path_sans_ext()
    ) %>%
    purrr::map_df(
      readr::read_csv,
      col_names = TRUE,
      show_col_types = FALSE,
      .id = "FileName"
    )

  required_columns <- c(
    "FileName",
    "position",
    "depth",
    "percentage"
  )

  missing_columns <- setdiff(required_columns, names(df7))

  if (length(missing_columns) > 0) {
    stop(
      "Error: AllPositions files are missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  df7 <- df7[, required_columns]

  # Handle missing toP
  if (is.null(toP)) {
    toP <- max(df7$position, na.rm = TRUE)
  }

  # Filter region of interest
  df7 <- df7[df7$position >= fromP & df7$position <= toP, ]

# Match sample names
df7$SampleName <- SampleList$SampleName[match(df7$FileName, SampleList$FileName)]

# Reshape: percentages
df8 <- reshape2::dcast(df7, FileName ~ position, value.var = "percentage")
df8$type <- "percentage"

# Reshape: depths
df10 <- reshape2::dcast(df7, FileName ~ position, value.var = "depth")
df10$type <- "Depth"

combined <- rbind(df8, df10)
combined_df <- combined %>%
  dplyr::select(type, dplyr::everything())

# Output directory
the_dir <- file.path(out_dir_base, "Overview")
check_create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}
check_create_dir(the_dir)

readr::write_csv(
  combined_df,
  file = file.path(the_dir, "DepthFile.csv")
)
}
