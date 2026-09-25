#' DdCBE_df
#'
#' Processes base editing data by filtering out positions where control samples exceed a maximum heteroplasmy threshold.
#' Returns a data frame with adjusted percentages (`AdjPercentage`) where such control-derived positions are excluded.
#'
#' @param min_threshold Minimum heteroplasmy percentage to keep (not used in filtering control samples).
#' @param max_threshold Maximum heteroplasmy percentage allowed in control samples before marking as background (default = 100).
#' @param controls Minimum number of distinct control samples in which a
#'   position must have a heteroplasmy percentage greater than or equal to
#'   `max_threshold` for that position to be excluded.
#' @param SampleList Data frame containing sample metadata, including
#'   `FileName`, `SampleName`, and `Condition`. Defaults to the global
#'   `SampleList` object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). Input files are read from the `AllPositions`
#'   subdirectory within this directory.
#'
#' @return A data frame containing the adjusted editing percentages.
#'   The function also reports the number and coordinates of positions masked
#'   based on the specified control threshold.
#' @export
#'
#' @examples
#' \dontrun{
#' DdCBE_df(min_threshold = 0, max_threshold = 10, controls = 3)
#' }

DdCBE_df <- function(
    min_threshold = 0,
    max_threshold = 100,
    controls = 3,
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

  # Validate numeric inputs
  if (!is.numeric(min_threshold) || !is.numeric(max_threshold) || !is.numeric(controls)) {
    stop("Error: 'min_threshold', 'max_threshold', and 'controls' must be numeric.")
  }

  all_positions_dir <- file.path(out_dir_base, "AllPositions")

  AllReportFiles <- list.files(
    all_positions_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  if (length(AllReportFiles) == 0) {
    stop("Error: No report files found in '", all_positions_dir, "'.")
  }

df7 <- AllReportFiles %>%
  purrr::set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>% # Name without extension
  purrr::map_df(read_csv,
                col_names = FALSE,
                skip = 1,
                .id = "FileName")

# Rename required columns
names(df7) [2]  <- "chr"
names(df7) [3] <- "position"
names(df7) [10] <- "percentage"

# Clean filenames and merge metadata
df7$FileName <- gsub("_R30", "", df7$FileName)
idx5 <- match(df7$FileName, SampleList$FileName)
df7$SampleName <- SampleList$SampleName[idx5]
df7$Condition <- SampleList$Condition[idx5]

# Apply max thresholds on control samples to know which positions to ignore
df8 <- df7[df7$Condition == "control" & (df7$percentage >= max_threshold), ]

# Identify positions exceeding the threshold in at least the
# user-specified number of distinct control samples
subset <- df8 %>%
  dplyr::group_by(position) %>%
  dplyr::filter(dplyr::n_distinct(FileName) >= controls) %>%
  dplyr::ungroup()

# Positions to mask
df10 <- sort(unique(subset$position))

message(
  "Masked positions: ", length(df10),
  " (threshold >= ", max_threshold,
  "% in at least ", controls,
  ifelse(controls == 1, " control sample)", " control samples)")
)

if (length(df10) > 0) {
  message(
    "Positions masked: ",
    paste(df10, collapse = ", ")
  )
}

df11 <- df7 %>%
  mutate(AdjPercentage = case_when(
      position %in% df10 ~ NA,
      TRUE ~ df7$percentage
    )
  )

return(df11)
}
