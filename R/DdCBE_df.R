#' DdCBE_df
#'
#' Processes base editing data by filtering out positions where control samples exceed a maximum heteroplasmy threshold.
#' Returns a data frame with adjusted percentages (`AdjPercentage`) where such control-derived positions are excluded.
#'
#' @param min_threshold Minimum heteroplasmy percentage to keep (not used in filtering control samples).
#' @param max_threshold Maximum heteroplasmy percentage allowed in control samples before marking as background (default = 100).
#' @param controls Number of control samples required to consistently exceed the threshold for a position to be excluded.
#'
#' @return No return value. The result is assigned to `.GlobalEnv$Adj`.
#' @export
#'
#' @examples
#' \dontrun{
#' DdCBE_df(min_threshold = 0, max_threshold = 10, controls = 3)
#' }

DdCBE_df = function(min_threshold = 0, max_threshold = 100, controls = 3) {

  # Check required object
  if (!exists("SampleList")) {
    stop("Error: 'SampleList' must exist in the global environment.")
  }

  # Validate numeric inputs
  if (!is.numeric(min_threshold) || !is.numeric(max_threshold) || !is.numeric(controls)) {
    stop("Error: 'min_threshold', 'max_threshold', and 'controls' must be numeric.")
  }

  # Read report files
  AllReportFiles <- list.files("./AllPositions", pattern = "\\.csv$", full.names = TRUE)
  if (length(AllReportFiles) == 0) {
    stop("Error: No report files found in './AllPositions'.")
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


#.GlobalEnv$df7 <- df7

# Apply max thresholds on control samples to know which positions to ignore
#df8 <- df7[df7$Condition == "control" & (df7$percentage >= max_threshold | df7$percentage <= min_threshold), ]
# Including min is wrong, because we will lose information in the on/off target effects
df8 <- df7[df7$Condition == "control" & (df7$percentage >= max_threshold), ]

# Position above/below WT threshold in n (=all control) samples
df8 %>%
  group_by(position) %>%
  filter(n() == controls) -> subset #These are the positions we want to ignore

#.GlobalEnv$df9 <- subset
df10 <- unique(as.vector(df8$position))
#.GlobalEnv$df10 <- df10

df11 <- df7 %>%
  mutate(AdjPercentage = case_when(
      position %in% df10 ~ NA,
      TRUE ~ df7$percentage
    )
  )

assign("Adj", df11, envir = .GlobalEnv)

}
