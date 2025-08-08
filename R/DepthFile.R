#' DepthFile
#'
#' Creates an overview file containing the depth and heteroplasmy percentage per position
#' for each sample in the specified position range.
#'
#' @param fromP Integer, starting position for the range of interest. Default is 1.
#' @param toP Integer, ending position for the range of interest. If not supplied, the max position found is used.
#' @keywords depth, heteroplasmy, overview
#' @export
#' @examples
#' \dontrun{
#' DepthFile(fromP = 300, toP = 500)
#' }

DepthFile = function(fromP = 1, toP = NULL) {

  # Ensure required object exists
  if (!exists("SampleList")) {
    stop("Error: 'SampleList' object must exist.")
  }

  # Read all files from AllPositions folder
  AllReportFiles <- list.files("./AllPositions", pattern = "\\.csv$", full.names = TRUE)
  if (length(AllReportFiles) == 0) {
    stop("Error: No CSV files found in './AllPositions'.")
  }

  df7 <- AllReportFiles %>%
    purrr::set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>% # Name without extension
    purrr::map_df(read_csv,
                  col_names = FALSE,
                  skip = 1,
                  .id = "FileName")

  # Assign column names
  names(df7) [2]  <- "chr"
  names(df7) [3] <- "position"
  names(df7) [10] <- "percentage"
  names(df7) [5] <- "depth"

  # Handle missing toP
  if (is.null(toP)) {
    toP <- max(df7$position, na.rm = TRUE)
  }

  # Filter region of interest
  df7 <- df7[df7$position >= fromP & df7$position <= toP, ]
  df7 <- df7[, -c(2, 4, 6:9)]  # remove unneeded columns

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
the_dir <- "./Overview"
check_create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}
check_create_dir(the_dir)

write_csv(combined_df,file = paste0(the_dir,"/", "DepthFile.csv", sep=""))
}
