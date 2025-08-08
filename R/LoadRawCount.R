#' LoadRawCount
#'
#' Loads raw count files from specified file paths, filters rows based on minimum read depth, and writes the filtered data to a subdirectory called "minimum".
#' @param RawCountFiles A character vector of file paths to raw count data files.
#' @keywords load counts, filter depth
#' @export
#' @examples
#' \dontrun{
#' # Usage example
#' file_paths <- c("file1.csv", "file2.csv", "file3.csv")
#' LoadRawCount(file_paths)
#' }

LoadRawCount = function(RawCountFiles) { # specify depth before running this function
  # Check if all files exist
  if (any(!file.exists(RawCountFiles))) {
    stop("Error: One or more input files do not exist.")
  }
  # Check if 'depth' variable exists
  if (!exists("depth")) {
    stop("Error: The variable 'depth' is not defined in the global environment.")
  }

  # Check if 'depth' is a numeric value
  if (!is.numeric(depth) || length(depth) != 1) {
    stop("Error: 'depth' must be a single numeric value.")
  }

  process_file <- function(input) {
  col_Names <- c(paste0("A", 1:10)) # makes it easier later
  suppressWarnings(data <- read.delim(input, row.names = NULL, col.names = col_Names))
  data[data == ""] <- NA

  #data <- as.data.frame(data)

  #Change column names
  colnames(data) [c(1:9)] <- c("chrom","position","ref_base","depth_all","depth","A6","A7","A8","A9")

  #Keep only rows with a depth of more than X reads (default 30)
  data <- data[data$depth > depth, ]

   check_create_dir <- function(the_dir) {
     if (!dir.exists(the_dir)) {
       dir.create(the_dir, recursive = TRUE) } #Creates a directory if it doesn't already exist
    }

    the_dir <- "./minimum"
   check_create_dir(the_dir)

  # write as new csv
  write_csv(data, file = paste0(the_dir, "/", file_path_sans_ext(basename(input)), '.csv', sep = ""))

}
lapply(RawCountFiles, process_file)
invisible(NULL)
}
