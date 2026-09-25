#' LoadRawCount
#'
#' Loads raw count files from specified file paths, filters rows based on minimum read depth, and writes the filtered data to a subdirectory called "minimum".
#' @param RawCountFiles A character vector of file paths to raw count data files.
#' @param depth Numeric. Minimum read depth required to retain a row.
#'   Default is 30.
#' @param out_dir_base Base output directory. Defaults to the current working
#'   directory (`"."`). The `minimum` subdirectory is created within this directory.
#' @keywords load counts, filter depth
#' @export
#' @examples
#' \dontrun{
#' # Usage example
#' file_paths <- c("file1.csv", "file2.csv", "file3.csv")
#' LoadRawCount(file_paths)
#' }

LoadRawCount <- function(
    RawCountFiles,
    depth = 30,
    out_dir_base = "."
) {
  # Check if all files exist
  if (any(!file.exists(RawCountFiles))) {
    stop("Error: One or more input files do not exist.")
  }

  # Check if 'depth' is a numeric value
  if (!is.numeric(depth) || length(depth) != 1) {
    stop("Error: 'depth' must be a single numeric value.")
  }

  process_file <- function(input) {
    # Read the variable-width VarScan count table.
    # The first five fields are fixed; additional fields contain allele information.
    data <- read.delim(
      input,
      header = FALSE,
      skip = 1,
      fill = TRUE,
      stringsAsFactors = FALSE
    )

    if (ncol(data) < 6) {
      stop(
        "VarScan input must contain at least six columns: ",
        basename(input)
      )
    }

    fixed_names <- c(
      "chrom",
      "position",
      "ref_base",
      "depth_all",
      "depth"
    )

    allele_names <- paste0("A", 6:ncol(data))

    names(data) <- c(
      fixed_names,
      allele_names
    )

    data[data == ""] <- NA

  #Keep only rows with a depth of more than X reads (default 30)
  data <- data[data$depth > depth, ]

   check_create_dir <- function(the_dir) {
     if (!dir.exists(the_dir)) {
       dir.create(the_dir, recursive = TRUE) } #Creates a directory if it doesn't already exist
    }

   the_dir <- file.path(out_dir_base, "minimum")
   check_create_dir(the_dir)

  # write as new csv
   readr::write_csv(
     data,
     file = file.path(
       the_dir,
       paste0(tools::file_path_sans_ext(basename(input)), ".csv")
     )
   )

}
lapply(RawCountFiles, process_file)
invisible(NULL)
}
