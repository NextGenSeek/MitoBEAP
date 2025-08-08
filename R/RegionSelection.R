#' RegionSelection
#'
#' Filters genomic count data files based on a specified position range and an optional depth threshold.
#' Saves the filtered data into a "minimum" subdirectory.
#'
#' @param RawCountFiles A character vector of file paths to raw count files (tab-delimited).
#' @param position_range A numeric vector of length 2 specifying the start and end positions to retain. If NULL, uses the full range in each file.
#' @param depth_filter Logical; if TRUE, applies a depth threshold filter.
#' @param min_depth Minimum read depth to retain rows (used only if \code{depth_filter = TRUE}).
#' @keywords region selection, filter, depth
#' @export
#' @examples
#' \dontrun{
#' file_paths <- c("sample1.txt", "sample2.txt")
#' RegionSelection(file_paths, position_range = c(1000, 5000), depth_filter = TRUE, min_depth = 30)
#' }
RegionSelection <- function(RawCountFiles, position_range = NULL, depth_filter = FALSE, min_depth = 0) {
  # Validate file existence
  if (any(!file.exists(RawCountFiles))) {
    stop("Error: One or more input files do not exist.")
  }

  # Validate position_range
  if (!is.null(position_range)) {
    if (!is.numeric(position_range) || length(position_range) != 2) {
      stop("Error: 'position_range' must be a numeric vector of length 2.")
    }
  }

  # Validate depth filtering inputs
  if (!is.logical(depth_filter) || length(depth_filter) != 1) {
    stop("Error: 'depth_filter' must be a single logical value.")
  }

  if (!is.numeric(min_depth) || length(min_depth) != 1 || min_depth < 0) {
    stop("Error: 'min_depth' must be a single non-negative numeric value.")
  }

  process_file <- function(input) {
    col_Names <- paste0("A", 1:10)
    suppressWarnings(data <- read.delim(input, row.names = NULL, col.names = col_Names))
    data[data == ""] <- NA

    # Rename columns
    colnames(data)[1:9] <- c("chrom", "position", "ref_base", "depth", "q0_depth", "A6", "A7", "A8", "A9")

    # Convert to numeric with error handling
    data$position <- suppressWarnings(as.numeric(data$position))
    data$depth <- suppressWarnings(as.numeric(data$depth))

    if (any(is.na(data$position))) {
      warning(paste("Warning: NA values found in 'position' column in file:", input))
    }

    if (any(is.na(data$depth))) {
      warning(paste("Warning: NA values found in 'depth' column in file:", input))
    }

    # Default range
    position_range_use <- if (is.null(position_range)) {
      c(min(data$position, na.rm = TRUE), max(data$position, na.rm = TRUE))
    } else {
      position_range
    }

    # Filter by position range
    data <- data[data$position >= position_range_use[1] & data$position <= position_range_use[2], ]

    # Optional depth filter
    if (depth_filter) {
      data <- data[data$depth > min_depth, ]
    }

    # Output dir creation
    check_create_dir <- function(the_dir) {
      if (!dir.exists(the_dir)) {
        dir.create(the_dir, recursive = TRUE)
      }
    }

    the_dir <- "./minimum"
    check_create_dir(the_dir)

    # Save filtered file
    write_csv(data, file = paste0(the_dir, "/", file_path_sans_ext(basename(input)), ".csv"))
  }

  lapply(RawCountFiles, process_file)
  invisible(NULL)
}
