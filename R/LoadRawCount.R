#' LoadRawCount
#'
#' Loads raw count files from specified file paths, filters rows based on
#' minimum read depth, and writes the filtered data to a subdirectory
#' called "minimum".
#'
#' @param RawCountFiles A character vector of file paths to raw count data files.
#' @param depth Numeric. Minimum read depth required to retain a row.
#'   Default is 30.
#' @param out_dir_base Base output directory. Defaults to the current working
#'   directory (`"."`). The `minimum` subdirectory is created within this directory.
#'
#' @keywords load counts, filter depth
#' @export
#'
#' @examples
#' \dontrun{
#' file_paths <- c("file1.csv", "file2.csv", "file3.csv")
#' LoadRawCount(file_paths)
#' }

LoadRawCount <- function(
    RawCountFiles,
    depth = 30,
    out_dir_base = "."
) {

  if (any(!file.exists(RawCountFiles))) {
    stop("Error: One or more input files do not exist.")
  }

  if (!is.numeric(depth) || length(depth) != 1) {
    stop("Error: 'depth' must be a single numeric value.")
  }

  process_file <- function(input) {

    # VarScan readcount files contain a variable number of
    # tab-separated allele fields per genomic position.
    lines <- readLines(input, warn = FALSE)

    # Remove the VarScan header
    lines <- lines[-1]

    # Split each physical line into its tab-separated fields
    fields <- strsplit(lines, "\t", fixed = TRUE)

    # Find the widest record in this file
    max_fields <- max(lengths(fields))

    # Pad shorter records with NA so that one physical input
    # line always corresponds to one row
    fields <- lapply(
      fields,
      function(x) {
        length(x) <- max_fields
        x
      }
    )

    data <- as.data.frame(
      do.call(rbind, fields),
      stringsAsFactors = FALSE
    )

    # The first five fields are fixed VarScan fields
    colnames(data)[1:5] <- c(
      "chrom",
      "position",
      "ref_base",
      "depth_all",
      "depth"
    )

    # Remaining fields contain the variable allele information
    if (max_fields > 5) {
      colnames(data)[6:max_fields] <- paste0(
        "A",
        6:max_fields
      )
    }

    # Convert fixed numeric fields
    data$position <- as.numeric(data$position)
    data$depth_all <- as.numeric(data$depth_all)
    data$depth <- as.numeric(data$depth)

    # Keep only rows above the requested depth
    data <- data[
      !is.na(data$depth) &
        data$depth > depth,
      ,
      drop = FALSE
    ]

    the_dir <- file.path(
      out_dir_base,
      "minimum"
    )

    if (!dir.exists(the_dir)) {
      dir.create(
        the_dir,
        recursive = TRUE
      )
    }

    readr::write_csv(
      data,
      file = file.path(
        the_dir,
        paste0(
          tools::file_path_sans_ext(
            basename(input)
          ),
          ".csv"
        )
      )
    )
  }

  lapply(
    RawCountFiles,
    process_file
  )

  invisible(NULL)
}
