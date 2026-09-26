#' Calculate complete mitochondrial mismatch spectrum
#'
#' Calculates the complete single-nucleotide mismatch spectrum from mutation
#' count files. Unlike \code{CalcAllMutations()}, which retains only the most
#' abundant non-reference base at each position, this function retains every
#' non-reference base reported in the input.
#'
#' This provides a chemistry-independent assessment of nucleotide mismatches
#' before editor-specific filtering (for example, C-to-T/G-to-A filtering for
#' DdCBE or A-to-G/T-to-C filtering for adenine base editors).
#'
#' @param AllReportFiles Optional character vector of input CSV file paths.
#'   If not supplied, files ending in \code{_allMutations.csv} are read from
#'   the \code{counts} subdirectory within \code{out_dir_base}.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (\code{"."}). Output files are written to the
#'   \code{MismatchSpectrum} subdirectory.
#'
#' @return A data frame containing all non-reference single-nucleotide
#'   mismatches across all supplied samples. Columns are:
#'   \code{FileName}, \code{position}, \code{RefBase}, \code{MutBase},
#'   \code{MutReads}, \code{Coverage}, \code{percentage}, and
#'   \code{Mutation}.
#'
#' @details
#' The function analyses single-nucleotide mismatches only. It does not detect
#' or quantify insertions or deletions and does not assess nuclear off-target
#' sites.
#'
#' @export
CalculateMismatchSpectrum <- function(
    AllReportFiles = NULL,
    out_dir_base = "."
) {

  if (is.null(AllReportFiles)) {

    counts_dir <- file.path(out_dir_base, "counts")

    AllReportFiles <- list.files(
      counts_dir,
      pattern = "_allMutations\\.csv$",
      full.names = TRUE
    )

    if (length(AllReportFiles) == 0) {
      stop(
        "Error: No mutation count files found in '",
        counts_dir,
        "'."
      )
    }
  }

  if (length(AllReportFiles) == 0) {
    stop("Error: No input files supplied.")
  }

  missing_files <- AllReportFiles[!file.exists(AllReportFiles)]

  if (length(missing_files) > 0) {
    stop(
      "Error: The following input file(s) do not exist: ",
      paste(missing_files, collapse = ", ")
    )
  }

  output_dir <- file.path(out_dir_base, "MismatchSpectrum")
  check_create_dir(output_dir)

  process_file <- function(input) {

    df <- readr::read_csv(
      input,
      show_col_types = FALSE
    )

    required_columns <- c(
      "position",
      "ref_base"
    )

    missing_columns <- setdiff(required_columns, names(df))

    if (length(missing_columns) > 0) {
      stop(
        "Error: Missing required column(s) in '",
        basename(input),
        "': ",
        paste(missing_columns, collapse = ", ")
      )
    }

    # Identify base/read column pairs produced by the mutation-counting step.
    base_columns <- grep(
      "^base\\.[0-9]+$",
      names(df),
      value = TRUE
    )

    if (length(base_columns) == 0) {
      stop(
        "Error: No mutation base columns (e.g. 'base.1') found in '",
        basename(input),
        "'."
      )
    }

    base_numbers <- sub("^base\\.", "", base_columns)
    read_columns <- paste0("reads.", base_numbers)

    missing_read_columns <- setdiff(read_columns, names(df))

    if (length(missing_read_columns) > 0) {
      stop(
        "Error: Missing read-count column(s) in '",
        basename(input),
        "': ",
        paste(missing_read_columns, collapse = ", ")
      )
    }

    results <- vector("list", length = 0)

    result_index <- 1L

    for (i in seq_len(nrow(df))) {

      ref_base <- toupper(as.character(df$ref_base[i]))

      bases <- toupper(
        as.character(
          unlist(df[i, base_columns], use.names = FALSE)
        )
      )

      reads <- suppressWarnings(
        as.numeric(
          unlist(df[i, read_columns], use.names = FALSE)
        )
      )

      # Use the total depth reported by the input where available.
      # Otherwise calculate coverage from the individual base counts.
      if ("depth" %in% names(df) &&
          !is.na(df$depth[i])) {

        coverage <- as.numeric(df$depth[i])

      } else {

        coverage <- sum(reads, na.rm = TRUE)
      }

      valid <- !is.na(bases) &
        bases %in% c("A", "C", "G", "T") &
        bases != ref_base

      if (!any(valid)) {
        next
      }

      mismatch_bases <- bases[valid]
      mismatch_reads <- reads[valid]

      mismatch_percentage <- ifelse(
        !is.na(mismatch_reads) &
          !is.na(coverage) &
          coverage > 0,
        (mismatch_reads / coverage) * 100,
        NA_real_
      )

      results[[result_index]] <- data.frame(
        FileName = tools::file_path_sans_ext(
          basename(input)
        ),
        position = df$position[i],
        RefBase = ref_base,
        MutBase = mismatch_bases,
        MutReads = mismatch_reads,
        Coverage = coverage,
        percentage = mismatch_percentage,
        Mutation = paste0(
          ref_base,
          ">",
          mismatch_bases
        ),
        stringsAsFactors = FALSE
      )

      result_index <- result_index + 1L
    }

    if (length(results) == 0) {

      sample_result <- data.frame(
        FileName = character(),
        position = numeric(),
        RefBase = character(),
        MutBase = character(),
        MutReads = numeric(),
        Coverage = numeric(),
        percentage = numeric(),
        Mutation = character(),
        stringsAsFactors = FALSE
      )

    } else {

      sample_result <- dplyr::bind_rows(results)
    }

    output_name <- paste0(
      tools::file_path_sans_ext(basename(input)),
      "_MismatchSpectrum.csv"
    )

    readr::write_csv(
      sample_result,
      file.path(output_dir, output_name)
    )

    sample_result
  }

  all_results <- lapply(
    AllReportFiles,
    process_file
  )

  dplyr::bind_rows(all_results)
}
