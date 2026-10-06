#' Calculate complete mitochondrial mismatch spectrum
#'
#' Calculates the complete single-nucleotide mismatch spectrum from
#' minimum-depth mitochondrial nucleotide count files. Unlike
#' \code{CalcAllMutations()}, which retains only the most abundant
#' non-reference base at each position, this function retains every
#' non-reference base reported in the input.
#'
#' This provides a chemistry-independent assessment of nucleotide mismatches
#' before editor-specific filtering (for example, C-to-T/G-to-A filtering for
#' DdCBE or A-to-G/T-to-C filtering for adenine base editors).
#'
#' @param AllReportFiles Optional character vector of input CSV file paths.
#'   If not supplied, CSV files are read from the \code{minimum}
#'   subdirectory within \code{out_dir_base}.
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

    minimum_dir <- file.path(out_dir_base, "minimum")

    AllReportFiles <- list.files(
      minimum_dir,
      pattern = "\\.csv$",
      full.names = TRUE
    )

    if (length(AllReportFiles) == 0) {
      stop(
        "Error: No minimum-depth count files found in '",
        minimum_dir,
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
      "ref_base",
      "depth"
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

    # Identify VarScan allele columns (A6, A7, A8, ...).
    allele_cols <- grep(
      "^A[0-9]+$",
      names(df),
      value = TRUE
    )

    if (length(allele_cols) == 0) {
      stop(
        "Error: No VarScan allele columns (e.g. A6-A9) found in '",
        basename(input),
        "'."
      )
    }

    results <- vector("list", length = 0)
    result_index <- 1L

    for (i in seq_len(nrow(df))) {

      ref_base <- toupper(
        as.character(df$ref_base[i])
      )

      coverage <- suppressWarnings(
        as.numeric(df$depth[i])
      )

      for (allele_col in allele_cols) {

        allele_entry <- df[[allele_col]][i]

        if (is.na(allele_entry) ||
            allele_entry == "") {
          next
        }

        allele_parts <- strsplit(
          as.character(allele_entry),
          ":",
          fixed = TRUE
        )[[1]]

        if (length(allele_parts) < 2) {
          next
        }

        mut_base <- toupper(allele_parts[1])

        mut_reads <- suppressWarnings(
          as.numeric(allele_parts[2])
        )

        # Retain single-nucleotide mismatches only.
        if (
          !mut_base %in% c("A", "C", "G", "T") ||
          mut_base == ref_base
        ) {
          next
        }

        mismatch_percentage <- if (
          !is.na(mut_reads) &&
          !is.na(coverage) &&
          coverage > 0
        ) {
          (mut_reads / coverage) * 100
        } else {
          NA_real_
        }

        results[[result_index]] <- data.frame(
          FileName = tools::file_path_sans_ext(
            basename(input)
          ),
          position = df$position[i],
          RefBase = ref_base,
          MutBase = mut_base,
          MutReads = mut_reads,
          Coverage = coverage,
          percentage = mismatch_percentage,
          Mutation = paste0(
            ref_base,
            ">",
            mut_base
          ),
          stringsAsFactors = FALSE
        )

        result_index <- result_index + 1L
      }
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
