#' FilterATEdits
#'
#' Filter for adenine base editing
#'
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). By default, filtered files are written to the
#'   `percentages` subdirectory within this directory.
#' @param out_dir Optional explicit output directory. If supplied, this overrides
#'   `out_dir_base` for backward compatibility.
#' @param csvFiles Character vector containing paths to the input CSV files.
#' @return Invisibly returns `NULL`. Filtered A-to-G and T-to-C editing
#'   results are written to the selected output directory.
#' @export
FilterATEdits <- function(
    csvFiles,
    out_dir_base = ".",
    out_dir = NULL
) {
  # ── 1. Safety checks ────────────────────────────────────────────────────────
  if (any(!file.exists(csvFiles))) {
    stop("One or more input files not found.")
  }
  req_cols <- c("chrom","position","ref_base","depth",
                "base","reads","percentage")

  # ── 2. Output directory ─────────────────────────────────────────────────────
  if (is.null(out_dir)) {
    out_dir <- file.path(out_dir_base, "percentages")
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # ── 3. Process every file ───────────────────────────────────────────────────
  lapply(csvFiles, function(f) {
    dat <- readr::read_csv(f, show_col_types = FALSE)

    if (!all(req_cols %in% names(dat)))
      stop(sprintf("File %s is missing one or more required columns.", f))

    # ── 3a. Keep only A→G and T→C edits
    edits <- subset(
      dat,
      (ref_base == "A" & base == "G") |
        (ref_base == "T" & base == "C")
    )

    # If your upstream pipeline sometimes leaves base/reads NA but percentage>0,
    # you can drop NA rows safely here:
    edits <- edits[!is.na(edits$percentage) & edits$percentage > 0, ]

    # ── 3b. Write the filtered file
    outfile <- file.path(
      out_dir,
      sprintf("%s.csv", tools::file_path_sans_ext(basename(f)))
    )
    readr::write_csv(edits, outfile)
  })

  invisible(NULL)
}

