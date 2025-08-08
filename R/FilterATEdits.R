#' FilterATEdits
#'
#' #' Filter for adenine base editing
#'
#' @param csvFiles list with csv files
#' @param out_dir output directory. Default "./percentages"
#' @return Describe what the function returns
#' @export
FilterATEdits <- function(csvFiles, out_dir = "./percentages") {
  # ── 1. Safety checks ────────────────────────────────────────────────────────
  if (any(!file.exists(csvFiles))) {
    stop("One or more input files not found.")
  }
  req_cols <- c("chrom","position","ref_base","depth",
                "base","reads","percentage")

  # ── 2. Output directory ─────────────────────────────────────────────────────
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── 3. Process every file ───────────────────────────────────────────────────
  lapply(csvFiles, function(f) {
    dat <- readr::read_csv(f, show_col_types = FALSE)

    if (!all(req_cols %in% names(dat)))
      stop(sprintf("File %s is missing one or more required columns.", f))

    # ── 3a. Keep only C→T and G→A edits
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

