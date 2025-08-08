#' FilterCGEdits
#'
#' Filter for CG editing
#'
#' @param csvFiles list with csv files
#' @param OntargetPosition Numeric; position within the genome
#' @param out_dir_base output directory. Default "./percentages"
#' @return Describe what the function returns
#' @export
FilterCGEdits <- function(csvFiles,
                          OntargetPosition,
                          out_dir_base = ".") {
  # ────────────────────────────────────────────────────────────────────────────
  #  1.  Safety checks & helpers
  # ────────────────────────────────────────────────────────────────────────────
  if (any(!file.exists(csvFiles))) {
    stop("One or more input files not found.")
  }

  req_cols <- c("chrom","position","ref_base","depth",
                "base","reads","percentage")

  mkdir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  # Prepare folders
  paths <- file.path(out_dir_base,
                     c("percentages","mean","OnTarget","Coverage","AllPositions"))
  vapply(paths, mkdir, FUN.VALUE = logical(1))

  # ────────────────────────────────────────────────────────────────────────────
  #  2.  Main loop per file
  # ────────────────────────────────────────────────────────────────────────────
  lapply(csvFiles, function(f) {

    ## 2a. read & validate -----------------------------------------------------
    dat <- readr::read_csv(f, show_col_types = FALSE)
    if (!all(req_cols %in% names(dat)))
      stop(sprintf("File %s is missing one or more required columns.", f))

    dat$position <- as.numeric(dat$position)  # be sure
    dat$depth    <- as.numeric(dat$depth)

    ## 2b. split into subsets --------------------------------------------------
    # all C/G reference bases
    dataCG <- subset(dat, ref_base %in% c("C","G"))

    # rows that actually harbour the desired edit
    edits  <- subset(dataCG,
                     (ref_base == "C" & base == "T") |
                       (ref_base == "G" & base == "A"))

    # bring edits onto every C/G position  (non‑edited rows get NA)
    allCG  <- merge(dataCG[ , c("chrom","position","ref_base","depth")],
                    edits[ , c("chrom","position","base","reads","percentage")],
                    by = c("chrom","position"),
                    all.x = TRUE)

    # percentage 0 for untouched C/G sites
    allCG$percentage[is.na(allCG$percentage)] <- 0

    ## 2c. write percentages file ---------------------------------------------
    out_stub <- tools::file_path_sans_ext(basename(f))
    readr::write_csv(
      allCG[order(allCG$position), ],
      file.path(out_dir_base,"percentages", paste0(out_stub,".csv"))
    )

    ## 2d. off‑target mean -----------------------------------------------------
    off   <- subset(allCG, !(position %in% OntargetPosition))
    meanP <- mean(off$percentage)
    write(meanP,
          file = file.path(out_dir_base,"mean", paste0(out_stub,"_mean.txt")))

    ## 2e. on‑target percentage ------------------------------------------------
    onRow <- subset(allCG, position %in% OntargetPosition)
    # if the edit never occurred, percentage is already zero
    write(onRow$percentage,
          file = file.path(out_dir_base,"OnTarget",
                           paste0(out_stub,"_ontarget.txt")))

    ## 2f. coverage file -------------------------------------------------------
    coverage <- mean(dat$depth, na.rm = TRUE)
    write(coverage,
          file = file.path(out_dir_base,"Coverage",
                           paste0(out_stub,"_coverage.txt")))

    ## 2g. AllPositions annotation file ---------------------------------------
    ann <- merge(dat,
                 allCG[ , c("chrom","position","base","reads","percentage")],
                 by = c("chrom","position"),
                 suffixes = c("", ".mut"))

    names(ann)[names(ann) %in% c("base.mut","reads.mut","percentage.mut")] <-
      c("MutBase","MutReads","MutPercentage")

    readr::write_csv(
      ann[order(ann$position), ],
      file.path(out_dir_base,"AllPositions", paste0(out_stub,".csv"))
    )
  })

  invisible(NULL)
}
