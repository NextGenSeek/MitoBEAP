#' CombineOnTarget
#'
#' Combines on-target editing percentages from multiple output files into
#' a single summary table and writes it to `Overview/OnTarget.csv`
#' within `out_dir_base`.
#'
#' Empty files that contain only the sentinel "x" (or have no data rows
#' after the header) are assigned an on-target percentage of 0.
#'
#' @param OnTargetFiles Character vector of file paths to *_ontarget.txt files.
#' @param out_dir_base Base output directory. Defaults to the current working
#'   directory (`"."`). The `Overview` subdirectory is created within this directory.
#'
#' @return A data frame containing the on-target editing percentage for each
#'   input sample. The same data are also written to `Overview/OnTarget.csv`
#'   within `out_dir_base`.
#'
#' @export
CombineOnTarget <- function(OnTargetFiles, out_dir_base = ".") {

  if (any(!file.exists(OnTargetFiles))) {
    stop("Error: One or more on-target files do not exist.")
  }

  out_dir <- file.path(out_dir_base, "Overview")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  ## Helper: always returns a tibble with X1 as *character*
  safe_read <- function(fp) {
    raw <- suppressWarnings(readr::read_lines(fp))

    empty_or_x <- length(raw) == 0 ||
      (length(raw) == 1 && trimws(raw) == "x")

    if (empty_or_x) {
      tibble::tibble(X1 = "0")                # character "0"
    } else {
      dat <- readr::read_csv(
        fp,
        col_names = FALSE,                    # keep your header skip below
        skip = 1,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE
      )

      if (nrow(dat) == 0) tibble::tibble(X1 = "0") else dat
    }
  }

  df <- OnTargetFiles %>%
    purrr::set_names(tools::file_path_sans_ext(basename(.))) %>%  # give ID
    purrr::map_df(safe_read, .id = "FileName")

  # --- cleaning -------------------------------------------------------------
  df$FileName <- sub("_counts_ontarget$", "", df$FileName)
  df$FileName <- sub("_ontarget$",        "", df$FileName)

  df$`On target %` <- as.numeric(gsub("^1\\s+", "", df$X1))
  df$X1 <- NULL                                   # drop raw column

  # --------------------------------------------------------------------------
  readr::write_csv(df, file.path(out_dir, "OnTarget.csv"))

  return(df)
}
