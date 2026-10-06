#' MeanCalc
#'
#' Combine mean off-target editing values from multiple samples
#'
#' Combines unadjusted mean off-target percentage values generated for
#' individual samples into a single summary table.
#'
#' @param MeanFiles A character vector of file paths to mean off-target
#'   output files, one per sample.
#' @param out_dir_base Base output directory. Defaults to the current working
#'   directory (`"."`). The `Overview` subdirectory is created within this
#'   directory.
#'
#' @return A data frame containing the unadjusted mean off-target percentage
#'   for each sample. The same data are written to
#'   `Overview/All_OffTarget_Mean.csv`.
#'
#' @keywords off-target mean summary
#' @export
#'
#' @examples
#' \dontrun{
#' mean_files <- list.files(
#'   "./mean",
#'   pattern = "*mean.txt",
#'   full.names = TRUE
#' )
#'
#' Mean <- MeanCalc(mean_files)
#' }

MeanCalc <- function(MeanFiles, out_dir_base = ".") {

  if (any(!file.exists(MeanFiles))) {
    stop("Error: One or more mean files do not exist.")
  }

  the_dir <- file.path(out_dir_base, "Overview")
  check_create_dir(the_dir)

  df <- MeanFiles %>%
    purrr::set_names(
      nm = basename(.) %>%
        tools::file_path_sans_ext()
    ) %>%
    purrr::map_df(
      readr::read_csv,
      col_names = FALSE,
      skip = 1,
      .id = "FileName"
    )

  df$FileName <- gsub(
    pattern = "_counts_mean",
    replacement = "",
    x = df$FileName
  )

  df$FileName <- gsub(
    pattern = "_mean",
    replacement = "",
    x = df$FileName
  )

  df$X1 <- gsub(
    pattern = "1 ",
    replacement = "",
    x = df$X1
  )

  names(df)[2] <- "Off target %"

  readr::write_csv(
    df,
    file.path(
      the_dir,
      "All_OffTarget_Mean.csv"
    )
  )

  return(df)
}
