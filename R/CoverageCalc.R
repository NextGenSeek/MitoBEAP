#' CoverageCalc
#'
#' Combines coverage data from multiple files into a single overview file.
#' Strips unnecessary parts from filenames, renames columns, and saves the result as "Coverage.csv".
#'
#' @param CoverageFiles A character vector of file paths to coverage summary files (CSV).
#' @return A data frame containing the coverage for each input sample.
#'   The same data are also assigned to `Coverage` in the global environment
#'   and written to `./Overview/Coverage.csv`.
#' @keywords coverage, directory, combine
#' @export
#' @examples
#' \dontrun{
#' CoverageFiles <- c("./data/coverage_file1.csv", "./data/coverage_file2.csv")
#' CoverageCalc(CoverageFiles)
#' }
CoverageCalc <- function(CoverageFiles) {
  # Check if all files exist
  if (any(!file.exists(CoverageFiles))) {
    stop("Error: One or more coverage files do not exist.")
  }

  # Create output directory
  the_dir <- "./Overview"
  check_create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  check_create_dir(the_dir)

df4 <- CoverageFiles %>%
  purrr::set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>% # Name without extension
  purrr::map_df(read_csv,
                col_names = FALSE,
                skip = 1,
                .id = "coverage")
df4$coverage <- gsub(pattern = "_counts_coverage", "", x=df4$coverage)
df4$coverage <- gsub(pattern = "_coverage", "", x=df4$coverage)
df4$X1 <- gsub(pattern = "1 ", "", x=df4$X1)

names(df4) [1]  <- "FileName"
names(df4) [2] <- "Coverage"
Coverage <- df4

# Assign to global environment for downstream MitoBEAP functions
assign("Coverage", df4, envir = .GlobalEnv)

readr::write_csv(df4, file = file.path(the_dir, "Coverage.csv"))

return(df4)
}
