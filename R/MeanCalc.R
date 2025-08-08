#' MeanCalc
#'
#' Combines off-target mean percentage values from multiple samples into a single summary file.
#' The result is saved as "All_OffTarget_Mean.csv" in the "Overview" directory.
#'
#' @param MeanFiles A character vector of file paths to mean off-target output files (one per sample).
#' @keywords off-target, mean, summary
#' @return No return value. A plot is saved to disk.
#' @export
#' @examples
#' \dontrun{
#' mean_files <- list.files("./mean", pattern = "*.txt", full.names = TRUE)
#' MeanCalc(mean_files)
#' }

# Calculate off-target values

MeanCalc = function(MeanFiles) {

  # Check if all files exist
  if (any(!file.exists(MeanFiles))) {
    stop("Error: One or more mean files do not exist.")
  }

  # Create output directory
  the_dir <- "./Overview"
  check_create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  check_create_dir(the_dir)


# Combine all mean/off target files into one file

df <- MeanFiles %>%
  purrr::set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>% # Name without extension
  purrr::map_df(read_csv,
                col_names = FALSE,
                skip = 1,
                .id = "All_mean")
df$All_mean <- gsub(pattern = "_counts_mean", "", x=df$All_mean)
df$All_mean <- gsub(pattern = "_mean", "", x=df$All_mean)
df$X1 <- gsub(pattern = "1 ", "", x=df$X1)

names(df) [1]  <- "FileName"
names(df) [2] <- "Off target %"
All_mean <- df

# Assign to global environment
assign("All_mean", df, envir = .GlobalEnv)

write_csv(df,file = paste0(the_dir,"/", "All_OffTarget_Mean.csv", sep=""))
}
