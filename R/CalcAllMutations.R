#' CalcAllMutations
#'
#' Calculate all mutations
#'
#' @param AllReportFiles list with csv files
#' @return Describe what the function returns
#' @export
CalcAllMutations = function(AllReportFiles) {
  AllReportFiles <- list.files("./counts", pattern = "*_allMutations.csv",
                               full.names = TRUE)
  process_file <- function(input) {
    df9 <- read.delim(input, row.names = NULL, header = T, sep = ",")

    colnames(df9)[which(names(df9) == "ref_base")] <- "RefSeq"
    colnames(df9)[which(names(df9) == "depth")] <- "CoverageAll"
    colnames(df9)[which(names(df9) == "base.1")] <- "Seq1"
    colnames(df9)[which(names(df9) == "reads.1")] <- "Depth1"
    if ("base.2" %in% names(df9)) {
      names(df9)[which(names(df9) == "base.2")] <- "Seq2"
    }
    if ("reads.2" %in% names(df9)) {
      names(df9)[which(names(df9) == "reads.2")] <- "Depth2"
    }
    if ("base.3" %in% names(df9)) {
      names(df9)[which(names(df9) == "base.3")] <- "Seq3"
    }
    if ("reads.3" %in% names(df9)) {
      names(df9)[which(names(df9) == "reads.3")] <- "Depth3"
    }
    if ("base.4" %in% names(df9)) {
      names(df9)[which(names(df9) == "base.4")] <- "Seq4"
    }
    if ("reads.4" %in% names(df9)) {
      names(df9)[which(names(df9) == "reads.4")] <- "Depth4"
    }

    # Make dataframe with only the highest mutation for each row

    # Check which Seq and Depth columns are present
    seq_cols <- grep("^Seq", names(df9), value = TRUE)
    depth_cols <- grep("^Depth", names(df9), value = TRUE)

    # Create a new dataframe with the required columns
    new_df <- df9[, c("position", "RefSeq")]

    # Initialize columns for the selected sequence and depth
    new_df$HighestMM <- NA
    new_df$MMDepth <- NA
    new_df$Coverage <- NA
    new_df$MM_percentage <- NA

    # Find the combination with the highest depth for each row
    for (i in 1:nrow(df9)) {
      depths <- unlist(df9[i, depth_cols])
      sequences <- unlist(df9[i, seq_cols])

      # Filter out the sequences that are equal to RefSeq
      valid_indices <- which(sequences != df9$RefSeq[i])
      valid_depths <- depths[valid_indices]
      valid_sequences <- sequences[valid_indices]

      if (length(valid_depths) > 0) {
        max_depth_idx <- which.max(valid_depths)

        if (!is.na(max_depth_idx)) {
          new_df$HighestMM[i] <- valid_sequences[max_depth_idx]
          new_df$MMDepth[i] <- valid_depths[max_depth_idx]
        }
      }
      # Calculate coverage by summing the depth columns, ignoring NA values
      new_df$Coverage[i] <- sum(depths, na.rm = TRUE)

      # Calculate MMDepth as the percentage of MaxDepth compared with Coverage
      if (!is.na(new_df$MMDepth[i]) && new_df$Coverage[i] != 0) {
        new_df$MM_percentage[i] <- (new_df$MMDepth[i] / new_df$Coverage[i]) * 100
      } else {
        new_df$MM_percentage[i] <- NA
      }
    }
    # Save all positions file
    the_dir <- "./AllMutations"
    check_create_dir(the_dir)

    write_csv(new_df,file = paste0(the_dir,"/", file_path_sans_ext(basename(input)), "_Highest_Mutation.csv", sep=""))

  }
  lapply(AllReportFiles, process_file)
  invisible(NULL)
}
