#' DdCBE_df_All
#'
#' @description What the function does.
#' @param min_threshold Numeric; minimum heteroplasmy level to take into account, Default = 0
#' @param max_threshold Numeric; maximum heteroplasmy level in the controls to take into account, Default = 60
#' @param controls Numeric; number of controls that need to have heteroplasmy level above max_threshold. Default = 2
#' @return Describe what the function returns
#' @export
DdCBE_df_All = function(min_threshold = 0, max_threshold = 60, controls = 2) {

  AllMutFiles <- list.files("./AllMutations", pattern = "*_Highest_Mutation.csv",
                            full.names = TRUE)
  df7 <- AllMutFiles %>%
    purrr::set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>% # Name without extension
    purrr::map_df(read_csv,
                  col_names = TRUE,
                  skip = 0,
                  .id = "FileName")
  df7$FileName <- gsub(pattern = "_R30_allMutations_Highest_Mutation", "", x=df7$FileName)

  idx5 <- match(df7$FileName, SampleList$FileName)
  df7$SampleName <- SampleList$SampleName [idx5]
  df7$Condition <- SampleList$Condition [idx5]

  # Apply max thresholds on control samples to know which positions to ignore
  df8 <- df7[df7$Condition == "control" & (df7$MM_percentage >= max_threshold), ]

  # Position above/below WT threshold in n (=all control) samples
  df8 %>%
    group_by(position) %>%
    filter(n() == controls) -> subset #These are the positions we want to ignore

  #.GlobalEnv$df9 <- subset
  df10 <- unique(as.vector(df8$position))
  #.GlobalEnv$df10 <- df10

  df11 <- df7 %>%
    mutate(AdjPercentage = case_when(
      position %in% df10 ~ NA,
      TRUE ~ df7$MM_percentage
    )
    )

  .GlobalEnv$Adj <- df11

}
