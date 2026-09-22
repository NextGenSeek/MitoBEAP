#' DdCBE_df_All
#'
#' @description What the function does.
#' @param min_threshold Numeric; minimum heteroplasmy level to take into account, Default = 0
#' @param max_threshold Numeric; maximum heteroplasmy level in the controls to take into account, Default = 60
#' @param controls Numeric; number of controls that need to have heteroplasmy level above max_threshold. Default = 2
#' @param SampleList Data frame containing sample metadata.
#'   Defaults to the global `SampleList` object if not supplied.
#' @return A data frame containing the mutation data with the adjusted
#'   percentage in `AdjPercentage`. The same data are also assigned to
#'   `Adj` in the global environment for backward compatibility.
#' @export
DdCBE_df_All <- function(
    min_threshold = 0,
    max_threshold = 60,
    controls = 2,
    SampleList = NULL
) {

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv)
  }

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

  subset <- df8 %>%
    dplyr::group_by(position) %>%
    dplyr::filter(dplyr::n_distinct(FileName) >= controls) %>%
    dplyr::ungroup()

  df10 <- unique(subset$position)

  df11 <- df7 %>%
    mutate(AdjPercentage = case_when(
      position %in% df10 ~ NA,
      TRUE ~ df7$MM_percentage
    )
    )

  assign("Adj", df11, envir = .GlobalEnv)
  return(df11)

}
