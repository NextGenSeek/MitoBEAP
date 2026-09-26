#' Calculate an empirical control-derived detection limit
#'
#' Calculates an empirical limit of detection (LOD) for mitochondrial editing
#' from the distribution of editing percentages observed in control samples.
#'
#' Positions masked during control-based filtering (`AdjPercentage = NA`) are
#' excluded. An optional on-target position can also be excluded.
#'
#' @param Adj Data frame produced by `DdCBE_df()`. Must contain `position`,
#'   `Condition`, and `AdjPercentage`.
#' @param control Character string identifying the control condition.
#'   Default is `"control"`.
#' @param percentile Numeric value between 0 and 1 specifying the percentile
#'   of the control background distribution used as the empirical detection
#'   limit. Default is `0.99`.
#' @param OntargetPosition Optional numeric vector containing one or more
#'   on-target mtDNA positions to exclude from the background distribution.
#'   Default is `NULL`.
#'
#' @return A list containing the empirical LOD (`LOD`), percentile used
#'   (`percentile`), number of control observations used (`n_observations`),
#'   number of control samples represented (`n_controls`), and the control
#'   background values used to calculate the LOD (`background`).
#'
#' @details
#' The empirical LOD is defined as the specified percentile of the observed
#' control editing-percentage distribution after removal of masked positions
#' and, when supplied, on-target positions. The default 99th percentile
#' provides a conservative threshold relative to the observed control
#' background. This is an empirical background threshold rather than an
#' instrument-specific analytical limit of detection.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lod <- CalculateLOD(
#'   Adj = Adj,
#'   control = "control",
#'   percentile = 0.99,
#'   OntargetPosition = 1559
#' )
#'
#' lod$LOD
#' }
CalculateLOD <- function(
    Adj,
    control = "control",
    percentile = 0.99,
    OntargetPosition = NULL
) {

  required_columns <- c(
    "position",
    "Condition",
    "AdjPercentage"
  )

  missing_columns <- setdiff(required_columns, names(Adj))

  if (length(missing_columns) > 0) {
    stop(
      "Error: 'Adj' is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (!is.numeric(percentile) ||
      length(percentile) != 1 ||
      is.na(percentile) ||
      percentile <= 0 ||
      percentile >= 1) {
    stop("Error: 'percentile' must be a single numeric value between 0 and 1.")
  }

  if (!control %in% Adj$Condition) {
    stop(
      "Error: control condition '",
      control,
      "' was not found in 'Condition'."
    )
  }

  control_data <- Adj |>
    dplyr::filter(
      Condition == control,
      !is.na(AdjPercentage)
    )

  if (!is.null(OntargetPosition)) {
    control_data <- control_data |>
      dplyr::filter(
        !position %in% OntargetPosition
      )
  }

  if (nrow(control_data) == 0) {
    stop("Error: No control observations were available for LOD calculation.")
  }

  background <- control_data$AdjPercentage

  background <- background[
    is.finite(background)
  ]

  if (length(background) == 0) {
    stop("Error: No finite control editing percentages were available.")
  }

  lod <- as.numeric(
    stats::quantile(
      background,
      probs = percentile,
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
  )

  n_controls <- if ("SampleName" %in% names(control_data)) {
    dplyr::n_distinct(control_data$SampleName)
  } else {
    NA_integer_
  }

  result <- list(
    LOD = lod,
    percentile = percentile,
    n_observations = length(background),
    n_controls = n_controls,
    background = background
  )

  return(result)
}
