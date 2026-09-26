#' Compare editing between treated and control samples
#'
#' Performs a per-position comparison of mitochondrial editing between treated
#' and control samples using Fisher's exact test. Read counts are pooled across
#' samples within each condition before testing.
#'
#' Positions masked during control-based filtering are excluded from the
#' analysis (`AdjPercentage = NA`).
#'
#' @param Adj Data frame produced by `DdCBE_df()`. Must contain `position`,
#'   `Condition`, `depth`, `MutReads`, and `AdjPercentage`.
#' @param treated Character string identifying the treated condition.
#'   Default is `"treated"`.
#' @param control Character string identifying the control condition.
#'   Default is `"control"`.
#'
#' @return A data frame containing one row per mtDNA position with pooled
#'   treated and control read counts, editing percentages, the difference
#'   between conditions, Fisher's exact test odds ratio,
#'   95% Wilson confidence intervals for the pooled editing percentages, raw p-value, and
#'   Benjamini-Hochberg adjusted p-value.
#'
#' @details
#' Fisher's exact test is performed on pooled edited and non-edited sequencing
#' reads for each position. This tests differences in sequencing-read
#' proportions and does not model biological variability between replicate
#' samples.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- CompareEditing(
#'   Adj = Adj,
#'   treated = "treated",
#'   control = "control"
#' )
#' }
CompareEditing <- function(
    Adj,
    treated = "treated",
    control = "control"
) {

  required_columns <- c(
    "position",
    "Condition",
    "depth",
    "MutReads",
    "AdjPercentage"
  )

  missing_columns <- setdiff(required_columns, names(Adj))

  if (length(missing_columns) > 0) {
    stop(
      "Error: 'Adj' is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (!treated %in% Adj$Condition) {
    stop("Error: treated condition '", treated, "' was not found in 'Condition'.")
  }

  if (!control %in% Adj$Condition) {
    stop("Error: control condition '", control, "' was not found in 'Condition'.")
  }

  # Exclude positions masked during control-based filtering
  data <- Adj |>
    dplyr::filter(
      !is.na(.data$AdjPercentage),
      .data$Condition %in% c(treated, control)
    )

  # Pool read counts within each condition at each mtDNA position
  pooled <- data |>
    dplyr::group_by(.data$position, .data$Condition) |>
    dplyr::summarise(
      MutReads = sum(.data$MutReads, na.rm = TRUE),
      depth = sum(.data$depth, na.rm = TRUE),
      .groups = "drop"
    )

  treated_data <- pooled |>
    dplyr::filter(.data$Condition == treated) |>
    dplyr::select(
      .data$position,
      treated_mut_reads = .data$MutReads,
      treated_depth = .data$depth
    )

  control_data <- pooled |>
    dplyr::filter(.data$Condition == control) |>
    dplyr::select(
      .data$position,
      control_mut_reads = .data$MutReads,
      control_depth = .data$depth
    )

  results <- dplyr::inner_join(
    treated_data,
    control_data,
    by = "position"
  )

  if (nrow(results) == 0) {
    stop("Error: No positions were available for comparison.")
  }

  # Calculate non-edited reads
  results <- results |>
    dplyr::mutate(
      treated_nonedited_reads =
        .data$treated_depth - .data$treated_mut_reads,
      control_nonedited_reads =
        .data$control_depth - .data$control_mut_reads
    )

  # Editing percentages
  results <- results |>
    dplyr::mutate(
      treated_percentage =
        100 * .data$treated_mut_reads / .data$treated_depth,
      control_percentage =
        100 * .data$control_mut_reads / .data$control_depth,
      difference =
        .data$treated_percentage - .data$control_percentage
    )

  # Calculate 95% Wilson confidence intervals for editing percentages
  z <- stats::qnorm(0.975)

  wilson_interval <- function(x, n) {

    p <- x / n

    denominator <- 1 + (z^2 / n)

    centre <- (
      p + (z^2 / (2 * n))
    ) / denominator

    margin <- (
      z *
        sqrt(
          (p * (1 - p) / n) +
            (z^2 / (4 * n^2))
        )
    ) / denominator

    c(
      lower = 100 * (centre - margin),
      upper = 100 * (centre + margin)
    )
  }

  treated_ci <- t(
    mapply(
      wilson_interval,
      results$treated_mut_reads,
      results$treated_depth
    )
  )

  control_ci <- t(
    mapply(
      wilson_interval,
      results$control_mut_reads,
      results$control_depth
    )
  )

  results$treated_ci_lower <- treated_ci[, "lower"]
  results$treated_ci_upper <- treated_ci[, "upper"]
  results$control_ci_lower <- control_ci[, "lower"]
  results$control_ci_upper <- control_ci[, "upper"]

  # Fisher's exact test at each position
  fisher_results <- lapply(
    seq_len(nrow(results)),
    function(i) {

      test_table <- matrix(
        c(
          results$treated_mut_reads[i],
          results$treated_nonedited_reads[i],
          results$control_mut_reads[i],
          results$control_nonedited_reads[i]
        ),
        nrow = 2,
        byrow = TRUE
      )

      test <- stats::fisher.test(test_table)

      data.frame(
        odds_ratio = unname(test$estimate),
        p_value = test$p.value
      )
    }
  )

  fisher_results <- dplyr::bind_rows(fisher_results)

  results$odds_ratio <- fisher_results$odds_ratio
  results$p_value <- fisher_results$p_value

  # Correct for multiple testing
  results$p_adj <- stats::p.adjust(
    results$p_value,
    method = "BH"
  )

  results <- results |>
    dplyr::arrange(.data$p_adj, .data$position)

  return(results)
}
