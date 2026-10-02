#' Plot Grouped Off-Target Editing
#'
#' Create a grouped bar chart of mean off-target editing
#'
#' Calculates the mean adjusted off-target editing percentage for each sample
#' across all evaluable positions, excluding the specified on-target position.
#' Missing values are excluded from the calculation and are not treated as
#' zero editing.
#'
#' Sample-level mean off-target editing percentages are then grouped using the
#' `CombinedName` column in `SampleList`. Bars show the group mean, individual
#' sample values can be overlaid as points, and error bars show the standard
#' deviation (SD) or standard error of the mean (SEM).
#'
#' This function summarises the overall adjusted off-target editing level and
#' does not apply the empirical background threshold used to identify
#' individual off-target positions.
#'
#' @param Adj Data frame containing adjusted editing percentages. Must contain
#'   `FileName`, `position`, and `AdjPercentage` columns.
#' @param SampleList Data frame containing sample metadata. Must contain
#'   `FileName` and `CombinedName` columns.
#' @param OntargetPosition Numeric. Position of the intended on-target edit.
#'   This position is excluded from the off-target calculation.
#' @param error Character. Error bar to display. Either `"SD"` (default),
#'   `"SEM"`, or `"none"`.
#' @param show_points Logical. If TRUE (default), individual sample-level mean
#'   off-target editing percentages are shown as points.
#' @param out_dir_base Base output directory. Defaults to the current working
#'   directory (`"."`).
#' @param y_max Optional numeric value specifying the maximum displayed
#'   y-axis value. If NULL (default), the y-axis is scaled automatically.
#'
#' @return Invisibly returns a list containing the plot (`plot`), the
#'   sample-level mean off-target editing data (`data`), and the group summary
#'   (`summary`). The plot is also saved as PNG and PDF and the summary and
#'   sample-level data are saved as CSV files.
#'
#' @export
PlotGroupedOffTarget <- function(
    Adj,
    SampleList,
    OntargetPosition,
    error = "SD",
    show_points = TRUE,
    y_max = NULL,
    out_dir_base = "."
) {

  # Check arguments --------------------------------------------------------

  error <- toupper(error)

  if (!error %in% c("SD", "SEM", "NONE")) {
    stop(
      "error must be one of: 'SD', 'SEM', or 'none'."
    )
  }

  required_adj <- c(
    "FileName",
    "position",
    "AdjPercentage"
  )

  missing_adj <- setdiff(
    required_adj,
    names(Adj)
  )

  if (length(missing_adj) > 0) {
    stop(
      "Adj is missing required column(s): ",
      paste(missing_adj, collapse = ", ")
    )
  }

  required_samplelist <- c(
    "FileName",
    "CombinedName"
  )

  missing_samplelist <- setdiff(
    required_samplelist,
    names(SampleList)
  )

  if (length(missing_samplelist) > 0) {
    stop(
      "SampleList is missing required column(s): ",
      paste(missing_samplelist, collapse = ", ")
    )
  }

  if (!is.numeric(OntargetPosition) ||
      length(OntargetPosition) != 1 ||
      is.na(OntargetPosition)) {
    stop(
      "OntargetPosition must be a single numeric value."
    )
  }


  # Prepare data -----------------------------------------------------------

  off_target_data <- Adj

  off_target_data$AdjPercentage <-
    suppressWarnings(
      as.numeric(off_target_data$AdjPercentage)
    )

  off_target_data$position <-
    suppressWarnings(
      as.numeric(off_target_data$position)
    )

  # Standardise filenames
  off_target_data$FileName <- sub(
    "_counts$",
    "",
    off_target_data$FileName
  )

  off_target_data$FileName <- sub(
    "_R30$",
    "",
    off_target_data$FileName
  )


  # Remove on-target position ---------------------------------------------

  off_target_data <- off_target_data[
    is.na(off_target_data$position) |
      off_target_data$position != OntargetPosition,
    ,
    drop = FALSE
  ]


  # Calculate sample-level mean off-target editing ------------------------

  sample_summary <- off_target_data |>
    dplyr::group_by(.data$FileName) |>
    dplyr::summarise(
      EvaluatedPositions = sum(
        !is.na(.data$AdjPercentage)
      ),
      MeanOffTarget = mean(
        .data$AdjPercentage,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  # Samples with no evaluable positions produce NaN
  sample_summary$MeanOffTarget[
    is.nan(sample_summary$MeanOffTarget)
  ] <- NA_real_


  # Add group information -------------------------------------------------

  idx <- match(
    sample_summary$FileName,
    SampleList$FileName
  )

  sample_summary$CombinedName <-
    SampleList$CombinedName[idx]

  if (anyNA(sample_summary$CombinedName)) {

    unmatched <- unique(
      sample_summary$FileName[
        is.na(sample_summary$CombinedName)
      ]
    )

    stop(
      "Sample metadata could not be matched for: ",
      paste(unmatched, collapse = ", ")
    )
  }


  # Preserve SampleList group order ---------------------------------------

  group_order <- unique(
    SampleList$CombinedName[
      SampleList$CombinedName %in%
        sample_summary$CombinedName
    ]
  )

  sample_summary$CombinedName <- factor(
    sample_summary$CombinedName,
    levels = group_order
  )


  # Calculate group statistics -------------------------------------------

  group_summary <- sample_summary |>
    dplyr::group_by(.data$CombinedName) |>
    dplyr::summarise(
      n = sum(!is.na(.data$MeanOffTarget)),
      Mean = mean(
        .data$MeanOffTarget,
        na.rm = TRUE
      ),
      SD = stats::sd(
        .data$MeanOffTarget,
        na.rm = TRUE
      ),
      SEM = .data$SD / sqrt(.data$n),
      .groups = "drop"
    )

  # SD and SEM are undefined for n = 1
  group_summary$SD[
    group_summary$n < 2
  ] <- NA_real_

  group_summary$SEM[
    group_summary$n < 2
  ] <- NA_real_


  # Select error bar ------------------------------------------------------

  if (error == "SD") {
    group_summary$Error <- group_summary$SD
  }

  if (error == "SEM") {
    group_summary$Error <- group_summary$SEM
  }

  if (error == "NONE") {
    group_summary$Error <- 0
  }


  # Create plot -----------------------------------------------------------

  p <- ggplot2::ggplot(
    group_summary,
    ggplot2::aes(
      x = .data$CombinedName,
      y = .data$Mean
    )
  ) +
    ggplot2::geom_col(
      width = 0.7
    )

  if (error != "NONE") {

    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = pmax(0, .data$Mean - .data$Error),
          ymax = .data$Mean + .data$Error
        ),
        width = 0.2
      )
  }

  if (show_points) {

    p <- p +
      ggplot2::geom_point(
        data = sample_summary,
        ggplot2::aes(
          x = .data$CombinedName,
          y = .data$MeanOffTarget
        ),
        inherit.aes = FALSE,
        position = ggplot2::position_jitter(
          width = 0.08,
          height = 0
        ),
        size = 2.5
      )
  }

  p <- p +
    ggplot2::labs(
      x = NULL,
      y = "Mean off-target editing (%)"
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(
        mult = c(0, 0.08)
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

  # Optionally restrict the displayed y-axis range without removing data
  if (!is.null(y_max)) {
    p <- p +
      ggplot2::coord_cartesian(
        ylim = c(0, y_max)
      )
  }

  # Save results ----------------------------------------------------------

  output_dir <- file.path(
    out_dir_base,
    "GroupedBarChart"
  )

  check_create_dir(output_dir)

  readr::write_csv(
    sample_summary,
    file.path(
      output_dir,
      "Grouped_OffTarget_sample_means.csv"
    )
  )

  readr::write_csv(
    group_summary,
    file.path(
      output_dir,
      "Grouped_OffTarget_summary.csv"
    )
  )

  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      "Grouped_OffTarget_barplot.png"
    ),
    plot = p,
    width = 9,
    height = 6,
    dpi = 300
  )

  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      "Grouped_OffTarget_barplot.pdf"
    ),
    plot = p,
    width = 9,
    height = 6
  )


  # Return results --------------------------------------------------------

  invisible(
    list(
      plot = p,
      data = sample_summary,
      summary = group_summary
    )
  )
}
