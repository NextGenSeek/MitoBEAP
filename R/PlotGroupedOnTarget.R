#' PlotGroupedOnTarget
#'
#' Create a grouped bar chart of on-target editing
#'
#' Creates a bar chart showing the mean on-target editing percentage for each
#' experimental group. Individual sample values are overlaid as points and
#' error bars show the standard deviation (SD) or standard error of the mean
#' (SEM).
#'
#' Samples are assigned to groups using the `CombinedName` column in
#' `SampleList`.
#'
#' @param OnTarget Data frame containing on-target editing percentages.
#'   Must contain the columns `FileName` and `On target \%`.
#' @param SampleList Data frame containing sample metadata. Must contain
#'   `FileName` and `CombinedName` columns.
#' @param error Character. Error bar to display. Either `"SD"` (default),
#'   `"SEM"`, or `"none"`.
#' @param show_points Logical. If TRUE (default), individual sample values are
#'   shown as points.
#' @param out_dir_base Base output directory. Defaults to the current working
#'   directory (`"."`).
#'
#' @return Invisibly returns a list containing the plot (`plot`), the
#'   sample-level data (`data`), and the group summary (`summary`).
#'   The plot is also saved as PNG and PDF and the summary is saved as CSV.
#'
#' @export
PlotGroupedOnTarget <- function(
    OnTarget,
    SampleList,
    error = "SD",
    show_points = TRUE,
    out_dir_base = "."
) {

  # Check arguments --------------------------------------------------------

  error <- toupper(error)

  if (!error %in% c("SD", "SEM", "NONE")) {
    stop(
      "error must be one of: 'SD', 'SEM', or 'none'."
    )
  }

  required_ontarget <- c(
    "FileName",
    "On target %"
  )

  missing_ontarget <- setdiff(
    required_ontarget,
    names(OnTarget)
  )

  if (length(missing_ontarget) > 0) {
    stop(
      "OnTarget is missing required column(s): ",
      paste(missing_ontarget, collapse = ", ")
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


  # Prepare sample-level data ---------------------------------------------

  plot_data <- OnTarget

  # Remove standard suffixes if present
  plot_data$FileName <- sub(
    "_counts$",
    "",
    plot_data$FileName
  )

  plot_data$FileName <- sub(
    "_R30$",
    "",
    plot_data$FileName
  )

  # Add group information
  idx <- match(
    plot_data$FileName,
    SampleList$FileName
  )

  plot_data$CombinedName <-
    SampleList$CombinedName[idx]

  # Stop if samples could not be matched
  if (anyNA(plot_data$CombinedName)) {

    unmatched <- unique(
      plot_data$FileName[
        is.na(plot_data$CombinedName)
      ]
    )

    stop(
      "Sample metadata could not be matched for: ",
      paste(unmatched, collapse = ", ")
    )
  }

  # Convert editing percentage to numeric
  plot_data[["On target %"]] <-
    suppressWarnings(
      as.numeric(plot_data[["On target %"]])
    )

  if (all(is.na(plot_data[["On target %"]]))) {
    stop(
      "No numeric on-target editing percentages were found."
    )
  }


  # Preserve SampleList group order ---------------------------------------

  group_order <- unique(
    SampleList$CombinedName[
      SampleList$CombinedName %in%
        plot_data$CombinedName
    ]
  )

  plot_data$CombinedName <- factor(
    plot_data$CombinedName,
    levels = group_order
  )


  # Calculate group statistics -------------------------------------------

  group_summary <- plot_data |>
    dplyr::group_by(.data$CombinedName) |>
    dplyr::summarise(
      n = sum(!is.na(.data[["On target %"]])),
      Mean = mean(
        .data[["On target %"]],
        na.rm = TRUE
      ),
      SD = stats::sd(
        .data[["On target %"]],
        na.rm = TRUE
      ),
      SEM = .data$SD / sqrt(.data$n),
      .groups = "drop"
    )

  # SD is undefined for n = 1
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
        data = plot_data,
        ggplot2::aes(
          x = .data$CombinedName,
          y = .data[["On target %"]]
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
      y = "On-target editing (%)"
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


  # Save results ----------------------------------------------------------

  output_dir <- file.path(
    out_dir_base,
    "GroupedBarChart"
  )

  check_create_dir(output_dir)

  readr::write_csv(
    group_summary,
    file.path(
      output_dir,
      "Grouped_OnTarget_summary.csv"
    )
  )

  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      "Grouped_OnTarget_barplot.png"
    ),
    plot = p,
    width = 9,
    height = 6,
    dpi = 300
  )

  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      "Grouped_OnTarget_barplot.pdf"
    ),
    plot = p,
    width = 9,
    height = 6
  )


  # Return results --------------------------------------------------------

  invisible(
    list(
      plot = p,
      data = plot_data,
      summary = group_summary
    )
  )
}
