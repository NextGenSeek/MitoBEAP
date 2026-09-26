#' off_target_count_summary
#'
#' @description
#' Summarises the number of adjusted off-target positions within a specified
#' editing range for each sample and experimental group. Group means and
#' standard errors are calculated and can be displayed as a bar chart.
#' @param Adj Data frame containing adjusted off-target editing data.
#'   Defaults to the global `Adj` object if not supplied.
#' @param SampleList Data frame containing sample metadata.
#'   Defaults to the global `SampleList` object if not supplied.
#' @param lower Numeric. Minimum adjusted editing percentage to include.
#'   This can be set to the empirical control-derived detection limit returned
#'   by `CalculateLOD()`.
#' @param upper Numeric. Maximum adjusted editing percentage to include.
#' @param condition_levels Default control and treated
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). By default, output files are written to the `Plots`
#'   subdirectory within this directory.
#' @param out_dir Optional explicit output directory. If supplied, this overrides
#'   `out_dir_base` for backward compatibility.
#' @param csv_prefix Default "MutationCount"
#' @param plot_file pdf name. Default "MutationCount_grouped.pdf"
#' @param make_plot Logical. If `TRUE`, creates the grouped off-target count plot.
#'   Default is `TRUE`.
#' @param combined_col_in_samplelist Character. Name of the column in
#'   `SampleList` containing the combined sample/group name.
#'   Default is `"CombinedName"`.
#' @param condition_col Character. Name of the column containing the experimental
#'   condition. Default is `"Condition"`.
#' @param OntargetPosition Optional numeric vector containing one or more
#'   intended on-target mtDNA positions to exclude from the off-target count.
#'   Default is `NULL`.
#' @return A list containing per-sample counts (`counts`), grouped summary
#'   statistics (`stats`), the sample-level Wilcoxon rank-sum comparison
#'   (`burden_test`), and the joined input data (`data_joined`).
#'   `burden_test` is `NULL` when both conditions are not represented by
#'   at least two samples. When `make_plot = TRUE`, the returned list also
#'   contains the ggplot object (`p`).
#' @export
off_target_count_summary <- function(
    Adj = NULL,
    SampleList = NULL,
    lower = 0.1,
    upper = 70,
    OntargetPosition = NULL,
    condition_levels = c("control", "treated"),
    out_dir_base = ".",
    out_dir = NULL,
    csv_prefix = "MutationCount",
    plot_file = "MutationCount_grouped.pdf",
    make_plot = TRUE,
    combined_col_in_samplelist = "CombinedName",
    condition_col = "Condition"
) {
  if (is.null(Adj)) {
    if (!exists("Adj", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'Adj' must be supplied or exist in the global environment.")
    }
    Adj <- get("Adj", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv, inherits = FALSE)
  }

  # Determine output directory
  if (is.null(out_dir)) {
    out_dir <- file.path(out_dir_base, "Plots")
  }

    # --- checks -----------------------------------------------------------
    stopifnot(is.data.frame(Adj), is.data.frame(SampleList))
    stopifnot(all(c("FileName", "AdjPercentage") %in% names(Adj)))
    stopifnot("FileName" %in% names(SampleList))
    stopifnot(combined_col_in_samplelist %in% names(SampleList))
    stopifnot(condition_col %in% names(Adj) || condition_col %in% names(SampleList))

    if (!is.null(out_dir) && !dir.exists(out_dir))
        dir.create(out_dir, recursive = TRUE)

    # --- join CombinedName (and Condition if needed) ----------------------
    join_cols <- c("FileName", combined_col_in_samplelist)
    if (!(condition_col %in% names(Adj)) && (condition_col %in% names(SampleList))) {
        join_cols <- c(join_cols, condition_col)
    }

    data <- dplyr::left_join(
        Adj,
        dplyr::distinct(SampleList[, join_cols]),
        by = "FileName"
    )

    # rename joined CombinedName to the expected column name
    if (combined_col_in_samplelist != "CombinedName") {
        data <- dplyr::rename(data, CombinedName = dplyr::all_of(combined_col_in_samplelist))
    }

    # if Condition exists in both, prefer Adj's and only fill missing from SampleList
    if (condition_col %in% names(Adj) && condition_col %in% names(SampleList)) {
        # after join, SampleList condition will be suffixed .y typically; make it robust:
        # easiest: re-join only CombinedName when Adj already has Condition
        data <- dplyr::left_join(
            Adj,
            dplyr::distinct(SampleList[, c("FileName", combined_col_in_samplelist)]),
            by = "FileName"
        )
        if (combined_col_in_samplelist != "CombinedName") {
            data <- dplyr::rename(data, CombinedName = dplyr::all_of(combined_col_in_samplelist))
        }
    }

    # final required columns now present?
    stopifnot(all(c("FileName", "CombinedName", "AdjPercentage", condition_col) %in% names(data)))

    # Exclude intended on-target position(s), if supplied
    if (!is.null(OntargetPosition)) {
      data <- data %>%
        dplyr::filter(!.data$position %in% OntargetPosition)
    }

    # --- per-sample counts ------------------------------------------------
    counts <- data %>%
        dplyr::group_by(.data$FileName, .data$CombinedName, .data[[condition_col]]) %>%
        dplyr::summarise(
            count_non_missing = sum(!is.na(.data$AdjPercentage) &
                                        .data$AdjPercentage > lower &
                                        .data$AdjPercentage < upper),
            .groups = "drop"
        ) %>%
        dplyr::rename(Condition = dplyr::all_of(condition_col)) %>%
        dplyr::mutate(Condition = factor(.data$Condition, levels = condition_levels))

    # --- sample-level statistical comparison -------------------------------
    burden_test <- NULL

    test_data <- counts %>%
      dplyr::filter(
        !is.na(.data$Condition),
        !is.na(.data$count_non_missing)
      )

    conditions_present <- unique(as.character(test_data$Condition))

    if (all(condition_levels %in% conditions_present)) {

      control_counts <- test_data$count_non_missing[
        test_data$Condition == condition_levels[1]
      ]

      treated_counts <- test_data$count_non_missing[
        test_data$Condition == condition_levels[2]
      ]

      if (length(control_counts) >= 2 && length(treated_counts) >= 2) {

        wt <- stats::wilcox.test(
          treated_counts,
          control_counts,
          alternative = "two.sided",
          exact = FALSE
        )

        burden_test <- data.frame(
          test = "Wilcoxon rank-sum",
          control_condition = condition_levels[1],
          treated_condition = condition_levels[2],
          n_control = length(control_counts),
          n_treated = length(treated_counts),
          median_control = stats::median(control_counts),
          median_treated = stats::median(treated_counts),
          W = unname(wt$statistic),
          p_value = wt$p.value
        )
      }
    }

    # --- group stats (mean ± SE) -----------------------------------------
    stats <- counts %>%
        dplyr::group_by(.data$CombinedName, .data$Condition) %>%
        dplyr::summarise(
            avg_count = mean(.data$count_non_missing),
            se_count  = stats::sd(.data$count_non_missing) / sqrt(dplyr::n()),
            .groups   = "drop"
        )

    # --- optional CSV output ---------------------------------------------
    if (!is.null(out_dir)) {
        readr::write_csv(stats,
                         file.path(out_dir, paste0(csv_prefix, "_grouped.csv")))
        readr::write_csv(counts,
                         file.path(out_dir, paste0(csv_prefix, "_per_sample.csv")))
    }

    # --- plot -------------------------------------------------------------
    if (make_plot) {
      p <- ggplot2::ggplot(
        stats,
        ggplot2::aes(
          x = .data$CombinedName,
          y = .data$avg_count,
          fill = .data$Condition
        )
      ) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data$avg_count - .data$se_count,
            ymax = .data$avg_count + .data$se_count
          ),
          width = 0.2
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            control = "skyblue",
            treated = "lightcoral"
          )
        ) +
        ggplot2::labs(
          x = NULL,
          y = "Number of off-target positions",
          fill = "Condition"
        ) +
        ggplot2::theme_classic(base_size = 11) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            size = 10
          ),
          axis.text.y = ggplot2::element_text(size = 10),
          axis.title.y = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 10),
          legend.title = ggplot2::element_text(size = 11)
        )

        if (!is.null(out_dir))
            ggplot2::ggsave(file.path(out_dir, plot_file), p, width = 7, height = 7)

      return(list(
        counts = counts,
        stats = stats,
        burden_test = burden_test,
        p = p,
        data_joined = data
      ))
    }

    list(
      counts = counts,
      stats = stats,
      burden_test = burden_test,
      data_joined = data
    )
}
