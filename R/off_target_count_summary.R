#' off_target_count_summary
#'
#' @description What the function does.
#' @param lower minimum heteroplasmy level to take into account
#' @param upper maximum heteroplasmy level to take into account
#' @param condition_levels Default control and treated
#' @param out_dir output directory
#' @param csv_prefix Default "MutationCount"
#' @param plot_file pdf name. Default "MutationCount_grouped.pdf"
#' @return Describe what the function returns
#' @export
off_target_count_summary <- function(lower = 0.1,
                                   upper = 70,
                                   condition_levels = c("control", "treated"),
                                   out_dir       = ".",
                                   csv_prefix    = "MutationCount",
                                   plot_file     = "MutationCount_grouped.pdf"
                                   ) {

  data = Adj
  stopifnot(all(c("FileName", "CombinedName", "Condition", "AdjPercentage") %in%
                  names(data)))

  if (!is.null(out_dir) && !dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)

  ##  per sample counts ---------------------------------------------
  counts <- data %>%
    dplyr::group_by(FileName, CombinedName, Condition) %>%
    dplyr::summarise(
      count_non_missing = sum(!is.na(AdjPercentage) &
                                AdjPercentage > lower &
                                AdjPercentage < upper),
      .groups = "drop"
    )

  ## group stats (mean ± SE) ---------------------------------------
  stats <- counts %>%
    dplyr::group_by(CombinedName, Condition) %>%
    dplyr::summarise(
      avg_count = mean(count_non_missing),
      sd_count  = stats::sd(count_non_missing) / sqrt(dplyr::n()),
      .groups   = "drop"
    ) %>%
    dplyr::mutate(Condition = factor(Condition, levels = condition_levels))

  ## 4.optional CSV output ---------------------------------------------
  if (!is.null(out_dir)) {
    readr::write_csv(stats,
                     file.path(out_dir, paste0(csv_prefix, "_grouped.csv")))
    readr::write_csv(counts,
                     file.path(out_dir, paste0(csv_prefix, "_per_sample.csv")))
  }

  ## 5.build plot -------------------------------------------------------
  if (make_plot) {
    p <- ggplot2::ggplot(
      stats,
      ggplot2::aes(x = CombinedName,
                   y = avg_count,
                   fill = Condition)
    ) +
      ggplot2::geom_bar(stat = "identity", width = 0.7) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = avg_count - sd_count,
                     ymax = avg_count + sd_count),
        width = 0.2
      ) +
      ggplot2::scale_fill_manual(
        values = c(control = "skyblue", treated = "lightcoral")
      ) +
      ggplot2::labs(
        x = NULL,
        y = "No. of off-target positions"
      ) +
      ggplot2::theme_minimal(base_size = 14) +            # overall default text size
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12  # x‑axis tick labels
        ),
        axis.text.y  = ggplot2::element_text(size = 12),  # y‑axis tick labels
        axis.title.y = ggplot2::element_text(size = 14),  # y‑axis title
        legend.text  = ggplot2::element_text(size = 12),  # legend labels
        legend.title = ggplot2::element_text(size = 14)   # legend title
      )

    ## optional PDF output -------------------------------------------
    if (!is.null(out_dir))
      ggplot2::ggsave(file.path(out_dir, plot_file), p,
                      width = 7, height = 7)

    return(list(counts = counts, stats = stats, p = p))
  } else {
    return(list(counts = counts, stats = stats))
  }
}
