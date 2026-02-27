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
off_target_count_summary <- function(Adj = get("Adj", envir = .GlobalEnv),
                                      SampleList = get("SampleList", envir = .GlobalEnv),
                                      lower = 0.1,
                                      upper = 70,
                                      condition_levels = c("control", "treated"),
                                      out_dir       = ".",
                                      csv_prefix    = "MutationCount",
                                      plot_file     = "MutationCount_grouped.pdf",
                                      make_plot     = TRUE,
                                      combined_col_in_samplelist = "CombinedName",
                                      condition_col = "Condition") {
    
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
            ggplot2::aes(x = .data$CombinedName, y = .data$avg_count, fill = .data$Condition)
        ) +
            ggplot2::geom_col(width = 0.7) +
            ggplot2::geom_errorbar(
                ggplot2::aes(ymin = .data$avg_count - .data$se_count,
                             ymax = .data$avg_count + .data$se_count),
                width = 0.2
            ) +
            ggplot2::scale_fill_manual(values = c(control = "skyblue", treated = "lightcoral")) +
            ggplot2::labs(x = NULL, y = "No. of off-target positions") +
            ggplot2::theme_minimal(base_size = 14) +
            ggplot2::theme(
                axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
                axis.text.y  = ggplot2::element_text(size = 12),
                axis.title.y = ggplot2::element_text(size = 14),
                legend.text  = ggplot2::element_text(size = 12),
                legend.title = ggplot2::element_text(size = 14)
            )
        
        if (!is.null(out_dir))
            ggplot2::ggsave(file.path(out_dir, plot_file), p, width = 7, height = 7)
        
        return(list(counts = counts, stats = stats, p = p, data_joined = data))
    }
    
    list(counts = counts, stats = stats, data_joined = data)
}
