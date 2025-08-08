#' plot_offTarget_histograms
#'
#' Make histograms
#'
#' @param bw Bin width
#' @param min_pct Minimum heteroplasmy percentage
#' @param max_pct Maximum heteroplasmy percentage
#' @return Describe what the function returns
#' @export
plot_OffTarget_histograms <- function(bw = 1,
                                  min_pct  = 0,
                                  max_pct  =  Inf) {

    ## ---- sanity checks ---------------------------------------------------------
    stopifnot(exists("Adj"),
              "SampleName" %in% names(Adj),
              "percentage" %in% names(Adj),
              is.numeric(bw), bw > 0,
              is.numeric(min_pct), is.numeric(max_pct),
              min_pct < max_pct)

    # Remove OntargetPosition
    Adj <- Adj[Adj$position != OntargetPosition, ]

    ## ---- keep only chosen range -----------------------------------------------
    Adj_subset <- Adj |>
      dplyr::filter(percentage >= min_pct,
             percentage <= max_pct)

    if (nrow(Adj_subset) == 0L) {
      warning("No rows fall in the selected range; nothing plotted.")
      return(invisible(NULL))
    }

    ## ---- global bin edges for that range --------------------------------------
    rng         <- range(Adj_subset$percentage, na.rm = TRUE)
    lower_edge  <- floor(rng[1] / bw) * bw
    upper_edge  <- ceiling(rng[2] / bw) * bw
    breaks_vec  <- seq(lower_edge, upper_edge, by = bw)
    message(" using ", length(breaks_vec) - 1,
            " bins of width ", bw,
            "(range ", lower_edge, "-", upper_edge, ")")

    ## ---- loop over samples -----------------------------------------------------
    for (nm in unique(Adj_subset$SampleName)) {

      df_sub <- dplyr::filter(Adj_subset, SampleName == nm)

      p <- ggplot(df_sub, aes(percentage)) +
        geom_histogram(breaks = breaks_vec,
                       fill   = "darkseagreen",
                       colour = "black") +
        scale_x_continuous(
          limits       = c(lower_edge, upper_edge),
          breaks       = pretty(breaks_vec, n = 10),   # << only ~10 labels
          minor_breaks = breaks_vec                    # keep light minor gridlines
        ) +
        labs(title = paste("Mutation Histogram _", nm),
             x     = "Heteroplasmy (%)",
             y     = "No. of mtDNA positions") +
        theme_minimal()

      safe_nm <- gsub("[^A-Za-z0-9_]", "_", nm)

      ggsave(
        filename = paste0("Histogram_", safe_nm, ".pdf"),
        plot     = p,
        width    = 7,
        height   = 5
      )
    }

    invisible(NULL)
  }
