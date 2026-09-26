#' plot_OffTarget_histograms
#'
#' Creates per-sample histograms of adjusted off-target editing percentages.
#'
#' @param bw Numeric. Histogram bin width.
#' @param min_pct Numeric. Minimum adjusted editing percentage to include.
#' @param max_pct Numeric. Maximum adjusted editing percentage to include.
#' @param Adj Data frame containing adjusted editing percentages.
#'   Defaults to the global `Adj` object if not supplied.
#' @param OntargetPosition Numeric. Genomic position of the intended on-target
#'   editing site. Defaults to the global `OntargetPosition` object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). Histograms are written to the `Plots` subdirectory
#'   within this directory.
#' @return Invisibly returns `NULL`. PNG and PDF versions of each sample
#'   histogram are saved to the `Plots` subdirectory within `out_dir_base`.
#' @export
plot_OffTarget_histograms <- function(
    bw = 1,
    min_pct = 0,
    max_pct = Inf,
    Adj = NULL,
    OntargetPosition = NULL,
    out_dir_base = "."
) {

  # Use supplied objects; fall back to global objects for backward compatibility
  if (is.null(Adj)) {
    if (!exists("Adj", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'Adj' must be supplied or exist in the global environment.")
    }
    Adj <- get("Adj", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(OntargetPosition)) {
    if (!exists("OntargetPosition", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'OntargetPosition' must be supplied or exist in the global environment.")
    }
    OntargetPosition <- get("OntargetPosition", envir = .GlobalEnv, inherits = FALSE)
  }

    ## ---- sanity checks ---------------------------------------------------------
  stopifnot(
    "SampleName" %in% names(Adj),
    "AdjPercentage" %in% names(Adj),
    is.numeric(bw), bw > 0,
    is.numeric(min_pct), is.numeric(max_pct),
    min_pct < max_pct
  )

    # Remove OntargetPosition
    Adj <- Adj[Adj$position != OntargetPosition, ]

    ## ---- keep only chosen range -----------------------------------------------
    Adj_subset <- Adj |>
      dplyr::filter(
        AdjPercentage >= min_pct,
        AdjPercentage <= max_pct
      )

    if (nrow(Adj_subset) == 0L) {
      warning("No rows fall in the selected range; nothing plotted.")
      return(invisible(NULL))
    }

    ## ---- global bin edges for that range --------------------------------------
    rng <- range(Adj_subset$AdjPercentage, na.rm = TRUE)
    lower_edge  <- floor(rng[1] / bw) * bw
    upper_edge  <- ceiling(rng[2] / bw) * bw
    breaks_vec  <- seq(lower_edge, upper_edge, by = bw)
    message(" using ", length(breaks_vec) - 1,
            " bins of width ", bw,
            "(range ", lower_edge, "-", upper_edge, ")")

    ## ---- output directory ------------------------------------------------------
    the_dir <- file.path(out_dir_base, "Plots")
    check_create_dir(the_dir)

    ## ---- loop over samples -----------------------------------------------------
    for (nm in unique(Adj_subset$SampleName)) {

      df_sub <- dplyr::filter(Adj_subset, SampleName == nm)

      p <- ggplot2::ggplot(
        df_sub,
        ggplot2::aes(x = AdjPercentage)
      ) +
        ggplot2::geom_histogram(
          breaks = breaks_vec,
          fill = "darkseagreen",
          colour = "black"
        ) +
        ggplot2::scale_x_continuous(
          limits = c(lower_edge, upper_edge),
          breaks = pretty(breaks_vec, n = 10),
          minor_breaks = breaks_vec
        ) +
        ggplot2::labs(
          title = paste("Off-target editing:", nm),
          x = "Adjusted editing (%)",
          y = "Number of mtDNA positions"
        ) +
        ggplot2::theme_classic(base_size = 13) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 15),
          axis.text = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 14)
        )

      safe_nm <- gsub("[^A-Za-z0-9_]", "_", nm)

      safe_nm <- gsub("[^A-Za-z0-9_]", "_", nm)

      ggplot2::ggsave(
        filename = file.path(
          the_dir,
          paste0("Histogram_", safe_nm, ".png")
        ),
        plot = p,
        width = 7,
        height = 5,
        dpi = 300
      )

      ggplot2::ggsave(
        filename = file.path(
          the_dir,
          paste0("Histogram_", safe_nm, ".pdf")
        ),
        plot = p,
        width = 7,
        height = 5
      )
    }

    invisible(NULL)
  }
