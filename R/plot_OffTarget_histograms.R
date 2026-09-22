#' plot_offTarget_histograms
#'
#' Make histograms
#'
#' @param bw Bin width
#' @param min_pct Minimum heteroplasmy percentage
#' @param max_pct Maximum heteroplasmy percentage
#' @param Adj Data frame containing editing percentages.
#'   Defaults to the global `Adj` object if not supplied.
#' @param OntargetPosition Numeric. Genomic position of the intended on-target
#'   editing site. Defaults to the global `OntargetPosition` object if not supplied.
#' @return Describe what the function returns
#' @export
plot_OffTarget_histograms <- function(
    bw = 1,
    min_pct = 0,
    max_pct = Inf,
    Adj = NULL,
    OntargetPosition = NULL
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

    ## ---- loop over samples -----------------------------------------------------
    for (nm in unique(Adj_subset$SampleName)) {

      df_sub <- dplyr::filter(Adj_subset, SampleName == nm)

      p <- ggplot(df_sub, aes(AdjPercentage)) +
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
