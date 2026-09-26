#' AdjScatter
#'
#' Creates scatterplots showing adjusted editing percentages for each sample,
#' highlighting and labeling positions above a user-defined threshold.
#'
#' @param min_threshold Numeric; minimum adjusted editing percentage to display (default = 0).
#' @param labelPercentage Numeric; threshold above which positions are labeled on the plot (default = 10).
#' @param Adj Data frame containing adjusted editing percentages.
#'   Defaults to the global `Adj` object if not supplied.
#' @param OntargetPosition Numeric. Genomic position of the intended on-target
#'   editing site. Defaults to the global `OntargetPosition` object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). Plots are written to the `Plots/AdjustedPlots`
#'   subdirectory within this directory.
#' @keywords plot, heteroplasmy, scatter, labeling
#' @export
#'
#' @examples
#' \dontrun{
#' AdjScatter(min_threshold = 0, labelPercentage = 10)
#' }

AdjScatter <- function(
    min_threshold = 0,
    labelPercentage = 10,
    Adj = NULL,
    OntargetPosition = NULL,
    out_dir_base = "."
) {

  # Use supplied objects; fall back to global objects for backward compatibility
  if (is.null(Adj)) {
    if (!exists("Adj", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'Adj' must be supplied or created first (e.g., via DdCBE_df()).")
    }
    Adj <- get("Adj", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(OntargetPosition)) {
    if (!exists("OntargetPosition", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'OntargetPosition' must be supplied or exist in the global environment.")
    }
    OntargetPosition <- get("OntargetPosition", envir = .GlobalEnv, inherits = FALSE)
  }

  # Filter and clean data
  data <- Adj[Adj$AdjPercentage >= min_threshold, ]
  data <- tidyr::drop_na(data)

  if (nrow(data) == 0) {
    stop("No data points meet the minimum threshold for plotting.")
  }

sample_names <- unique(data$SampleName)

for (sample_name in sample_names) {
  sample_data <- subset(data, SampleName == sample_name)
  sample_data$legend <- ifelse(sample_data$position == OntargetPosition, "On target", "Off target / background")

  # Create output directory if it doesn't exist
  the_dir <- file.path(out_dir_base, "Plots", "AdjustedPlots")
  check_create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  check_create_dir(the_dir)

  # Make a scatterplot for each sample
  p <- ggplot2::ggplot(
    sample_data,
    ggplot2::aes(
      x = position,
      y = AdjPercentage,
      colour = legend
    )
  ) +
    ggplot2::geom_point(alpha = 0.3) +
    ggrepel::geom_text_repel(
      data = subset(
        sample_data,
        AdjPercentage > labelPercentage
      ),
      ggplot2::aes(label = position),
      size = 3,
      show.legend = FALSE
    ) +
    ggplot2::labs(
      title = sample_name,
      x = "mtDNA position",
      y = "Adjusted editing (%)",
      colour = NULL
    ) +
    ggplot2::ylim(0, 100) +
    ggplot2::scale_color_manual(
      values = c(
        "Off target / background" = "black",
        "On target" = "red"
      )
    ) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 13),
      legend.position = "right"
    )

  # Save high-resolution PNG and vector PDF
  outname_png <- file.path(
    the_dir,
    paste0(sample_name, ".png")
  )

  outname_pdf <- file.path(
    the_dir,
    paste0(sample_name, ".pdf")
  )

  ggplot2::ggsave(
    outname_png,
    plot = p,
    width = 8,
    height = 4,
    dpi = 300
  )

  ggplot2::ggsave(
    outname_pdf,
    plot = p,
    width = 8,
    height = 4
  )

  }

}
