#' bystander_only_relevant
#'
#' Creates a heatmap of adjusted editing percentages for potential target sites surrounding the on-target site.
#' Highlights potential bystander effects within a user-defined window.
#'
#' @param BystanderDistance Integer. Number of positions upstream/downstream from the on-target site to include.
#' @param title Character. Title of the heatmap plot. Default is "Bystander effect".
#' @param xlab Character. Label for the x-axis. Default is "mtDNA position".
#' @param ylab Character. Label for the y-axis. Default is "" (blank).
#' @param fill_colours Character vector of colours used for the heatmap gradient.
#' @param fill_values Numeric vector specifying the values corresponding to
#'   `fill_colours`.
#' @param Adj Data frame containing adjusted editing percentages.
#'   Defaults to the global `Adj` object if not supplied.
#' @param SampleList Data frame containing sample metadata.
#'   Defaults to the global `SampleList` object if not supplied.
#' @param OntargetPosition Numeric. Genomic position of the intended on-target
#'   editing site. Defaults to the global `OntargetPosition` object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). The heatmap is written to the `Plots` subdirectory
#'   within this directory.
#' @return Invisibly returns the ggplot object. PNG and PDF versions of the
#'   heatmap are saved in the `Plots` subdirectory within `out_dir_base`.
#' @export
#'
#' @examples
#' \dontrun{
#' bystander_only_relevant(BystanderDistance = 10)
#' }

bystander_only_relevant <- function(
    BystanderDistance = 10,
    title = "Bystander effect",
    xlab = "mtDNA position",
    ylab = " ",
    fill_colours = c("white", "lightblue", "darkblue"),
    fill_values = c(0, 30, 100),
    Adj = NULL,
    SampleList = NULL,
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

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(OntargetPosition)) {
    if (!exists("OntargetPosition", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'OntargetPosition' must be supplied or exist in the global environment.")
    }
    OntargetPosition <- get("OntargetPosition", envir = .GlobalEnv, inherits = FALSE)
  }

  Adj$position <- as.numeric(as.character(Adj$position))

  AdjBy <- Adj[
    Adj$position >= (OntargetPosition - BystanderDistance) &
      Adj$position <= (OntargetPosition + BystanderDistance),
  ]

  # Retain required columns explicitly rather than relying on column positions
  AdjBy <- AdjBy[, c(
    "FileName",
    "position",
    "ref_base",
    "percentage",
    "Condition",
    "AdjPercentage"
  )]

  AdjBy$SampleName <- SampleList$SampleName[match(AdjBy$FileName, SampleList$FileName)]
  AdjBy$Order <- as.integer(SampleList$Order[match(AdjBy$FileName, SampleList$FileName)])

  AdjBy$SampleName <- factor(
    AdjBy$SampleName,
    levels = rev(SampleList$SampleName[order(SampleList$Order)])
  )

  # keep only positions with at least one non-NA value
  valid_positions <- sort(unique(AdjBy$position[!is.na(AdjBy$AdjPercentage)]))
  AdjBy <- AdjBy[AdjBy$position %in% valid_positions, ]

  # discrete x-axis
  AdjBy$position_f <- factor(AdjBy$position, levels = valid_positions)

  # labels for x-axis
  label_df <- Adj[
    Adj$position %in% valid_positions,
    c("position", "ref_base")
  ]
  label_df <- label_df[!duplicated(label_df$position), ]
  label_df <- label_df[order(label_df$position), ]

  position_labels <- as.character(label_df$position)
  names(position_labels) <- as.character(label_df$position)

  # rectangle around on-target
  rect_df <- NULL
  if (OntargetPosition %in% valid_positions) {
    idx <- match(OntargetPosition, valid_positions)
    rect_df <- data.frame(
      xmin = idx - 0.5,
      xmax = idx + 0.5,
      ymin = 0.5,
      ymax = length(levels(AdjBy$SampleName)) + 0.5
    )
  }

  p <- ggplot2::ggplot(
    AdjBy,
    ggplot2::aes(
      x = .data$position_f,
      y = .data$SampleName,
      fill = .data$AdjPercentage
    )
  ) +
    ggplot2::geom_tile(
      color = "white",
      linewidth = 0.5
    ) +
    {
      if (!is.null(rect_df)) {
        ggplot2::geom_rect(
          data = rect_df,
          ggplot2::aes(
            xmin = .data$xmin,
            xmax = .data$xmax,
            ymin = .data$ymin,
            ymax = .data$ymax
          ),
          inherit.aes = FALSE,
          color = "black",
          fill = NA,
          linewidth = 0.5
        )
      }
    } +
    ggplot2::scale_fill_gradientn(
      colours = fill_colours,
      values = scales::rescale(fill_values),
      na.value = "grey96"
    ) +
    ggplot2::scale_x_discrete(
      name = xlab,
      labels = position_labels
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title = "Editing (%)"
      )
    ) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 15,
        hjust = 0.5
      ),
      axis.text.x = ggplot2::element_text(
        size = 10,
        angle = 45,
        hjust = 1
      ),
      axis.text.y = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 14),
      panel.grid = ggplot2::element_blank()
    )

  the_dir <- file.path(out_dir_base, "Plots")
  check_create_dir(the_dir)

  # Adjust figure height to the number of samples
  n_samples <- length(unique(AdjBy$SampleName))
  plot_height <- max(4, min(10, 2.5 + 0.35 * n_samples))

  ggplot2::ggsave(
    filename = file.path(
      the_dir,
      "HeatmapBystanderEffect_only_relevant.png"
    ),
    plot = p,
    width = 8,
    height = plot_height,
    dpi = 300
  )

  ggplot2::ggsave(
    filename = file.path(
      the_dir,
      "HeatmapBystanderEffect_only_relevant.pdf"
    ),
    plot = p,
    width = 8,
    height = plot_height
  )

  invisible(p)
}
