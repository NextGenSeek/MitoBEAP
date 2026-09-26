#' Bystander
#'
#' Creates a heatmap of adjusted editing percentages for positions surrounding the on-target site.
#' Highlights potential bystander effects within a user-defined window.
#'
#' @param BystanderDistance Integer. Number of positions upstream/downstream from the on-target site to include.
#' @param title Character. Title of the heatmap plot. Default is "Bystander effect".
#' @param xlab Character. Label for the x-axis. Default is "mtDNA position".
#' @param ylab Character. Label for the y-axis. Default is "" (blank).
#' @param Adj Data frame containing adjusted editing percentages.
#'   Defaults to the global `Adj` object if not supplied.
#' @param SampleList Data frame containing sample metadata.
#'   Defaults to the global `SampleList` object if not supplied.
#' @param OntargetPosition Numeric; mitochondrial genome position of the
#'   intended on-target edit. Defaults to the global `OntargetPosition`
#'   object if not supplied.
#' @param fill_colours Character vector of colours used for the heatmap gradient.
#' @param fill_values Numeric vector defining the percentage values corresponding
#'   to `fill_colours`. Default is `c(0, 30, 100)`.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). The heatmap is written to the `Plots` subdirectory
#'   within this directory.
#' @return A ggplot object. PNG and PDF versions of the heatmap are saved
#'   in the `Plots` subdirectory within `out_dir_base`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Default heatmap
#' Bystander(BystanderDistance = 10)
#'
#' # Custom colour scale
#' Bystander(
#'   fill_colours = c("white", "yellow", "red"),
#'   fill_values = c(0, 10, 100)
#' )
#' }
Bystander <- function(
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

# Select region of interest
AdjBy <- Adj[Adj$position>=(OntargetPosition-BystanderDistance) & Adj$position<=(OntargetPosition+BystanderDistance),] #select region of interest
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
AdjBy$SampleName <- factor(AdjBy$SampleName, levels = rev(SampleList$SampleName[order(SampleList$Order)]))

# Extract positions and corresponding letters for secondary axis
secondary_labels <- Adj$ref_base[Adj$position >= (OntargetPosition - BystanderDistance) &
                             Adj$position <= (OntargetPosition + BystanderDistance)]
positions <- Adj$position[Adj$position >= (OntargetPosition - BystanderDistance) &
                            Adj$position <= (OntargetPosition + BystanderDistance)]

# Make heatmap from this file

p <- ggplot2::ggplot(
  AdjBy,
  ggplot2::aes(
    x = position,
    y = SampleName,
    fill = AdjPercentage
  )
) +
  ggplot2::geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  ggplot2::scale_fill_gradientn(
    colours = fill_colours,
    values = scales::rescale(fill_values),
    na.value = "grey96"
  ) +
  ggplot2::scale_x_continuous(
    name = xlab,
    breaks = positions,
    sec.axis = ggplot2::dup_axis(
      name = "",
      labels = secondary_labels
    )
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      title = "Editing (%)"
    )
  ) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = OntargetPosition - 0.5,
      xmax = OntargetPosition + 0.5,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    color = "black",
    fill = NA,
    linewidth = 1
  ) +
  ggplot2::coord_fixed() +
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
    axis.text.x.top = ggplot2::element_text(
      size = 12,
      angle = 0,
      hjust = 0.5
    ),
    axis.title = ggplot2::element_text(size = 14)
  )

the_dir <- file.path(out_dir_base, "Plots")
check_create_dir(the_dir)

# Adjust figure height to the number of samples
n_samples <- length(unique(AdjBy$SampleName))
plot_height <- max(4, min(10, 2.5 + 0.35 * n_samples))

ggplot2::ggsave(
  filename = file.path(the_dir, "HeatmapBystanderEffect.png"),
  plot = p,
  width = 8,
  height = plot_height,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path(the_dir, "HeatmapBystanderEffect.pdf"),
  plot = p,
  width = 8,
  height = 6
)

invisible(p)
}
