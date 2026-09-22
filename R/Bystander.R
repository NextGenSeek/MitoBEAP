#' Bystander
#'
#' Creates a heatmap of adjusted heteroplasmy percentages for positions surrounding the on-target site.
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
#'
#' @return A ggplot object. The heatmap is also saved to
#'   `./Plots/HeatmapBystanderEffect.png`.
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
    OntargetPosition = NULL
) {

  if (is.null(Adj)) {
    if (!exists("Adj", envir = .GlobalEnv)) {
      stop("Error: 'Adj' must be supplied or exist in the global environment.")
    }
    Adj <- get("Adj", envir = .GlobalEnv)
  }

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv)
  }

  if (is.null(OntargetPosition)) {
    if (!exists("OntargetPosition", envir = .GlobalEnv)) {
      stop("Error: 'OntargetPosition' must be supplied or exist in the global environment.")
    }
    OntargetPosition <- get("OntargetPosition", envir = .GlobalEnv)
  }

# Select region of interest
AdjBy <- Adj[Adj$position>=(OntargetPosition-BystanderDistance) & Adj$position<=(OntargetPosition+BystanderDistance),] #select region of interest
AdjBy <- AdjBy[ -c(2,4:9,11)] # remove columns that are not needed
AdjBy$SampleName <- SampleList$SampleName[match(AdjBy$FileName, SampleList$FileName)]

AdjBy$Order <- as.integer(SampleList$Order[match(AdjBy$FileName, SampleList$FileName)])
AdjBy$SampleName <- factor(AdjBy$SampleName, levels = rev(SampleList$SampleName[order(SampleList$Order)]))

# Extract positions and corresponding letters for secondary axis
secondary_labels <- Adj$X3[Adj$position >= (OntargetPosition - BystanderDistance) &
                             Adj$position <= (OntargetPosition + BystanderDistance)]
positions <- Adj$position[Adj$position >= (OntargetPosition - BystanderDistance) &
                            Adj$position <= (OntargetPosition + BystanderDistance)]

# Make heatmap from this file

p <- ggplot(AdjBy, aes(x = position, y = SampleName, fill = AdjPercentage)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) +
  ggplot2::scale_fill_gradientn(
    colours = fill_colours,
    values = scales::rescale(fill_values),
    na.value = "grey96"
  ) +
  scale_x_continuous(name = xlab,
                     breaks = positions,  # Align the breaks with your positions
                     sec.axis = dup_axis(name = "",
                                         labels = secondary_labels)) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  ggplot2::geom_rect(
    aes(
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
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y    = element_text(size = 14),
        axis.text.x.top = element_text(angle = 0, hjust = 0.5)) +  # Set secondary axis labels horizontally
  ggtitle(title) +
  ggplot2::xlab(xlab) +
  ggplot2::ylab(ylab)

the_dir <- "./Plots"
check_create_dir(the_dir)

ggsave(filename = paste0(the_dir,"/","HeatmapBystanderEffect.png"),plot = p, width = 8, height = 6)

return(p)
}
