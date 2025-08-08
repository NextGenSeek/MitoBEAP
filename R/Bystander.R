#' Bystander
#'
#' Creates a heatmap of adjusted heteroplasmy percentages for positions surrounding the on-target site.
#' Highlights potential bystander effects within a user-defined window.
#'
#' @param BystanderDistance Integer. Number of positions upstream/downstream from the on-target site to include.
#' @param title Character. Title of the heatmap plot. Default is "Bystander effect".
#' @param xlab Character. Label for the x-axis. Default is "mtDNA position".
#' @param ylab Character. Label for the y-axis. Default is "" (blank).
#'
#' @return No return value. Saves a heatmap plot to `./Plots/HeatmapBystanderEffect.png`.
#' @export
#'
#' @examples
#' \dontrun{
#' Bystander(BystanderDistance = 10)
#' }
Bystander = function(BystanderDistance = 10,
                     title = "Bystander effect",
                     xlab = "mtDNA position",
                     ylab = " ") {

  required <- c("Adj", "SampleList", "OntargetPosition")
  missing <- required[!sapply(required, exists, envir = .GlobalEnv)]
  if (length(missing) > 0) {
    stop("Error: Missing required global objects: ", paste(missing, collapse = ", "))
  }

  Adj <- get("Adj", envir = .GlobalEnv)
  SampleList <- get("SampleList", envir = .GlobalEnv)
  OntargetPosition <- get("OntargetPosition", envir = .GlobalEnv)

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

ggplot(AdjBy, aes(x = position, y = SampleName, fill = AdjPercentage)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) +
  scale_fill_gradient2(high = "darkblue", mid = "lightblue", low = "white",
                       midpoint = 30, na.value = "grey96") +
  scale_x_continuous(name = xlab,
                     breaks = positions,  # Align the breaks with your positions
                     sec.axis = dup_axis(name = "",
                                         labels = secondary_labels)) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y    = element_text(size = 14),
        axis.text.x.top = element_text(angle = 0, hjust = 0.5)) +  # Set secondary axis labels horizontally
  ggtitle(title) +
  xlab(xlab) +
  ylab(ylab)

the_dir <- "./Plots"
check_create_dir(the_dir)

ggsave(filename = paste0(the_dir,"/","HeatmapBystanderEffect.png"),width = 8, height = 6)
}
