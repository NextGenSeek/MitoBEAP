#' AdjScatter
#'
#' Creates scatterplots showing adjusted heteroplasmy levels for each sample,
#' highlighting and labeling positions above a user-defined threshold.
#'
#' @param min_threshold Numeric; minimum adjusted heteroplasmy percentage to display (default = 0).
#' @param labelPercentage Numeric; threshold above which positions are labeled on the plot (default = 10).
#'
#' @keywords plot, heteroplasmy, scatter, labeling
#' @export
#'
#' @examples
#' \dontrun{
#' AdjScatter(min_threshold = 0, labelPercentage = 10)
#' }

AdjScatter = function(min_threshold = 0, labelPercentage = 10) {

  # Ensure required object exists
  if (!exists("Adj")) {
    stop("Error: 'Adj' data frame must be created first (e.g., via DdCBE_df()).")
  }
  if (!exists("OntargetPosition")) {
    stop("Error: 'OntargetPosition' must be defined in the global environment.")
  }

  # Filter and clean data
  data <- Adj[Adj$AdjPercentage >= min_threshold, ]
  data <- tidyr::drop_na(data)

  if (nrow(data) == 0) {
    stop("No data points meet the minimum threshold for plotting.")
  }

  assign("df10", data, envir = .GlobalEnv)

sample_names <- unique(data$SampleName)

for (sample_name in sample_names) {
  sample_data <- subset(data, SampleName == sample_name)
  sample_data$legend <- ifelse(sample_data$position == OntargetPosition, "On target", "Off target / background")

  # Create output directory if it doesn't exist
  the_dir <- "./Plots/AdjustedPlots"
  check_create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  check_create_dir(the_dir)

  # Make a simple scatterplot for each file
ggplot2::ggplot(sample_data, aes(x=position, y=AdjPercentage, color=legend)) +
    geom_point(alpha = 0.3) +
    geom_text_repel(data= subset(sample_data, AdjPercentage > labelPercentage), aes(label = position), vjust = -0.5, size = 3, show.legend = F) +  # Add labels for dots above specified %
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  labs(y="heteroplasmy level", x="Position") +
    theme(axis.text=element_text(size=12)) +
    ylim(0,100) +
    scale_color_manual(values = c("Off target / background" = "black", "On target" = "red")) +
    ggtitle(paste(sample_name)) # Title with sample name

  # Save plot
  outname_png = paste0(the_dir,"/" ,sample_name,'.png')
  ggsave(outname_png, width = 10, height = 4)

  # Ensure graphics devices are closed
  while (!is.null(dev.list())) dev.off()

  }

}
