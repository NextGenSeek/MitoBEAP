#' OnVsOffTargetAdj
#'
#' Creates a scatterplot comparing on-target editing with adjusted off-target effects.
#'
#' @param xlab Character. Label for the x-axis. Default: "Off target effects (\%)".
#' @param ylab Character. Label for the y-axis. Default: "Heteroplasmy level (\%)".
#' @param ggtitle Character. Plot title. Default: "On-versus off-target effects".
#' @param condition Logical. If TRUE, points are grouped and colored by condition. Default is TRUE.
#'
#' @return No return value. A PNG plot is saved to `./Plots/`.
#' @export
#'
#' @examples
#' \dontrun{
#' OnVsOffTargetAdj(
#'   xlab = "Off target effects (%)",
#'   ylab = "Heteroplasmy level (%)",
#'   ggtitle = "Comparison"
#' )
#' }
OnVsOffTargetAdj = function(xlab = "Off target effects (%)",
                           ylab = "Heteroplasmy level (%)",
                           ggtitle = "On-versus off-target effects",
                           condition = TRUE) {
  # Check required global objects
  required <- c("Adj", "All_ontarget", "SampleList")
  missing <- required[!sapply(required, exists, envir = .GlobalEnv)]
  if (length(missing) > 0) {
    stop("Error: The following required objects are missing from the global environment: ", paste(missing, collapse = ", "))
  }

  Adj <- get("Adj", envir = .GlobalEnv)
  OnTarget <- get("All_ontarget", envir = .GlobalEnv)
  SampleList <- get("SampleList", envir = .GlobalEnv)

  # Calculate the mean of "percentages" for each sample separately
  Adj_percentages <- Adj %>%
    group_by(SampleName) %>%
    summarise(
      Adj_percentages = mean(AdjPercentage, na.rm = TRUE)
    )
  data <- Adj_percentages %>% drop_na()

  idx2 <- match(data$SampleName, SampleList$SampleName)
  data$FileName <- SampleList$FileName [idx2]
  data$Condition <- SampleList$Condition [idx2]

  Overview_adj <- as.data.frame(left_join(data, OnTarget))
  Overview_adj$'On target %' <- as.numeric(Overview_adj$'On target %')

  # User-specified condition
  #condition <- TRUE  # Set this to TRUE or FALSE as desired

  # Determine label column based on user-specified condition
  label_column <- if (condition) {
    Overview_adj$Condition
  } else {
    Overview_adj$SampleName
  }

  # Set color palette
  if (condition) {
    num_groups <- length(unique(Overview_adj$Condition))
    if (num_groups > RColorBrewer::brewer.pal.info["Dark2", "maxcolors"]) {
      stop("Too many conditions to display with 'Dark2' palette. Reduce groups or use a different palette.")
    }
    color_palette <- RColorBrewer::brewer.pal(num_groups, "Dark2")
  } else {
    color_palette <- NULL
  }

# Dynamic font size based on SampleName length
Overview_adj$label_size <- scales::rescale(
  nchar(Overview_adj$SampleName),
  to = c(6, 3),                    # max → min text size
  from = range(nchar(Overview_adj$SampleName))
)
  
  # Create scatterplot
  p <- ggplot(Overview_adj, aes(x = `Adj_percentages`, y = `On target %`, colour = label_column)) +
    geom_point(size = 3, aes(color = label_column)) +
    ggtitle(ggtitle) +
    geom_text_repel(aes(label = SampleName,color = label_column,size = label_size), box.padding = unit(0.3, "lines"), show.legend = FALSE) +
    scale_color_manual(values = color_palette) +
    theme(legend.position = "right") +
    theme(axis.text = element_text(size = 12)) +
    theme(axis.title = element_text(size = 16)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(color = "black")) +
    expand_limits(x = 0, y = 0) +
    xlab(xlab) +
    ylab(ylab) +
    scale_size_identity() +
    if (condition) guides(color = guide_legend(title = "Condition")) else guides(color = "none")  # Show legend only when condition is TRUE

  print(p)

  # Save plot
  the_dir <- "./Plots/AdjustedPlots"
  check_create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  check_create_dir(the_dir)

  ggplot2::ggsave(file = file.path(the_dir, "OnOffTarget_Adj.png"), plot = p, width = 8, height = 6)
}
