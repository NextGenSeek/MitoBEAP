#' OnVsOffTargetAdj
#'
#' Creates a scatterplot comparing on-target editing with adjusted off-target effects.
#'
#' @param xlab Character. Label for the x-axis. Default: "Off target effects (\%)".
#' @param ylab Character. Label for the y-axis. Default: "Heteroplasmy level (\%)".
#' @param ggtitle Character. Plot title. Default: "On-versus off-target effects".
#' @param condition Logical. If TRUE, points are grouped and colored by condition. Default is TRUE.
#' @param Adj Data frame containing adjusted off-target editing data.
#'   Defaults to the global `Adj` object if not supplied.
#' @param All_ontarget Data frame containing on-target editing percentages.
#'   Defaults to the global `All_ontarget` object if not supplied.
#' @param SampleList Data frame containing sample metadata.
#'   Defaults to the global `SampleList` object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). PNG and PDF plots are written to the
#'   `Plots/AdjustedPlots` subdirectory within this directory.
#'
#' @return Invisibly returns the ggplot object. PNG and PDF plots are saved
#'   to the `Plots/AdjustedPlots` subdirectory within `out_dir_base`.
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
OnVsOffTargetAdj <- function(
    xlab = "Off target effects (%)",
    ylab = "Heteroplasmy level (%)",
    ggtitle = "On-versus off-target effects",
    condition = TRUE,
    Adj = NULL,
    All_ontarget = NULL,
    SampleList = NULL,
    out_dir_base = "."
) {
  if (is.null(Adj)) {
    if (!exists("Adj", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'Adj' must be supplied or exist in the global environment.")
    }
    Adj <- get("Adj", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(All_ontarget)) {
    if (!exists("All_ontarget", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'All_ontarget' must be supplied or exist in the global environment.")
    }
    All_ontarget <- get("All_ontarget", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv, inherits = FALSE)
  }

  OnTarget <- All_ontarget

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
# Determine global label size based on longest SampleName
max_label_length <- max(nchar(Overview_adj$SampleName), na.rm = TRUE)

label_size <- dplyr::case_when(
  max_label_length <= 10 ~ 6,
  max_label_length <= 15 ~ 5,
  max_label_length <= 20 ~ 4,
  TRUE                  ~ 3
)
  # Create scatterplot
  p <- ggplot(Overview_adj, aes(x = `Adj_percentages`, y = `On target %`, colour = label_column)) +
    geom_point(size = 3, aes(color = label_column)) +
    ggtitle(ggtitle) +
    geom_text_repel(
    label = Overview_adj$SampleName, size = label_size,aes(color = label_column),box.padding = unit(0.3, "lines")) +
    scale_color_manual(values = color_palette) +
    theme(legend.position = "right") +
    theme(axis.text = element_text(size = 12)) +
    theme(axis.title = element_text(size = 16)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(color = "black")) +
    expand_limits(x = 0, y = 0) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_size_identity() +
    if (condition) guides(color = guide_legend(title = "Condition")) else guides(color = "none")  # Show legend only when condition is TRUE

  print(p)

  # Save plot
  the_dir <- file.path(out_dir_base, "Plots", "AdjustedPlots")
  check_create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  check_create_dir(the_dir)

  ggplot2::ggsave(file = file.path(the_dir, "OnOffTarget_Adj.png"), plot = p, width = 8, height = 6)
  ggplot2::ggsave(filename = file.path(the_dir, "OnOffTarget_Adj.pdf"), plot = p, width = 8, height = 6)

  invisible(p)
}
