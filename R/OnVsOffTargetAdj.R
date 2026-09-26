#' OnVsOffTargetAdj
#'
#' Creates a scatterplot comparing on-target editing with adjusted off-target effects.
#'
#' @param xlab Character. Label for the x-axis. Default: "Adjusted off-target editing (\%)".
#' @param ylab Character. Label for the y-axis. Default: "On-target editing (\%)".
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
#'   xlab = "Adjusted off-target editing (%)",
#'   ylab = "On-target editing (%)",
#'   ggtitle = "Comparison"
#' )
#' }
OnVsOffTargetAdj <- function(
    xlab = "Off target editing (%)",
    ylab = "On target editing (%)",
    ggtitle = "On-target versus off-target effects",
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

  # Dynamic label size based on the longest sample name
  max_label_length <- max(nchar(Overview_adj$SampleName), na.rm = TRUE)

  label_size <- dplyr::case_when(
    max_label_length <= 10 ~ 3.5,
    max_label_length <= 15 ~ 3.2,
    max_label_length <= 20 ~ 3.0,
    TRUE                   ~ 2.8
  )
  # Create scatterplot
  p <- ggplot2::ggplot(
    Overview_adj,
    ggplot2::aes(
      x = Adj_percentages,
      y = `On target %`,
      colour = label_column
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = SampleName),
      size = label_size,
      box.padding = grid::unit(0.3, "lines")
    ) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::labs(
      title = ggtitle,
      x = xlab,
      y = ylab,
      colour = if (condition) "Condition" else NULL
    ) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 13),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      legend.position = if (condition) "right" else "none"
    )
  if (condition) {
    p <- p +
      ggplot2::scale_color_manual(values = color_palette)
  }

  # Save plot
  the_dir <- file.path(out_dir_base, "Plots", "AdjustedPlots")
  check_create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  check_create_dir(the_dir)

  ggplot2::ggsave(
    file.path(the_dir, "OnOffTarget_Adj.png"),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )

  ggplot2::ggsave(
    file.path(the_dir, "OnOffTarget_Adj.pdf"),
    plot = p,
    width = 7,
    height = 5
  )

  invisible(p)
}
