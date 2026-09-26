#' AdjCreateBarChart
#'
#' Creates a bar chart of adjusted off-target percentages per sample
#'
#' @param colour Color used to fill the bars. Default is `"skyblue"`.
#' @param Adj Data frame containing adjusted editing percentages, including
#'   `SampleName` and `AdjPercentage`. Defaults to the global `Adj` object
#'   if not supplied.
#' @param SampleList Data frame containing sample metadata, including
#'   `SampleName` and `Order`. Defaults to the global `SampleList` object
#'   if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). The plot is written to the `Plots/AdjustedPlots`
#'   subdirectory within this directory.
#' @return Invisibly returns the ggplot object. PNG and PDF versions of the
#'   bar chart are saved to the `Plots/AdjustedPlots` subdirectory within
#'   `out_dir_base`.
#' @export
#' @name AdjCreateBarChart
#'
#' @examples
#' \dontrun{
#' AdjCreateBarChart(
#'   colour = "skyblue",
#'   Adj = Adj,
#'   SampleList = SampleList
#' )
#' }
utils::globalVariables(c(
  "Adj", "SampleList", "SampleName", "AdjPercentage", "OntargetPosition",
  "position", "legend", "FileName", "CombinedName", "Condition", "count_non_missing",
  "avg_count", "sd_count", "make_plot", "base", "reads", "depth", "type",
  "Order", "Off target %", "On target %", "RealName"
))

AdjCreateBarChart <- function(
    colour = "skyblue",
    Adj = NULL,
    SampleList = NULL,
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

# Calculate the mean of "percentages" for each sample separately
  offTarget_percentages <- Adj %>%
    group_by(SampleName) %>%
    summarise(
      offTarget_percentage = mean(AdjPercentage, na.rm = TRUE)
    )
data <- offTarget_percentages %>% drop_na()

the_dir <- file.path(out_dir_base, "Plots", "AdjustedPlots") #Name the new desired directory
check_create_dir(the_dir)

# Make a simple bar chart

data$Order <- as.integer(SampleList$Order[match(data$SampleName, SampleList$SampleName)])

# Reorder SampleName factor levels alphabetically
data$SampleName <- factor(data$SampleName, levels = rev(SampleList$SampleName[order(SampleList$Order)]))

p <- ggplot2::ggplot(
  data,
  ggplot2::aes(x = SampleName, y = offTarget_percentage)
) +
  ggplot2::geom_bar(
    stat = "identity",
    fill = colour,
    alpha = 0.7
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", offTarget_percentage)),
    hjust = 1.6,
    colour = "black",
    size = 4.5
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Adjusted off-target editing (%)"
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(
      0,
      max(data$offTarget_percentage, na.rm = TRUE),
      by = 10
    ),
    expand = c(0, 0)
  ) +
  ggplot2::scale_x_discrete(expand = c(0, 0)) +
  ggplot2::theme_classic(base_size = 13) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 13),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14)
  ) +
  ggplot2::coord_flip()

# Save plot
outname_png <- file.path(
  the_dir,
  "Adjusted_Barchart_offTarget.png"
)

outname_pdf <- file.path(
  the_dir,
  "Adjusted_Barchart_offTarget.pdf"
)

ggplot2::ggsave(
  outname_png,
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  outname_pdf,
  plot = p,
  width = 8,
  height = 6
)

invisible(p)

}
