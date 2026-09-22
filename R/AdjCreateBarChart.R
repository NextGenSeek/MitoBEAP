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
#' @return No return value. A bar chart PNG is saved to `./Plots/AdjustedPlots/Adjusted_Barchart_offTarget.png`.
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
    SampleList = NULL
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

the_dir <- "./Plots" #Name the new desired directory
check_create_dir(the_dir)

# Make a simple bar chart

data$Order <- as.integer(SampleList$Order[match(data$SampleName, SampleList$SampleName)])

# Reorder SampleName factor levels alphabetically
data$SampleName <- factor(data$SampleName, levels = rev(SampleList$SampleName[order(SampleList$Order)]))

p<- ggplot2::ggplot(data, aes(x=SampleName, y=offTarget_percentage)) +
  geom_bar(stat='identity', fill=colour, alpha = 0.7) +
  geom_text(aes(label = sprintf("%.2f", offTarget_percentage)),
            hjust = 1.6, color = "black", size = 4) +
  labs(x = NULL, y = "Off target %") +
  scale_y_continuous(breaks = seq(0, max(data$offTarget_percentage), by = 10), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme(axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")
  ) +
  coord_flip()

plot(p)
# Save plot
outname_png = paste0(the_dir,"/AdjustedPlots/","Adjusted_Barchart_offTarget.png")
ggsave(outname_png, plot = p, width = 8, height = 6)
invisible(dev.off())

}
