#' bystander_only_relevant
#'
#' Creates a heatmap of adjusted heteroplasmy percentages for potential target sites surrounding the on-target site.
#' Highlights potential bystander effects within a user-defined window.
#'
#' @param BystanderDistance Integer. Number of positions upstream/downstream from the on-target site to include.
#' @param title Character. Title of the heatmap plot. Default is "Bystander effect".
#' @param xlab Character. Label for the x-axis. Default is "mtDNA position".
#' @param ylab Character. Label for the y-axis. Default is "" (blank).
#'
#' @return No return value. Saves a heatmap plot to `./Plots/HeatmapBystanderEffect_only_relevant.png`.
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
    fill_values  = c(0, 30, 100)
) {
  required <- c("Adj", "SampleList", "OntargetPosition")
  missing <- required[!sapply(required, exists, envir = .GlobalEnv)]

  if (length(missing) > 0) {
    stop("Error: Missing required global objects: ", paste(missing, collapse = ", "))
  }

  Adj <- get("Adj", envir = .GlobalEnv)
  SampleList <- get("SampleList", envir = .GlobalEnv)
  OntargetPosition <- get("OntargetPosition", envir = .GlobalEnv)

  Adj$position <- as.numeric(as.character(Adj$position))

  AdjBy <- Adj[
    Adj$position >= (OntargetPosition - BystanderDistance) &
      Adj$position <= (OntargetPosition + BystanderDistance),
  ]

  AdjBy <- AdjBy[-c(2, 4:9, 11)]

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
    c("position", "X3")
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
    ggplot2::aes(x = position_f, y = SampleName, fill = AdjPercentage)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +

    # thinner black rectangle
    {
      if (!is.null(rect_df)) {
        ggplot2::geom_rect(
          data = rect_df,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
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
    ggplot2::guides(fill = ggplot2::guide_colorbar(title = "Percentage (%)")) +

    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 14),

      # ✅ ADD X-AXIS LINE
      axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.4),
      axis.ticks.x = ggplot2::element_line(color = "black"),

      # keep clean look
      panel.grid = ggplot2::element_blank()
    ) +

    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  print(p)

  the_dir <- "./Plots"
  check_create_dir(the_dir)

  ggplot2::ggsave(
    filename = paste0(the_dir, "/HeatmapBystanderEffect_only_relevant.png"),
    plot = p,
    width = 8,
    height = 6
  )
}
