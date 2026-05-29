#' CreateBarChart
#'
#' Creates bar charts for either on-target or off-target editing percentages per sample.
#' Saves the resulting plot as a PNG in the "Plots" directory.
#'
#' @param data_type A character string: either `"OnTarget"` or `"OffTarget"`. Determines the input data source.
#' @param colour A color name or hex code for the bar fill. Default is `"skyblue"`.
#'
#' @return No return value. A bar chart PNG is saved to `./Plots/Barchart_<data_type>.png`.
#' @export
#'
#' @examples
#' \dontrun{
#' CreateBarChart(data_type = "OnTarget", colour = "skyblue")
#' CreateBarChart(data_type = "OffTarget", colour = "#FF5733")
#' }

CreateBarChart <- function(data_type = c("OnTarget", "OffTarget"),
                           colour    = "skyblue") {

  data_type <- match.arg(data_type)

  ## ------------------------------------------------------------------------
  ## 1.  Pick the source object and the *name* of the percentage column
  ## ------------------------------------------------------------------------
  if (data_type == "OnTarget") {
    if (!exists("OnTarget", envir = .GlobalEnv)) {
      stop("Object 'OnTarget' not found.")
    }
    df         <- get("All_ontarget", envir = .GlobalEnv)
    perc_col   <- "On target %"
    ylab_title <- "On target %"

  } else {                               # OffTarget
    if (!exists("All_mean", envir = .GlobalEnv)) {
      stop("Object 'All_mean' not found.")
    }
    df         <- get("All_mean", envir = .GlobalEnv)
    perc_col   <- "Off target %"
    ylab_title <- "Off target %"
  }

  ## ------------------------------------------------------------------------
  ## 2.  Basic sanity checks
  ## ------------------------------------------------------------------------
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  if (!all(c("FileName", perc_col) %in% names(df))) {
    stop("The chosen data object must contain columns 'FileName' and '",
         perc_col, "'.\nActual columns are: ",
         paste(names(df), collapse = ", "))
  }
  if (!exists("SampleList", envir = .GlobalEnv)) {
    stop("Global object 'SampleList' is required but not found.")
  }
  SampleList <- get("SampleList", envir = .GlobalEnv)

  ## ------------------------------------------------------------------------
  ## 3.  Clean up file names and coerce to numeric
  ## ------------------------------------------------------------------------
  df$FileName  <- gsub("_mean|_ontarget", "", df$FileName)
  df[[perc_col]] <- as.numeric(df[[perc_col]])

  ## Map to sample names / order
  df$SampleName <- SampleList$SampleName[match(df$FileName,
                                               SampleList$FileName)]
  df$Order      <- SampleList$Order[match(df$FileName,
                                          SampleList$FileName)]

  df$SampleName <- factor(df$SampleName,
                          levels = rev(SampleList$SampleName[
                            order(SampleList$Order)]))

  ## ------------------------------------------------------------------------
  ## 4.  Build the bar chart
  ## ------------------------------------------------------------------------
  the_dir <- "./Plots"
  if (!dir.exists(the_dir)) dir.create(the_dir, recursive = TRUE)

  p <- ggplot2::ggplot(df, aes(x = SampleName, y = .data[[perc_col]])) +
    geom_bar(stat = "identity", fill = colour, alpha = 0.7) +
    geom_text(aes(label = sprintf("%.2f", .data[[perc_col]])),
              hjust = 1.6, colour = "black", size = 6) +
    labs(x = NULL, y = ylab_title) +
    scale_y_continuous(breaks = seq(0, max(df[[perc_col]], na.rm = TRUE), 10),
                       expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    theme(axis.text.y = element_text(size = 14),
          axis.title  = element_text(size = 14),
          panel.background = element_blank(),
          axis.line  = element_line(colour = "black")) +
    coord_flip()

  print(p)

  out_png <- file.path(the_dir,
                       sprintf("Barchart_%s.png", data_type))
  ggplot2::ggsave(out_png, plot = p, width = 8, height = 6)

  invisible(p)
}
