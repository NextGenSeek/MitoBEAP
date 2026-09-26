#' OnVsOffTarget
#'
#' Creates a scatterplot comparing on-target editing with off-target effects.
#'
#' @param xlab Character. Label for the x-axis. Default: "Off-target editing (\%)".
#' @param ylab Character. Label for the y-axis. Default: "On-target editing (\%)".
#' @param ggtitle Character. Plot title. Default: "On-versus off-target effects".
#' @param All_mean Data frame containing mean off-target editing percentages.
#'   Defaults to the global `All_mean` object if not supplied.
#' @param OnTarget Data frame containing on-target editing percentages.
#'   Defaults to the global `OnTarget` object if not supplied.
#' @param Coverage Data frame containing sequencing coverage information.
#'   Defaults to the global `Coverage` object if not supplied.
#' @param SampleList Data frame containing sample metadata.
#'   Defaults to the global `SampleList` object if not supplied.
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). The plot is written to the `Plots` subdirectory
#'   within this directory.
#' @return Invisibly returns the ggplot object. PNG and PDF versions of the
#'   plot are saved to the `Plots` subdirectory within `out_dir_base`.
#' @export
#' @examples
#' \dontrun{
#' OnVsOffTarget(
#'   ylab = "On-target editing (%)",
#'   ggtitle = "On-target versus off-target effects"
#' )
#' }
OnVsOffTarget <- function(
    xlab = "Off target editing (%)",
    ylab = "On-target editing (%)",
    ggtitle = "On-target versus off-target effects",
    All_mean = NULL,
    OnTarget = NULL,
    Coverage = NULL,
    SampleList = NULL,
    out_dir_base = "."
) {

  if (is.null(All_mean)) {
    if (!exists("All_mean", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'All_mean' must be supplied or exist in the global environment.")
    }
    All_mean <- get("All_mean", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(OnTarget)) {
    if (!exists("OnTarget", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'OnTarget' must be supplied or exist in the global environment.")
    }
    OnTarget <- get("OnTarget", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(Coverage)) {
    if (!exists("Coverage", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'Coverage' must be supplied or exist in the global environment.")
    }
    Coverage <- get("Coverage", envir = .GlobalEnv, inherits = FALSE)
  }

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv, inherits = FALSE)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv, inherits = FALSE)
  }

  # Clean sample names
  All_mean$FileName <- gsub("_mean", "", All_mean$FileName)
  All_mean$`Off target %` <- gsub("^1_", "", All_mean$`Off target %`)
  OnTarget$FileName <- gsub("_ontarget", "", OnTarget$FileName)
  Coverage$FileName <- gsub("_coverage", "", Coverage$FileName)

  # Match on-target editing and coverage using FileName
  idx3 <- match(All_mean$FileName, OnTarget$FileName)
  All_mean$`On target %` <- OnTarget$`On target %`[idx3]

  idx4 <- match(All_mean$FileName, Coverage$FileName)
  All_mean$Coverage <- Coverage$Coverage[idx4]

  # Add the user-facing sample name
  idx5 <- match(All_mean$FileName, SampleList$FileName)
  All_mean$RealName <- SampleList$SampleName[idx5]

  Overview_df <- as.data.frame(All_mean)
Overview_df$`Off target %` <- as.numeric(gsub(pattern = "\\s+", "", x = Overview_df$`Off target %`))
Overview_df$`On target %` <- as.numeric(Overview_df$`On target %`)

# Build plot
p <- ggplot2::ggplot(
  Overview_df,
  ggplot2::aes(
    x = `Off target %`,
    y = `On target %`,
    label = RealName
  )
) +
  ggplot2::geom_point(size = 3) +
  ggrepel::geom_text_repel(
    size = 3.5,
    box.padding = grid::unit(0.3, "lines")
  ) +
  ggplot2::labs(
    title = ggtitle,
    x = xlab,
    y = ylab
  ) +
  ggplot2::expand_limits(x = 0, y = 0) +
  ggplot2::theme_classic(base_size = 11) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 12),
    plot.title = ggplot2::element_text(size = 13)
  )

# Create output directory
the_dir <- file.path(out_dir_base, "Plots")
check_create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}
check_create_dir(the_dir)

out_png <- file.path(the_dir, "OnOffTarget.png")
out_pdf <- file.path(the_dir, "OnOffTarget.pdf")

ggplot2::ggsave(
  out_png,
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)

ggplot2::ggsave(
  out_pdf,
  plot = p,
  width = 7,
  height = 5
)
invisible(p)
}
