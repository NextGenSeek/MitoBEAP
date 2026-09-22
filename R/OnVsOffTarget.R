#' OnVsOffTarget
#'
#' Creates a scatterplot comparing on-target editing with off-target effects.
#'
#' @param xlab Character. Label for the x-axis. Default: "Off target effects (\%)".
#' @param ylab Character. Label for the y-axis. Default: "Heteroplasmy level (\%)".
#' @param ggtitle Character. Plot title. Default: "On-versus off-target effects".
#' @param All_mean Data frame containing mean off-target editing percentages.
#'   Defaults to the global `All_mean` object if not supplied.
#' @param OnTarget Data frame containing on-target editing percentages.
#'   Defaults to the global `OnTarget` object if not supplied.
#' @param Coverage Data frame containing sequencing coverage information.
#'   Defaults to the global `Coverage` object if not supplied.
#' @param SampleList Data frame containing sample metadata.
#'   Defaults to the global `SampleList` object if not supplied.
#' @return Invisibly returns the ggplot object. A PNG plot is saved to `./Plots/`.
#' @export
#' @examples
#' \dontrun{
#' OnVsOffTarget(ylab = "Heteroplasmy level (%)", ggtitle = "On vs Off")
#' }
OnVsOffTarget <- function(
    xlab = "Off target effects (%)",
    ylab = "Heteroplasmy level (%)",
    ggtitle = "On-versus off-target effects",
    All_mean = NULL,
    OnTarget = NULL,
    Coverage = NULL,
    SampleList = NULL
) {

  if (is.null(All_mean)) {
    if (!exists("All_mean", envir = .GlobalEnv)) {
      stop("Error: 'All_mean' must be supplied or exist in the global environment.")
    }
    All_mean <- get("All_mean", envir = .GlobalEnv)
  }

  if (is.null(OnTarget)) {
    if (!exists("OnTarget", envir = .GlobalEnv)) {
      stop("Error: 'OnTarget' must be supplied or exist in the global environment.")
    }
    OnTarget <- get("OnTarget", envir = .GlobalEnv)
  }

  if (is.null(Coverage)) {
    if (!exists("Coverage", envir = .GlobalEnv)) {
      stop("Error: 'Coverage' must be supplied or exist in the global environment.")
    }
    Coverage <- get("Coverage", envir = .GlobalEnv)
  }

  if (is.null(SampleList)) {
    if (!exists("SampleList", envir = .GlobalEnv)) {
      stop("Error: 'SampleList' must be supplied or exist in the global environment.")
    }
    SampleList <- get("SampleList", envir = .GlobalEnv)
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
p <- ggplot(Overview_df,aes(x=`Off target %`, y=`On target %`, label = RealName)) +
  geom_point(size = 4) +
  ggtitle(ggtitle) +
  geom_text_repel(size = 4, box.padding = unit(0.3, "lines")) +
  theme(axis.text = element_text(size=14)) +
  theme(axis.title = element_text(size = 20)) +
  expand_limits(x=0,y=0) +
  ggplot2::xlab(xlab) +
  ggplot2::ylab(ylab)

# Create output directory
the_dir <- "./Plots"
check_create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}
check_create_dir(the_dir)

ggplot2::ggsave(file = file.path(the_dir, "OnOffTarget.png"), plot = p, width = 8, height = 6)
invisible(p)
}
