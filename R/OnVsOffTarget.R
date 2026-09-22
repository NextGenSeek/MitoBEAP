#' OnVsOffTarget
#'
#' Creates a scatterplot comparing on-target editing with off-target effects.
#'
#' @param xlab Character. Label for the x-axis. Default: "Off target effects (\%)".
#' @param ylab Character. Label for the y-axis. Default: "Heteroplasmy level (\%)".
#' @param ggtitle Character. Plot title. Default: "On-versus off-target effects".
#' @return No return value. A PNG plot is saved to `./Plots/`.
#' @export
#' @examples
#' \dontrun{
#' OnVsOffTarget(ylab = "Heteroplasmy level (%)", ggtitle = "On vs Off")
#' }
OnVsOffTarget = function(xlab = "Off target effects (%)",
                         ylab = "Heteroplasmy level (%)",
                         ggtitle = "On-versus off-target effects") {

  required_objects <- c("All_mean", "OnTarget", "Coverage", "SampleList")
  missing <- required_objects[!sapply(required_objects, exists, envir = .GlobalEnv)]
  if (length(missing) > 0) {
    stop(
      "Error: The following required objects are missing from the global environment: ",
      paste(missing, collapse = ", ")
    )
  }

  All_mean <- get("All_mean", envir = .GlobalEnv)
  OnTarget <- get("OnTarget", envir = .GlobalEnv)
  Coverage <- get("Coverage", envir = .GlobalEnv)
  SampleList <- get("SampleList", envir = .GlobalEnv)

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
}
