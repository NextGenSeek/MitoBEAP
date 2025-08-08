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

  required_objects <- c("Mean", "OnTarget", "Coverage", "SampleList")
  missing <- required_objects[!sapply(required_objects, exists, envir = .GlobalEnv)]
  if (length(missing) > 0) {
    stop("Error: The following required objects are missing from the global environment: ", paste(missing, collapse = ", "))
  }

  Mean <- get("Mean", envir = .GlobalEnv)
  OnTarget <- get("OnTarget", envir = .GlobalEnv)
  Coverage <- get("Coverage", envir = .GlobalEnv)
  SampleList <- get("SampleList", envir = .GlobalEnv)

# Clean sample names
Mean$Sample <- gsub("_mean", "", Mean$Sample)
Mean$`Off target %` <- gsub("1 ", "", Mean$`Off target %`)
OnTarget$Sample <- gsub("_ontarget", "", OnTarget$Sample)
Coverage$Sample <- gsub("_coverage", "", Coverage$Sample)

idx3 <- match(Mean$Sample, OnTarget$Sample)
Mean$`On target %` <- OnTarget$`On target %`[idx3]

idx4 <- match(Mean$Sample, Coverage$Sample)
Mean$Coverage <- Coverage$Coverage [idx4]

idx5 <- match(Mean$Sample, SampleList$Sample)
Mean$RealName <- SampleList$Name [idx5]
#Mean$Order <- SampleList$Order [idx5]

Overview_df <- as.data.frame(Mean)
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
  xlab(xlab) +
  ylab(ylab)

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
