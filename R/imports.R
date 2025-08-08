#' @importFrom dplyr %>% group_by summarise mutate filter n left_join
#' @importFrom tidyr drop_na
#' @importFrom readr read_csv write_csv
#' @importFrom utils read.csv read.delim write.csv write.table
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_bar geom_tile
#' @importFrom ggplot2 geom_histogram scale_color_manual dup_axis
#' @importFrom ggplot2 scale_fill_gradient2 scale_x_discrete scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 labs guides guide_legend guide_colorbar
#' @importFrom ggplot2 theme theme_minimal element_text element_blank element_line
#' @importFrom ggplot2 ggtitle ggsave coord_flip coord_fixed expand_limits
#' @importFrom grid unit
#' @importFrom grDevices dev.list dev.off
#' @importFrom ggrepel geom_text_repel
#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @importFrom tibble add_column
#' @importFrom dplyr case_when
#' @importFrom splitstackshape concat.split
#'
utils::globalVariables(c(
  ".", ".data", "SampleName", "FileName", "Condition",
  "offTarget_percentage", "ref_base", "reads.1", "reads.2",
  "reads.3", "reads.4", "base.1", "base.2", "base.3", "base.4",
  "percentage", "ylim"
))
