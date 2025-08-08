#' check_create_dir
#'
#' This function allows you create and name a subdirectory
#' @param the_dir directory. Default ./subfolder
#' @keywords create (sub)directory
#' @export
#' @examples
#' check_create_dir("./name_subdir")

check_create_dir <- function(the_dir = "./subfolder") {
  if (!dir.exists(the_dir)) {
    dir.create(the_dir, recursive = TRUE) } #Creates a directory if it doesn't already exist
}
