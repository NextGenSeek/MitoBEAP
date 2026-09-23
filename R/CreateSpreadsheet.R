#' CreateSpreadsheet
#'
#' Combines all CSV files from the "AllPositions" folder into a multi-sheet Excel workbook.
#' Each sample gets its own worksheet. The final Excel file is saved as "All_data.xlsx" in the "Overview" directory.
#'
#' @param out_dir_base Base analysis directory. Defaults to the current working
#'   directory (`"."`). Input files are read from the `AllPositions`
#'   subdirectory and the Excel workbook is written to the `Overview`
#'   subdirectory.
#' @return No return value. Saves `All_data.xlsx` to the `Overview`
#'   subdirectory within `out_dir_base`.
#' @keywords excel data aggregation
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure the "./AllPositions" directory contains CSV files before running
#' CreateSpreadsheet()
#' }
CreateSpreadsheet <- function(out_dir_base = ".") {
  # Collect all report CSV files
  all_positions_dir <- file.path(out_dir_base, "AllPositions")

  ReportFiles <- list.files(
    all_positions_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  if (length(ReportFiles) == 0) {
    stop("Error: No CSV files found in '", all_positions_dir, "'.")
  }

# creating work book
suppressWarnings(rm(wb)) # remove old workbook if present
suppressWarnings(rm(sheet)) # remove old sheets if present
wb <- openxlsx::createWorkbook()

# going through each csv file
for (item in ReportFiles)
{
  # create a sheet in the workbook
  sheet <- openxlsx::addWorksheet(wb, sheetName=strsplit(item, "\\/|[.]")[[1]][4])

  # add the data to the new sheet
  openxlsx::writeData(wb, x= read.csv(item), sheet,rowNames=FALSE)
}

# Ensure output directory exists
the_dir <- file.path(out_dir_base, "Overview")
check_create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}
check_create_dir(the_dir)

# saving the workbook
openxlsx::saveWorkbook(
  wb,
  file = file.path(the_dir, "All_data.xlsx"),
  overwrite = TRUE
)
}
