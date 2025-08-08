#' CreateSpreadsheet
#'
#' Combines all CSV files from the "AllPositions" folder into a multi-sheet Excel workbook.
#' Each sample gets its own worksheet. The final Excel file is saved as "All_data.xlsx" in the "Overview" directory.
#'
#' @return No return value. Saves an Excel workbook to the "Overview" directory.
#' @keywords excel data aggregation
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure the "./AllPositions" directory contains CSV files before running
#' CreateSpreadsheet()
#' }
CreateSpreadsheet = function() {
  # Collect all report CSV files
  ReportFiles <- list.files("./AllPositions", pattern = "\\.csv$", full.names = TRUE)
  if (length(ReportFiles) == 0) {
    stop("Error: No CSV files found in './AllPositions'.")
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
the_dir <- "./Overview"
check_create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}
check_create_dir(the_dir)

# saving the workbook
openxlsx::saveWorkbook(wb,
                       file = paste0(the_dir,"/","All_data.xlsx"),
                       overwrite = TRUE)
}
