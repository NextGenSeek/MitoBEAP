#' OnTargetCalcCBE
#'
#' Analyzes count files to calculate base editing efficiency (C→T and G→A conversions), including on- and off-target effects.
#' Saves outputs like mutation percentages, mean off-target values, on-target editing rates, coverage, and annotated full data.
#'
#' @param MinXFiles A character vector of file paths to pre-filtered count data (CSV format).
#' @keywords CBE, base editing, efficiency, on-target, off-target
#' @export
#' @examples
#' \dontrun{
#' file_paths <- c("file1.csv", "file2.csv", "file3.csv")
#' OnTargetCalcCBE(file_paths)
#' }

OnTargetCalcCBE = function(MinXFiles) {
  # Check file existence
  if (any(!file.exists(MinXFiles))) {
    stop("One or more input files not found.")
  }

  # Check OntargetPosition existence
  if (!exists("OntargetPosition")) {
    stop("Error: 'OntargetPosition' must be defined in the global environment.")
  }

  process_file <- function(input) {
    message("Processing: ", input)
    data <- read.delim(input, row.names = NULL, header = T, sep = ",")

    # Install the 'splitstackshape' package if needed: install.packages("splitstackshape")
    if (!requireNamespace("splitstackshape", quietly = TRUE)) {
      stop("Please install the 'splitstackshape' package to use the 'concat.split' function.")
    }

    # Identify columns in the range 6:10 that exist in the data
    existing_cols <- intersect(6:10, which(colnames(data) %in% colnames(data)))

    # Apply the concat.split function only to the existing columns
    if (length(existing_cols) > 0) {
      data <- concat.split(data = data, split.col = existing_cols, sep = ":", drop = TRUE)
    }
    # split concatenated columns by `:`
    #data <- concat.split(data = data, split.col = c(6:10), sep = ":", drop = TRUE)

    # Remove unnecessary columns, but make sure to keep both strands
    data <- subset(data, select = c(1:3,5:7,14:15,22:23,30:31))

    # Add columns if they don't exist
    x <- c("A8_1","A8_2","A7_1","A7_2","A6_1","A6_2")
    add_column(data, !!!x[setdiff(names(x), names(data))])

    #Change column names
    colnames(data) [c(5:12)] <- c("base.1","reads.1","base.2","reads.2","base.3","reads.3","base.4","reads.4")

    # Select all C
    data2C <- data[data$ref_base %in% c("C"),]
    # Select all G
    data2G <- data[data$ref_base %in% c("G"),]
    # Combine data2C and data2G for later
    dataCG <- rbind(data2C, data2G)

    # For data2C select the reads that have a T
    data3C.1 <- subset(data2C, base.1 %in% c("T"), select=c("chrom", "position",  "ref_base", "depth", "base.1", "reads.1"))
    data3C.2 <- subset(data2C, base.2 %in% c("T"), select=c("chrom", "position", "ref_base", "depth","base.2", "reads.2"))
    data3C.3 <- subset(data2C, base.3 %in% c("T"), select=c("chrom", "position", "ref_base", "depth","base.3", "reads.3"))
    data3C.4 <- subset(data2C, base.4 %in% c("T"), select=c("chrom", "position",  "ref_base", "depth","base.4", "reads.4"))

    #Change column names
    colnames(data3C.1) [c(5:6)] <- c("base","reads")
    colnames(data3C.2) [c(5:6)] <- c("base","reads")
    colnames(data3C.3) [c(5:6)] <- c("base","reads")
    colnames(data3C.4) [c(5:6)] <- c("base","reads")

    # For data2G select the reads that have a A
    data3G.1 <- subset(data2G, base.1 %in% c("A"), select=c("chrom", "position", "ref_base", "depth", "base.1", "reads.1"))
    data3G.2 <- subset(data2G, base.2 %in% c("A"), select=c("chrom", "position", "ref_base", "depth", "base.2", "reads.2"))
    data3G.3 <- subset(data2G, base.3 %in% c("A"), select=c("chrom", "position", "ref_base", "depth", "base.3", "reads.3"))
    data3G.4 <- subset(data2G, base.4 %in% c("A"), select=c("chrom", "position", "ref_base", "depth","base.4", "reads.4"))

    #Change column names
    colnames(data3G.1) [c(5:6)] <- c("base","reads")
    colnames(data3G.2) [c(5:6)] <- c("base","reads")
    colnames(data3G.3) [c(5:6)] <- c("base","reads")
    colnames(data3G.4) [c(5:6)] <- c("base","reads")

    # Combine these datasets into one, sort and determine the mutation frequency
    All_Off <- rbind(data3C.1,data3C.2,data3C.3,data3C.4,data3G.1,data3G.2,data3G.3,data3G.4, fill=TRUE)
    All_Off$position <- as.numeric(All_Off$position)
    All_Off <- All_Off[order(All_Off$position),]
    All_Off$depth <- as.numeric(All_Off$depth)
    All_Off$reads <- as.numeric(All_Off$reads)

    data3 <- All_Off <- transform(All_Off , percentage = (reads / depth)*100)
    DuplicateList <- All_Off$position

   # Add positions without off-targets
    dataCG <- dataCG[,c(1:4)]
    dataCG[, 'base'] = NA
    dataCG[, 'reads'] = NA
    dataCG[, 'percentage'] = NA
    test <- rbind(data3, dataCG)
    # test4 <- test[,c(1:7)]
    test2 <- test[ ! test$position %in% DuplicateList, ]
    test2$percentage[is.na(test2$percentage)] <- 0

    AllCG <- rbind(data3, test2)

     the_dir <- "./percentages" #Name the new desired directory
     check_create_dir(the_dir)

     # write as new csv
     write_csv(AllCG,file = paste0(the_dir,"/", file_path_sans_ext(basename(input)), ".csv", sep=""))
     the_dir <- "./mean" #Name the new desired directory
     check_create_dir(the_dir)

     # Calulate mean percentage without on target
     data_off <- AllCG[!(AllCG$position %in% OntargetPosition),]
     mean <- mean(data_off$percentage)

     # Save mean as separate file
     write.table(mean, file = paste0(the_dir, "/", file_path_sans_ext(basename(input)), '_mean',".txt"))

     # Extract on target percentage
     ontarget <- data3[(data3$position == OntargetPosition),]

     the_dir <- "./OnTarget" #Name the new desired directory
     check_create_dir(the_dir)

     # As the mutations can be in any column, do this for ontarget:
     if (isTRUE(ontarget$base.1 == "A") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.1 / depth)*100) # isTRUE(condition)==TRUE to avoid error if NA
     } else if (isTRUE(ontarget$base.2 == "A") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.2 / depth)*100)
     } else if (isTRUE(ontarget$base.3 == "A") ==TRUE){
       ontarget <- transform(ontarget , percentage = (reads.3 / depth)*100)
     } else if (isTRUE(ontarget$base.4 == "A") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.4 / depth)*100)
     } else if (isTRUE(ontarget$base.1 == "T") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.2 / depth)*100)
     } else if (isTRUE(ontarget$base.2 == "T") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.2 / depth)*100)
     } else if (isTRUE(ontarget$base.3 == "T") ==TRUE){
       ontarget <- transform(ontarget , percentage = (reads.3 / depth)*100)
     } else if (isTRUE(ontarget$base.4 == "T") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.4 / depth)*100)
     }   else{
         print ("NA")
       }

     # Save ontarget as separate file
     write.table(ontarget$percentage,file = paste0(the_dir, "/", file_path_sans_ext(basename(input)), '_ontarget',".txt"))

     ################# Add average coverage ################

     the_dir <- "./Coverage" #Name the new desired directory
     check_create_dir(the_dir)

     # Calculate average coverage and save as separate file
     coverage <- mean(data$depth)
     write.table(coverage,file = paste0(the_dir, "/", file_path_sans_ext(basename(input)), '_coverage',".txt"))

     ### Create files with all positions, but only off targets and no background
     idx <- match(data$position, AllCG$position)
     data$MutBase <- AllCG$base[idx]
     data$MutReads <- AllCG$reads[idx]
     data$percentage <-AllCG$percentage[idx]
     data <- subset(data, select = c(1:6,13:15))  # remove unnecessary colums
     colnames(data) [c(5,6)] <- c("RefBase","RefReads")  #Change column names

     # Save all positions file
     the_dir <- "./AllPositions"
     check_create_dir(the_dir)

     write_csv(data,file = paste0(the_dir,"/", file_path_sans_ext(basename(input)), ".csv", sep=""))

  }
  lapply(MinXFiles, process_file)
  invisible(NULL)
}
