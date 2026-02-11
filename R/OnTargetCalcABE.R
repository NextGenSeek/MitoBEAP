#' OnTargetCalcABE
#'
#' This function allows you to perform all necessary calculations for adenine base editing on and off target effects
#' @param MinXFiles A character vector of file paths
#' @keywords Load counts and filter for minimum depth (default = 30)
#' @export
#' @examples
#' \dontrun{
#' # Usage example
#' file_paths <- c("file1.csv", "file2.csv", "file3.csv")
#' OnTargetCalcAB(file_paths)
#' }

OnTargetCalcABE = function(MinXFiles) {
  if (any(!file.exists(MinXFiles))) {
    stop("One or more input files not found.")
  }

  process_file <- function(input) {
    message("Processing: ", input)
    data <- read.delim(input, row.names = NULL, header = T, sep = ",")

    # Install the 'splitstackshape' package if needed: install.packages("splitstackshape")
    if (!requireNamespace("splitstackshape", quietly = TRUE)) {
      stop("Please install the 'splitstackshape' package to use the 'concat.split' function.")
    }

    # split concatenated columns by `:`
    data <- concat.split(data = data, split.col = c(6:10), sep = ":", drop = TRUE)

    # Remove unnecessary columns, but make sure to keep both strands
    data <- subset(data, select = c(1:4,6:7,14:15,22:23,30:31))

    # Add columns if they don't exist
    x <- c("A8_1","A8_2","A7_1","A7_2","A6_1","A6_2")
    add_column(data, !!!x[setdiff(names(x), names(data))])

    #Change column names
    colnames(data) [c(5:12)] <- c("base.1","reads.1","base.2","reads.2","base.3","reads.3","base.4","reads.4")

    # Select all A
    data2A <- data[data$ref_base %in% c("A"),]
    # Select all T
    data2T <- data[data$ref_base %in% c("T"),]
    # Combine data2A and data2T for later
    dataAT <- rbind(data2A, data2T)

    # For data2A select the reads that have a G
    data3A.1 <- subset(data2A, base.1 %in% c("G"), select=c("chrom", "position",  "ref_base", "depth", "base.1", "reads.1"))
    data3A.2 <- subset(data2A, base.2 %in% c("G"), select=c("chrom", "position", "ref_base", "depth","base.2", "reads.2"))
    data3A.3 <- subset(data2A, base.3 %in% c("G"), select=c("chrom", "position", "ref_base", "depth","base.3", "reads.3"))
    data3A.4 <- subset(data2A, base.4 %in% c("G"), select=c("chrom", "position",  "ref_base", "depth","base.4", "reads.4"))

    #Change column names
    colnames(data3A.1) [c(5:6)] <- c("base","reads")
    colnames(data3A.2) [c(5:6)] <- c("base","reads")
    colnames(data3A.3) [c(5:6)] <- c("base","reads")
    colnames(data3A.4) [c(5:6)] <- c("base","reads")

    # For data2T select the reads that have a C
    data3T.1 <- subset(data2T, base.1 %in% c("C"), select=c("chrom", "position", "ref_base", "depth", "base.1", "reads.1"))
    data3T.2 <- subset(data2T, base.2 %in% c("C"), select=c("chrom", "position", "ref_base", "depth", "base.2", "reads.2"))
    data3T.3 <- subset(data2T, base.3 %in% c("C"), select=c("chrom", "position", "ref_base", "depth", "base.3", "reads.3"))
    data3T.4 <- subset(data2T, base.4 %in% c("C"), select=c("chrom", "position", "ref_base", "depth","base.4", "reads.4"))

    #Change column names
    colnames(data3T.1) [c(5:6)] <- c("base","reads")
    colnames(data3T.2) [c(5:6)] <- c("base","reads")
    colnames(data3T.3) [c(5:6)] <- c("base","reads")
    colnames(data3T.4) [c(5:6)] <- c("base","reads")

    # Combine these datasets into one, sort and determine the mutation frequency
    All_Off <- rbind(data3A.1,data3A.2,data3A.3,data3A.4,data3T.1,data3T.2,data3T.3,data3T.4, fill=TRUE)
    All_Off$position <- as.numeric(All_Off$position)
    All_Off <- All_Off[order(All_Off$position),]
    All_Off$depth <- as.numeric(All_Off$depth)
    All_Off$reads <- as.numeric(All_Off$reads)

    data3 <- All_Off <- transform(All_Off , percentage = (reads / depth)*100)
    DuplicateList <- All_Off$position

   # Add positions without off-targets
    dataAT <- dataAT[,c(1:4)]
    dataAT[, 'base'] = NA
    dataAT[, 'reads'] = NA
    dataAT[, 'percentage'] = NA
    test <- rbind(data3, dataAT)
    # test4 <- test[,c(1:7)]
    test2 <- test[ ! test$position %in% DuplicateList, ]
    test2$percentage[is.na(test2$percentage)] <- 0

    AllAT <- rbind(data3, test2)

     the_dir <- "./percentages" #Name the new desired directory
     check_create_dir(the_dir)

     # write as new csv
     write_csv(AllAT,file = paste0(the_dir,"/", file_path_sans_ext(basename(input)), ".csv", sep=""))
     the_dir <- "./mean" #Name the new desired directory
     check_create_dir(the_dir)

     # Calulate mean percentage without on target
     data_off <- AllAT[!(AllAT$position %in% OntargetPosition),]
     mean <- mean(data_off$percentage)

     # Save mean as separate file
     write.table(mean, file = paste0(the_dir, "/", file_path_sans_ext(basename(input)), '_mean',".txt"))

     # Extract on target percentage
     ontarget <- data3[(data3$position == OntargetPosition),]

     the_dir <- "./OnTarget" #Name the new desired directory
     check_create_dir(the_dir)

     # As the mutations can be in any column, do this for ontarget:
     if (isTRUE(ontarget$base.1 == "G") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.1 / depth)*100) # isTRUE(condition)==TRUE to avoid error if NA
     } else if (isTRUE(ontarget$base.2 == "G") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.2 / depth)*100)
     } else if (isTRUE(ontarget$base.3 == "G") ==TRUE){
       ontarget <- transform(ontarget , percentage = (reads.3 / depth)*100)
     } else if (isTRUE(ontarget$base.4 == "G") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.4 / depth)*100)
     } else if (isTRUE(ontarget$base.1 == "C") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.2 / depth)*100)
     } else if (isTRUE(ontarget$base.2 == "C") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.2 / depth)*100)
     } else if (isTRUE(ontarget$base.3 == "C") ==TRUE){
       ontarget <- transform(ontarget , percentage = (reads.3 / depth)*100)
     } else if (isTRUE(ontarget$base.4 == "C") ==TRUE) {
       ontarget <- transform(ontarget , percentage = (reads.4 / depth)*100)
     }  else{
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
     idx <- match(data$position, AllAT$position)
     data$MutBase <- AllAT$base[idx]
     data$MutReads <- AllAT$reads[idx]
     data$percentage <-AllAT$percentage[idx]
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
