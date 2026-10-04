### 1.1 ==== Classify Occurrence Data ====
#' @title Classify Occurrence Data
#'
#' @description Function to take species occurrence data and classify it into
#' categories of 'presence only', 'presence/absence', or 'density'.
#'
#' @param occData A data frame (or object that inherits from a data frame)
#' containing the occurrence data to classify.
#' @param datasetIDCol A character vector containing the name of the column
#' containing a the dataset ID grouping variable.
#' @param occurrenceStatusCol A character vector containing the name of the
#' column(s) containing the occurrence status (marked as \code{"PRESENT"} or
#' \code{"ABSENT"}).
#' @param densityCol A character vector containing the name of the column(s)
#' containing the density data.
#' @param occurrenceOutColName A character scalar giving the name for the extra
#' column that will be appended to the occurrence data that will store the
#' combined occurrence information.
#' @param densityOutColName A character scalar giving the name for the extra
#' column that will be appended to the density data that will store the combined
#' density information.
#'
#' @return A list with the following elements:
#' \describe{
#'  \item{\code{presenceOnly}}{A data frame containing the rows that have been
#'  classified as having data that only contains presence records with no
#'  information on sampling effort ("presence only data").}
#'  \item{\code{presenceAbsence}}{A data frame containing the rows that have
#'  been classified as having data that contains presences or absences
#'  ("presence/absence data"). An extra column is appended to the data frame
#'  with a name specified by \code{occurrenceOutColName}.}
#'  \item{\code{density}}{A data frame containing the rows that have been
#'  classified as having data that contains some form of density information
#'  ("density data").  An extra column is appended to the data frame with a name
#'  specified by \code{densityOutColName}.}
#' }
#'
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @export
classifyOccDataType <- function(occData, datasetIDCol = "datasetKey", occurrenceStatusCol = "occurrenceStatus", densityCol = c("individualCount", "organismQuantity"), occurrenceOutColName = "occValues", densityOutColName = "densityValues") {
  absenceVals <- c("ABSENT", "ABSENCE", "0", "FALSE")
  # Sanity check the occurrence data
  inOccData <- occData
  if(!inherits(inOccData, "data.frame")) {
    inOccData <- tryCatch(as.data.frame(inOccData), error = function(err) {
      stop("invalid occurrence data structure: ", err)
    })
  }
  # Function to test whether the input column names are present in the target data
  hasCols <- function(inColName, inData) {
    sapply(X = inColName, FUN = function(curColName, inData) {
      !is.null(inData[[curColName]])
    }, inData = inData)
  }
  # Sanity check the dataset ID column
  inDatatsetIDCol <- tryCatch(as.character(datasetIDCol), error = function(err) {
    stop("invalid entry for the dataset ID column: ", err)
  })
  datasetColPres <- hasCols(inDatatsetIDCol, inOccData)
  if(any(!datasetColPres)) {
    warning("the following dataset ID column names were not  found in the occurrence dataset (and will therefore be ignored): ", paste(inDatatsetIDCol[!datasetColPres], collapse = ", "))
    inDatasetIDCol <- inDatatsetIDCol[datasetColPres]
  }
  # Sanity check the occurrence status column
  inOccurrenceStatusCol <- tryCatch(as.character(occurrenceStatusCol), error = function(err) {
    stop("invalid entry for occurrence status column: ", err)
  })
  occurrenceStatusColPres <- hasCols(inOccurrenceStatusCol, inOccData)
  if(any(!occurrenceStatusColPres)) {
    warning("the following occurrence status column names were not found in the occurrence dataset (and will therefore be ignored): ", paste(inOccurrenceStatusCol[!occurrenceStatusColPres], collapse = ", "))
    inOccurrenceStatusCol <- inOccurrenceStatusCol[occurrenceStatusColPres]
  }
  # Sanity check the density column
  inDensityCol <- tryCatch(as.character(densityCol), error = function(err) {
    stop("invalid entry for the density column: ", err)
  })
  densityColPres <- hasCols(inDensityCol, inOccData)
  if(any(!densityColPres)) {
    warning("the following density specification column names were not found in the occurrence dataset (and will therefore be ignored): ", paste(inDensityCol[!densityColPres], collapse = ", "))
    inDensityCol <- inDensityCol[densityColPres]
  }
  # Sanity check the occurrence output column name
  inOccurrenceOutColName <- tryCatch(as.character(occurrenceOutColName), error = function(err) {
    stop("invalid entry for the density column output name: ", err)
  })
  if(length(inOccurrenceOutColName) == 0) {
    stop("invalid entry for the density column output name: input vector has zero length")
  } else if(length(inOccurrenceOutColName) > 1) {
    warning("density column output name has length greater than one: only the first element will be used")
    inOccurrenceOutColName <- inOccurrenceOutColName[1]
  }
  # Sanity check the density output column name
  inDensityOutColName <- tryCatch(as.character(densityOutColName), error = function(err) {
    stop("invalid entry for the density column output name: ", err)
  })
  if(length(inDensityOutColName) == 0) {
    stop("invalid entry for the density column output name: input vector has zero length")
  } else if(length(inDensityOutColName) > 1) {
    warning("density column output name has length greater than one: only the first element will be used")
    inDensityOutColName <- inDensityOutColName[1]
  }
  # Initialise an output vector of data classification types
  dataType <- rep("presenceOnly", nrow(inOccData))
  # Combine the dataset ID columns to create a unique key
  datasetComb <- apply(X = as.matrix(as.data.frame(inOccData)[, inDatatsetIDCol, drop = FALSE]), FUN = paste, collapse = "", MARGIN = 1)
  occVal <- rep(NA, nrow(inOccData))
  if(length(inOccurrenceStatusCol) > 0 && length(inDatatsetIDCol) > 0) {
    
    # Find the datasets that have at least one absence
    hasAbsence <- unique(datasetComb[apply(X = as.matrix(as.data.frame(inOccData)[, inOccurrenceStatusCol, drop = FALSE]), FUN = function(curRow, absenceVals) {
      any(toupper(as.character(curRow)) %in% absenceVals)
    }, absenceVals = absenceVals, MARGIN = 1)])
    presAbsElement <- datasetComb %in% hasAbsence
    dataType[presAbsElement] <- "presenceAbsence"
    # Check the relevant columns for presence/absence information
    occVal <- apply(X = as.matrix(as.data.frame(inOccData)[, inOccurrenceStatusCol, drop = FALSE]), FUN = function(curRow, absenceVals) {
      any(!(toupper(as.character(curRow)) %in% absenceVals))
    }, absenceVals = absenceVals, MARGIN = 1)
    occVal[!presAbsElement] <- NA
  }
  densVal <- rep(NA, nrow(inOccData))
  if(length(inDensityCol) > 0 && length(inDatatsetIDCol) > 0) {
    # Perform an initial classification based on the presence of density information in the appropriate columns
    # Check the relevant columns for density information
    densVal <- apply(X = as.matrix(as.data.frame(inOccData)[, inDensityCol, drop = FALSE]), FUN = function(curRow) {
      inRow <- as.numeric(curRow)
      indsHas <- which(!is.na(inRow))
      if(length(indsHas) <= 0) {
        indsHas <- NA
      } else {
        indsHas <- inRow[indsHas[1]]
      }
      indsHas
    }, MARGIN = 1)
    dataType[!is.na(densVal)] <- "density"
    # Revise the classification based on the nature of the density information
    # If a dataset only has 0 or 1 for the density values then the dataset is
    # probably presence/absence data
    for(curDataSet in unique(datasetComb[dataType == "density"])) {
      isCurDataSet <- datasetComb == curDataSet
      if(all(densVal[isCurDataSet] %in% 0:1)) {
        occVal[isCurDataSet] <- as.logical(densVal[isCurDataSet])
        densVal[isCurDataSet] <- NA
        dataType[isCurDataSet] <- "presenceAbsence"
      }
    }
  }
  if(length(inOccurrenceStatusCol) > 0 && length(inDatatsetIDCol) > 0) {
    # Revisit the presence/absence datasets and check for edge cases
    for(curDataSet in unique(datasetComb[dataType == "presenceAbsence"])) {
      isCurDataSet <- datasetComb == curDataSet
      if(all(occVal[isCurDataSet])) {
        # If every record in the dataset is a presence then reclassify dataset as
        # a "presence only" dataset
        occVal[isCurDataSet] <- NA
        dataType[isCurDataSet] <- "presenceOnly"
      } else if(all(!occVal[isCurDataSet])) {
        # If every record in the dataset is an absence then reclassify the dataset as
        # an "unknown" dataset type - and won't be used.
        occVal[isCurDataSet] <- NA
        dataType[isCurDataSet] <- "unknown"
      }
    }
  }
  # Split up the different data types into a list of data frames
  outClassList <- list(
    presenceOnly = inOccData[dataType == "presenceOnly", ],
    presenceAbsence = inOccData[dataType == "presenceAbsence",] ,
    density = inOccData[dataType == "density", ])
  outClassList$presenceAbsence[[inOccurrenceOutColName]] <- occVal[dataType == "presenceAbsence"]
  outClassList$density[[inDensityOutColName]] <- densVal[dataType == "density"]
  attr(outClassList, "datasetIDCol") <- inDatatsetIDCol
  attr(outClassList, "occurrenceStatusCol") <- inOccurrenceStatusCol
  attr(outClassList, "densityCol") <- inDensityCol
  attr(outClassList, "outputColumns") <- stats::setNames(c(inOccurrenceOutColName, inDensityOutColName), c("occurrence", "density"))
  outClassList
}
