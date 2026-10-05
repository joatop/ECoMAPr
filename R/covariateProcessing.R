### 1.1 ==== Create time-stamped covariate layer ====
#' @title Retrieve a Covariate Layer at a Given Time Stamp
#'
#' @description Function to load a covariate layer and (optionally) add a
#' timestamp to it.
#' 
#' @param x A character scalar containing the filename of the raster file
#' containing the covariate information.
#' @param subds See \code{\link[terra]{rast}}.
#' @param lyrs See \code{\link[terra]{rast}}.
#' @param drivers See \code{\link[terra]{rast}}.
#' @param opts See \code{\link[terra]{rast}}.
#' @param win See \code{\link[terra]{rast}}.
#' @param snap See \code{\link[terra]{rast}}.
#' @param vsi See \code{\link[terra]{rast}}.
#' @param raw See \code{\link[terra]{rast}}.
#' @param noflip See \code{\link[terra]{rast}}.
#' @param guessCRS See \code{\link[terra]{rast}}.
#' @param domains See \code{\link[terra]{rast}}.
#' @param md See \code{\link[terra]{rast}}.
#' @param dims See \code{\link[terra]{rast}}.
#' @param lyrDate An object containing the date information of the covariate
#' layer.  Can be any type coercible by \code{\link[base]{as.Date}}.
#' 
#' @return A \code{SpatRaster}.  If \code{lyrdate} is not \code{NULL} then the
#' output has an extra attribute, \code{"lyrdate"}, the contains the timestemp
#' information.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[base]{as.Date}}, \code{\link[terra]{rast}}
#' @export
getCovariateLayer <- function(x, subds = 0, lyrs = NULL, drivers = NULL, opts = NULL, win = NULL, snap = "near", vsi = FALSE,
  raw = FALSE, noflip = FALSE, guessCRS = TRUE, domains = "", md = NULL, dims = NULL, lyrDate = NULL) {
  # Sanity check the filename input for the raster layer
  xIn <- tryCatch(as.character(x), error = function(err) {
    stop("invalid entry for the raster filename: ", err)
  })
  if(length(xIn) <= 0) {
    stop("invalid entry for the raster filename: entry has zero length")
  } else if(length(xIn) > 1) {
    warning("raster filename input has a length greater than one: only the first element will be used")
    xIn <- xIn[1]
  }
  if(is.na(xIn)) {
    stop("invalid entry for the raster filename: entry is NA")
  }
  # Sanity check the layer date
  inLayDate <- lyrDate
  if(!is.null(inLayDate)) {
    # Get and format the input layer date
    inLayDate <- tryCatch(as.Date(inLayDate), error = function(err) {
      stop("error encountered whilst processing data information: ", err)
    })
    if(length(inLayDate) <= 0) {
      inLayDate <- NULL
    } else if(length(inLayDate) > 1) {
      warning("covariate date specification has a length greater than one: only the first will be used")
      inLayDate <- inLayDate[1]
    }
  }
  # Import the spat raster
  inRast <- terra::rast(x = xIn, subds = subds, lyrs = lyrs, drivers = drivers, opts = opts, win = win, snap = snap, vsi = vsi, raw = raw, noflip = noflip, guessCRS = guessCRS, domains = domains, md = md, dims = dims)
  if(!is.null(inLayDate)) {
    attr(inRast, "lyrdate") <- inLayDate
  }
  inRast
}

#setCovariateLayer <- function(x, subds = 0, lyrs = NULL, drivers = NULL, opts = NULL, win = NULL, snap = "near", vsi = FALSE,
#  raw = FALSE, noflip = FALSE, guessCRS = TRUE, domains = "", md = NULL, dims = NULL, lyrDate = NULL) {
#  
#}
