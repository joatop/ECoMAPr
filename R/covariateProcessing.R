# 1. ---- FUNCTIONS FOR HANDLING COVARIATE METADATA ----

### 1.1 ==== Set metadata for a scalar covariate ====
#' @title Set Metadata for a Scalar Covariate
#' 
#' @description Create a covariate metadata object for a scalar covariate.
#' 
#' @param desc A character scalar giving a description of the covariate.
#' @param units A character scalar giving informationn about the units of the
#' covariate.
#' @param mincol A character scalar giving a colour name (as listed by
#' \code{\link[grDevices]{colors}}), or a hexadecimal string like the type
#' returned by \code{\link[grDevices]{rgb}}, that specifies the default colour
#' to use for the lowest value when plotting maps of the covariate.
#' @param maxcol A character scalar giving a colour name (as listed by
#' \code{\link[grDevices]{colors}}), or a hexadecimal string like the type
#' returned by \code{\link[grDevices]{rgb}}, that specifies the default colour
#' to use for the highest value when plotting maps of the covariate.
#' @param nacol A character scalar giving a colour name (as listed by
#' \code{\link[grDevices]{colors}}), or a hexadecimal string like the type
#' returned by \code{\link[grDevices]{rgb}}, that specifies the default colour
#' to use for \code{NA} values when plotting maps of the covariate.
#' 
#' @return A \code{scalarcovmetadata} object that contains the entire metadata
#' information.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[grDevices]{colors}}, \code{\link[grDevices]{rgb}}
#' @export
scalarCovMetadata <- function(desc, units, mincol = grDevices::rgb(255, 225, 255, maxColorValue = 255), maxcol = grDevices::rgb(176, 23, 31, maxColorValue = 255), nacol = grDevices::rgb(255, 255, 255, maxColorValue = 255)) {
  # Function to sanity check inputs
  testInput <- function(parval) {
    inval <- tryCatch(as.character(parval), error = function(err) {
      stop("invalid argument type: ", err)
    })
    if(length(inval) <= 0) {
      stop("invalid argument type: input vector has length zero")
    } else if(length(inval) > 1) {
      warning("input vector has length greater than one: only the first element will be used")
      inval <- inval[1]
    }
    if(is.na(inval)) {
      stop("invalid argument type: input is NA")
    }
    inval
  }
  # Import the metadata information
  outVec <- list(
    desc = testInput(desc),
    units = testInput(units),
    colours = stats::setNames(c(
      testInput(mincol), testInput(maxcol), testInput(nacol)
    ), c("mincol", "maxcol", "nacol"))
  )
  class(outVec) <- c("scalarcovmetadata", class(outVec))
  outVec
}

### 1.2 ==== Set metadata for a nominal/ordinal covariate ====
#' @title Set Metadata for a Nominal Covariate
#' 
#' @description Create a covariate metadata object for a nominal (or ordinal)
#' covariate.
#' 
#' @param desc A character scalar giving a description of the covariate.
#' @param catdesc A character vector containing the description of each of the
#' categories.
#' @param catval A numeric vector containing the value representing the category
#' in the raster object that this metadata describes.
#' @param catcol A character scalar giving default colours (in the format that
#' would be returned by \code{\link[grDevices]{colors}}) to be used in plotting
#' of the categories.
#' @param nacol A character scalar giving a colour name (as listed by
#' \code{\link[grDevices]{colors}}), or a hexadecimal string like the type
#' returned by \code{\link[grDevices]{rgb}}, that specifies the default colour
#' to use for \code{NA} values when plotting maps of the covariate.
#' @param ordinal A logical scalar.  If \code{TRUE} then the categories are
#' ordered ins everity or magnitude (for example, "low", "medium", "high").
#' 
#' @return A \code{nominalcovmetadata} object that contains the entire metadata
#' information.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[grDevices]{colors}}, \code{\link[grDevices]{rgb}}
#' @export
nominalCovMetadata <- function(desc, catdesc, catval = 1:length(catdesc), catcol = grDevices::palette(length(catdesc), recycle = TRUE), nacol = grDevices::rgb(255, 255, 255, maxColorValue = 255), ordinal = FALSE) {
  # Function to sanity check inputs
  testInput <- function(parval, isLengthOne) {
    inval <- tryCatch(as.character(parval), error = function(err) {
      stop("invalid argument type: ", err)
    })
    if(length(inval) <= 0) {
      stop("invalid argument type: input vector has length zero")
    } else if(isLengthOne && length(inval) > 1) {
      warning("input vector has length greater than one: only the first element will be used")
      inval <- inval[1]
    }
    if(any(is.na(inval))) {
      stop("invalid argument type: input is NA")
    }
    inval
  }
  # Sanity check the ordinal flag input
  inord <- tryCatch(as.logical(ordinal), error = function(err) {
    stop("invalid argument type: ", err)
  })
  if(length(inord) <= 0) {
    stop("invalid argument type: input vector has length zero")
  } else if(length(inord) > 1) {
    warning("input vector has length greater than one: only the first element will be used")
    inord <- inord[1]
  }
  if(any(is.na(inord))) {
    stop("invalid argument type: input is NA")
  }
  # Import the metadata information
  outVec <- list(
    desc = testInput(desc, TRUE),
    nacol = testInput(nacol, TRUE),
    ordinal = inord
  )
  incatdesc <- testInput(catdesc, FALSE)
  incatval <- testInput(catval, FALSE)
  incatcol <- testInput(catcol, FALSE)
  outVec$catinfo <- data.frame(
    value = incatval,
    col = matrix(
      c(incatdesc, incatcol[(1:length(incatdesc) - 1) %% incatcol + 1]),
      ncol = 2, dimnames = list(
        incatval[(1:length(incatdesc) - 1) %% incatval + 1],
        c("desc", "col")
      )
    )
  )
  class(outVec) <- c("nominalcovmetadata", class(outVec))
  outVec
}

# 2. ---- FUNCTIONS FOR HANDLING COVARIATE LAYERS ----

### 2.1 ==== Create time-stamped covariate layer ====
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
#' @keywords internal
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
  inRast <- terra::rast(x = xIn, subds = subds, lyrs = lyrs, drivers = drivers, opts = opts, win = win, snap = snap, vsi = vsi, raw = raw, noflip = noflip, guessCRS = guessCRS, domains = domains, md = md, dims = dims)[, 1]
  if(!is.null(inLayDate)) {
    attr(inRast, "lyrdate") <- inLayDate
  }
  inRast
}

### 2.2 ==== Set time-stamped covariate layer ====
#' @title Set a Covariate Layer at a Given Time Stamp
#'
#' @description Function to assign a particular covariate layer for use in
#' modelling functions.
#' 
#' @inheritParams getCovariateLayer
#' 
#' @return A language object containing an appropriately parametrised call
#' to \code{\link{getCovariateLayer}}.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[base]{call}}, \code{\link[base]{as.Date}},
#'  \code{\link[terra]{rast}}, \code{\link{getCovariateLayer}}
#' @keywords internal
#' @export
setCovariateLayer <- function(x, subds = 0, lyrs = NULL, drivers = NULL, opts = NULL, win = NULL, snap = "near", vsi = FALSE,
  raw = FALSE, noflip = FALSE, guessCRS = TRUE, domains = "", md = NULL, dims = NULL, lyrDate = NULL) {
  call("getCovariateLayer", x = x, subds = subds, lyrs = lyrs, drivers = drivers, opts = opts, win = win, snap = snap, vsi = vsi, raw = raw, noflip = noflip, guessCRS = guessCRS, domains = domains, md = md, dims = dims, lyrDate = lyrDate)
}

### 2.3 ==== Specify a covariate ====
#' @title Specify the Structure of a Covariate
#' 
#' @description Function that specifies the data location and metadata of a
#' covariate to be used in modelling.
#' 
#' @param x Either a character vector containing the locations of the rasters
#' that make up this covariate or a list with each element itself being a list
#' containing named elements that correspond to the arguments of
#' \code{\link[terra]{rast}} used to access the raster. If \code{x} has a
#' \code{names} attribute then the names are coerced to a date (using the
#' \code{\link{as.Date}} function) and that date is used to time reference the
#' raster layers (for spatio-temporal modelling).
#' @param ... Named arguments that can be used as default arguments for
#' \code{\link[terra]{rast}} when processing the elements of \code{x}. Matching
#' arguments will also be passed to \code{\link{scalarCovMetadata}} or
#' \code{\link{nominalCovMetadata}} to set the relevant metadata for the
#' covariate.
#' 
#' @return A \code{"covariateSpec"} object containing the unevaluated expressions
#' to import covariate information from raster files. 
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[base]{as.Date}}, \code{\link[terra]{rast}},
#'  \code{\link{scalarCovMetadata}}, \code{\link{nominalCovMetadata}}
#' @export
specifyCovariate <- function(x, ...) {
  # Sanity check the input
  xin <- x
  xnames <- names(xin)
  if(is.character(xin)) {
    # If the input is a character vector then convert it to a list
    xin <- lapply(X = xin, FUN = function(curVal) { list(x = curVal) })
  } else {
    xin <- tryCatch(as.list(xin), error = function(err) {
      stop("invalid covariate specification argument: ", err)
    })
  }
  if(!is.null(xnames)) {
    # If the input has a names attribute then use that as date information
    xnames <- tryCatch(as.Date(xnames), error = function(err) {
      stop("covariate specification has invalid date information: ", err)
    })
    xin <- lapply(X = 1:length(xin), FUN = function(curIndx, xin, xnames) {
      outVal <- xin[[curIndx]]
      if(!is.null(names(outVal)) && !is.null(outVal$lyrDate)) {
        if(outVal$lyrDate != xnames[curIndx]) {
          warning("mistmatch of covariate layer date between specification and names attribute: will use the names attiribute version")
          outVal$lyrDate <- xnames[curIndx]
        }
      } else {
        outVal <- append(outVal, list(lyrDate = xnames[curIndx]))
      }
      outVal
    }, xin = xin, xnames = xnames)
  } else {
    xnames <- sapply(X = xin, FUN = function(curElement) {
      outEl <- NA
      if(!is.null(curElement$lyrDate)) {
        outEl <- as.Date(curElement$lyrDate)
      }
      outEl
    })
  }
  if(length(xnames) <= 0) {
    stop("invalid covariate specification argument: input vector has length zero")
  } else if(length(xnames) > 1 && any(is.na(xnames) | xnames == "")) {
    stop("invalid covariate specification argument: input vectors of lengths greater then one must have named elements")
  }
  names(xin) <- xnames
  # Find arguments that are in the nominal metadata function but aren't in the scalar
  # metadata function
  argList <- list(...)
  inMetadata <- NULL
  nominalArgs <- methods::formalArgs(nominalCovMetadata)[
    !methods::formalArgs(nominalCovMetadata) %in% methods::formalArgs(scalarCovMetadata)
  ]
  if(!is.null(names(argList))) {
    if(any(names(argList) %in% nominalArgs)) {
      # Input is a nominal covariate
      inMetadata <- do.call(nominalCovMetadata, argList[names(argList) %in% methods::formalArgs(nominalCovMetadata)])
      argList <- argList[!(names(argList) %in% methods::formalArgs(nominalCovMetadata))]
    } else {
      # Input is a scalar covariate
      inMetadata <- do.call(scalarCovMetadata, argList[names(argList) %in% methods::formalArgs(scalarCovMetadata)])
      argList <- argList[!(names(argList) %in% methods::formalArgs(scalarCovMetadata))]
    }
  }
  # Set each of the covariate layers
  xout <- lapply(X = xin, FUN = function(curEl, argList) {
    do.call(setCovariateLayer, append(curEl, argList))
  }, argList = argList)
  names(xout) <- xnames
  attr(xout, "metadata") <- inMetadata
  class(xout) <- c("covariateSpec", class(xout))
  xout
}

### 2.4 ==== Align generic method ====
#' @title Align the Covariate(s)
#' 
#' @description Function to align covariate raster layers to ensure they have
#' the same spatial extent, resolution, and coordinate reference system.
#' 
#' @inheritParams align.covariateSpec
#' 
#' @return A \code{"covariateSpec"} object containing the unevaluated
#' expressions to import the aligned covariate information.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[terra]{crs}}, \code{\link[terra]{writeRaster}},
#'  \code{\link{specifyCovariate}}
#' @export
align <- function(spec, crs = NULL, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, cellres = NULL, forceAlign = FALSE, ...) {
  UseMethod("align.covariateSpec")
}

### 2.5 === Retrieve the spatial information about the covariate layer ====
#' @title Retrieve Covariate Layer Spatial Information
#' 
#' @description Function to find the spatial information about the covariate
#' layer that retrieves the information needed for alignment purposes
#' 
#' @param spec A \code{"covariateSpec"} object (as produced by
#' \code{\link{specifyCovariate}}) containing the layers of the covariate to
#' align.
#' @param crs The coordinate reference system to use in the alignment (see
#' \code{\link[terra]{crs}}). If \code{NULL} then the CRS of the first layer
#' will be used.
#' @param xmin The lower x-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param xmax The upper x-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param ymin The lower y-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param ymax The upper y-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param cellres A numeric vector containing the resolution of the cells in the
#' aligned raster in the x and y dimension.  If length 2 then the first element
#' specifies the resolution in the x-dimension and the second element specifies
#' the resolution in the y-dimension. If length 1 then the values is used for
#' the resolution in both dimensions.  If \code{NULL} the resolution  is
#' calculated automatically from the finest resolution layer in the covariate.
#' 
#' @return A list of spatial information for each of the covariate layers.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[terra]{crs}}, \code{\link[terra]{writeRaster}},
#'  \code{\link{specifyCovariate}}
#' @keywords internal
#' @export
getCovariateLayerSpatialInfo <- function(spec, crs = NULL, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, cellres = NULL) {
  # Function for sanity checking scalar input
  scalarSanityCheck <- function(x, isnumeric = TRUE, lengthTwo = FALSE) {
    xin <- x
    if(isnumeric) {
      xin <- tryCatch(as.numeric(xin), error = function(err) {
        stop("invalid argument type: ", err)
      })
    } else {
      xin <- tryCatch(as.character(xin), error = function(err) {
        stop("invalid argument type: ", err)
      })
    }
    if(length(xin) <= 0) {
      xin <- NULL
    } else if(!lengthTwo && length(xin) > 1) {
      warning("input argument has a length greater than one: only the first element will be used")
      xin <- xin[1]
    } else if(lengthTwo) {
      if(length(xin) == 1) {
        xin <- c(xin, xin)
      } else if(length(xin) > 2) {
        warning("input argument has a length greater than two: only the first two elements will be used")
        xin <- xin[1:2]
      }
    }
    if(any(is.na(xin))) {
      xin <- NULL
    }
    xin
  }
  # Retrieve the input rasters' spatial information
  if(!inherits(spec, "covariateSpec")) {
    stop("input is not a covariate specification object")
  }
  rastDef <- stats::setNames(lapply(X = spec, FUN = function(curElement) {
    # Initialise an output list
    outList <- list(crs = NULL, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, cellres = NULL)
    # Load the raster using the specified loading arguments
    inputRast <- eval(curElement)
    # Export the raster spatial extent, resolution and CRS
    outList$xmin <- terra::xmin(inputRast)
    outList$xmax <- terra::xmax(inputRast)
    outList$ymin <- terra::ymin(inputRast)
    outList$ymax <- terra::ymax(inputRast)
    outList$cellres <- terra::res(inputRast)
    outList$crs <- terra::crs(inputRast)
    outList
  }), names(spec))
  # Sanity check the other arguments
  incrs <- scalarSanityCheck(crs, FALSE)
  if(is.null(incrs)) {
    incrs <- rastDef[[1]]$crs
  }
  # First run through the covariate layers and work out spatial characteristics
  # (cell resolution and bounding box) based on a common CRS
  rastDef <- stats::setNames(lapply(X = rastDef, FUN = function(curDef, targetcrs) {
    outList <- append(curDef, list(
      xmin_conv = curDef$xmin,
      xmax_conv = curDef$xmax,
      ymin_conv = curDef$ymin,
      ymax_conv = curDef$ymax,
      cellres_conv = curDef$cellres
    ))
    if(curDef$crs != targetcrs) {
      # If the CRS are different between the layers then convert the bounding box
      # information accordingly
      tempCoords <- sf::st_as_sf(matrix(
        c(
          outList$xmin, outList$ymin,
          outList$xmin, outList$ymax,
          outList$xmax, outList$ymax,
          outList$xmax, outList$ymin
        ),
        ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y"))),
        coords = c("x", "y"), crs = sf::st_crs(outList$crs)
      )
      tempCoords <- sf::st_transform(tempCoords, sf::st_crs(targetcrs))
      outbbox <- sf::st_bbox(tempCoords)
      outList$xmin_conv <- outbbox["xmin"]
      outList$xmax_conv <- outbbox["xmax"]
      outList$ymin_conv <- outbbox["ymin"]
      outList$ymax_conv <- outbbox["ymax"]
      numx <- (outList$xmax - outList$xmin) / outList$cellres[1]
      numy <- (outList$ymax - outList$ymin) / outList$cellres[2]
      outList$cellres_conv <- c(
        (outList$xmax_conv - outList$xmin_conv) / numx,
        (outList$ymax_conv - outList$ymin_conv) / numy
      )
    }
    outList
  }, targetcrs = incrs), names(rastDef))
  inxmin <- scalarSanityCheck(xmin)
  if(is.null(inxmin)) {
    # inxmin <- rastDef[[1]]$xmin
    inxmin <- max(sapply(X = rastDef, FUN = function(curDef) { curDef$xmin_conv }))
  }
  inxmax <- scalarSanityCheck(xmax)
  if(is.null(inxmax)) {
    # inxmax <- rastDef[[1]]$xmax
    inxmax <- min(sapply(X = rastDef, FUN = function(curDef) { curDef$xmax_conv }))
  }
  inymin <- scalarSanityCheck(ymin)
  if(is.null(inymin)) {
    # inymin <- rastDef[[1]]$ymin
    inymin <- max(sapply(X = rastDef, FUN = function(curDef) { curDef$ymin_conv }))
  }
  inymax <- scalarSanityCheck(ymax)
  if(is.null(inymax)) {
    #inymax <- rastDef[[1]]$ymax
    inymax <- min(sapply(X = rastDef, FUN = function(curDef) { curDef$ymax_conv }))
  }
  incellres <- scalarSanityCheck(cellres, lengthTwo = TRUE)
  if(is.null(incellres)) {
    # incellres <- rastDef[[1]]$cellres
    incellres <- c(
      min(sapply(X = rastDef, FUN = function(curDef) { curDef$cellres_conv[1] })),
      min(sapply(X = rastDef, FUN = function(curDef) { curDef$cellres_conv[2] }))
    )
  }
  inxmax <- inxmin + ceiling((inxmax - inxmin) / incellres[1]) * incellres[1]
  inymax <- inymin + ceiling((inymax - inymin) / incellres[2]) * incellres[2]
  if(inxmin >= inxmax || inymin >= inymax) {
    stop("bounding box area equal to or less then zero: invalid bounding bax arguments or non-overlapping layers")
  }
  # Check to see if the rasters are already aligned
  isAligned <- sapply(X = rastDef, FUN = function(curDef, crs, xmin, xmax, ymin, ymax, cellres) {
    curDef$crs == crs && curDef$xmin == xmin && curDef$xmax == xmax && curDef$ymin == ymin && curDef$ymax == ymax && all(curDef$cellres == cellres)
  }, crs = incrs, xmin = inxmin, xmax = inxmax, ymin = inymin, ymax = inymax, cellres = incellres)
  attr(rastDef, "alignStatus") <- isAligned
  attr(rastDef, "align_boundary") <- stats::setNames(c(inxmin, inxmax, inymin, inymax), c("xmin", "xmax", "ymin", "ymax"))
  attr(rastDef, "align_cellres") <- incellres
  attr(rastDef, "align_crs") <- incrs
  rastDef
}

### 2.6 ==== Align a covariate layer ====
#' @title Align the Covariate Layer
#' 
#' @description Function that ensures the layers of the covariate have the same
#' spatial extent, resolution, and coordinate reference system.
#' 
#' @param spec A \code{"covariateSpec"} object (as produced by
#' \code{\link{specifyCovariate}}) containing the layers of the covariate to
#' align.
#' @param crs The coordinate reference system to use in the alignment (see
#' \code{\link[terra]{crs}}). If \code{NULL} then the CRS of the first layer
#' will be used.
#' @param xmin The lower x-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param xmax The upper x-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param ymin The lower y-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param ymax The upper y-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param cellres A numeric vector containing the resolution of the cells in the
#' aligned raster in the x and y dimension.  If length 2 then the first element
#' specifies the resolution in the x-dimension and the second element specifies
#' the resolution in the y-dimension. If length 1 then the values is used for
#' the resolution in both dimensions.  If \code{NULL} the resolution  is
#' calculated automatically from the finest resolution layer in the covariate.
#' @param forceAlign If \code{TRUE}, create a new aligned raster object and
#' save it even if the existing layers are already aligned.
#' @param ... Named arguments that can be used as arguments for
#' \code{\link[terra]{writeRaster}} when saving the aligned SpatRaster object.
#' It is often useful to set a \code{filename} argument to direct the output
#' for the aligned raster (otherwise the location defaults to
#' \code{tempfile(fileext = ".gif")}.
#' 
#' @return A \code{"covariateSpec"} object containing the unevaluated
#' expressions to import the aligned covariate information.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[terra]{crs}}, \code{\link[terra]{writeRaster}},
#'  \code{\link{specifyCovariate}}
#' @export
align.covariateSpec <- function(spec, crs = NULL, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, cellres = NULL, forceAlign = FALSE, ...) {
  # Retrieve the covariate layer spatial information
  rastDef <- getCovariateLayerSpatialInfo(spec, crs, xmin, xmax, ymin, ymax, cellres)
  outSpec <- spec
  if(scalarSanityCheck(forceAlign) > 0 || !all(attr(rastDef, "alignStatus"))) {
    # Assemble the other arguments as arguments for the writeRaster function
    writeArgs <- list(...)
    if(is.null(writeArgs$filename)) {
      writeArgs$filename <- tempfile(fileext = ".gif")
    }
    if(is.null(writeArgs$filetype)) {
      writeArgs$filetype <- "COG"
    }
    # Initialise a temporary raster object
    writeArgs$x <- terra::rast(
      xmin = attr(rastDef, "align_boundary")["xmin"],
      xmax = attr(rastDef, "align_boundary")["xmax"],
      ymin = attr(rastDef, "align_boundary")["ymin"],
      ymax = attr(rastDef, "align_boundary")["ymax"],
      crs = attr(rastDef, "align_crs"),
      resolution = attr(rastDef, "align_cellres"),
      nlyrs = length(rastDef), names = names(rastDef))
    cellCoords <- sf::st_as_sf(as.data.frame(terra::xyFromCell(writeArgs$x, 1:(terra::ncell(writeArgs$x)))), coords = c("x", "y"), crs = sf::st_crs(incrs))
    # If the rasters aren't already aligned (or if the user is forcing the alignment) then align them
    terra::setValues(writeArgs$x, values = sapply(X = 1:length(rastDef), FUN = function(curIndex, rastDef, tmpRast, spec, cellCoords) {
      # Load the raster
      covRaster <- eval(spec[[curIndex]])
      outValues <- rep(NA, terra::ncell(tmpRast))
      if(
        rastDef[[curIndex]]$crs == terra::crs(tmpRast) &&
        rastDef[[curIndex]]$xmin == terra::xmin(tmpRast) &&
        rastDef[[curIndex]]$xmax == terra::xmax(tmpRast) &&
        rastDef[[curIndex]]$ymin == terra::ymin(tmpRast) &&
        rastDef[[curIndex]]$ymax == terra::ymax(tmpRast) &&
        all(rastDef[[curIndex]]$cellres == terra::res(tmpRast))
      ) {
        # Rasters are already aligned: just copy the values across
        outValues <- terra::values(covRaster)[, 1]
      } else {
        # Rasters are not aligned: retrieve cell values
        tmpCoords <- cellCoords
        if(rastDef[[curIndex]]$crs != terra::crs(tmpRast)) {
          tmpCoords <- sf::st_transform(cellCoords, rastDef[[curIndex]]$crs)
        }
        outValues <- terra::extract(covRaster, tmpCoords)[, 1]
      }
      outValues
    }, rastDef = rastDef, tmpRast = writeArgs$x, spec = spec, cellCoords = cellCoords), keepnames = TRUE)
    # Write the raster to the output location
    do.call(terra::writeRaster, writeArgs)
    outSpec <- lapply(X = 1:(terra::nlyr(writeArgs$x)), FUN = function(lyrIndx, rastLoc, lyrDates) {
      outVal <- NULL
      if(is.null(lyrDates)) {
        outVal <- setCovariateLayer(x = rastLoc, lyrs = lyrIndx)
      } else {
        outVal <- setCovariateLayer(x = rastLoc, lyrs = lyrIndx, lyrDate = lyrDates[lyrIndx])
      }
      outVal
    }, rastLoc = writeArgs$filename, lyrDates = names(spec))
  }
  outSpec
}

### 2.7 ==== Create a covariate specification list ====
#' @title Create a Covariate Specification List
#'
#' @description Function to create a list of covariate specifications for use
#' in ECoMAP modelling.
#' 
#' @param ... A named set of arguments. Each element should be a
#' \code{"covariateSpec"} object as resulting from
#' \code{\link{specifyCovariate}}.
#' 
#' @return A \code{"covariateSpecList"} object containing the unevaluated
#' expressions to import covariate information from raster files. 
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[base]{as.Date}}, \code{\link[terra]{rast}},
#'  \code{\link{scalarCovMetadata}}, \code{\link{nominalCovMetadata}},
#'  \code{\link{specifyCovariate}}
#' @export
createCovariateSpecList <- function(...) {
  xin <- list(...)
  if(any(!sapply(X = xin, FUN = inherits, what = "covariateSpec"))) {
    stop("argumnet is not a covariate specification object")
  }
  class(xin) <- c("covariateSpecList", class(xin))
  xin
}

### 2.8 ==== Align a covariate layer list ====
#' @title Align the Covariate Layer List
#' 
#' @description Function that ensures the layers of each element in the list of
#' covariates have the same spatial extent, resolution, and coordinate reference
#' system.
#' 
#' @param spec A \code{"covariateSpecList"} object (as produced by
#' \code{\link{createCovariateSpecList}}) containing the list of layers of the
#' covariates to align.
#' @param crs The coordinate reference system to use in the alignment (see
#' \code{\link[terra]{crs}}). If \code{NULL} then the CRS of the first layer
#' will be used.
#' @param xmin The lower x-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param xmax The upper x-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param ymin The lower y-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param ymax The upper y-coordinate of the bounding box of the aligned raster.
#' If \code{NULL} then this is calculated automatically from the overlapping
#' extent of the composite layers.
#' @param cellres A numeric vector containing the resolution of the cells in the
#' aligned raster in the x and y dimension.  If length 2 then the first element
#' specifies the resolution in the x-dimension and the second element specifies
#' the resolution in the y-dimension. If length 1 then the values is used for
#' the resolution in both dimensions.  If \code{NULL} the resolution  is
#' calculated automatically from the finest resolution layer in the covariate.
#' @param forceAlign If \code{TRUE}, create a new aligned raster object and
#' save it even if the existing layers are already aligned.
#' @param ... Named arguments that can be used as arguments for
#' \code{\link[terra]{writeRaster}} when saving the aligned SpatRaster object.
#' It is often useful to set a \code{filename} argument to direct the output
#' for the aligned raster (otherwise the location defaults to
#' \code{tempfile(fileext = ".gif")}.
#' 
#' @return A \code{"covariateSpecList"} object containing the unevaluated
#' expressions to import the aligned covariate information.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link[terra]{crs}}, \code{\link[terra]{writeRaster}},
#'  \code{\link{specifyCovariate}}
#' @export
align.covariateSpecList <- function(spec, crs = NULL, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, cellres = NULL, forceAlign = FALSE, ...) {
  # Ensure that the input specification is a list
  inspec <- spec
  if(!inherits(inspec, "covariateSpecList")) {
    inspec <- tryCatch(as.list(inspec), error = function(err) {
      stop("invalid specification input: ", err)
    })
  }
  outSpec <- list()
  if(length(inpsec) > 0) {
    inargs <- list(...)
    inargs$forceAlign <- forceAlign
    # Retrieve the spatial information of the first element
    inargs$crs <- crs
    rastInfo <- getCovariateLayerSpatialInfo(spec = inspec[[1]], crs = inargs$crs, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, cellres = cellres)
    if(is.null(inargs$crs)) {
      # If the CRS is NULL then set it to the alignment CRS from the first element of the input list
      inargs$crs <- attr(rastInfo, "align_crs")
    }
    # Go through the input specification and retrieve the spatial information from the other elements
    if(length(inspec) > 1) {
      # Get the raster information of the other covariates
      rastInfo <- append(list(rastInfo), lapply(X = inspec[2:length(inspec)], FUN = getCovariateLayerSpatialInfo, crs = inargs$crs, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, cellres = cellres))
    }
    names(rastInfo) <- names(inspec)
    # If the bounding box arguments are NULL then calculate the minimum bounding
    # box where the rasters overlap
    inargs$xmin <- xmin
    if(is.null(inargs$xmin)) {
      inargs$xmin <- max(sapply(X = rastInfo, FUN = function(curCov) {
        attr(curCov, "align_boundary")["xmin"]
      }))
    }
    inargs$xmax <- xmax
    if(is.null(inargs$xmax)) {
      inargs$xmax <- min(sapply(X = rastInfo, FUN = function(curCov) {
        attr(curCov, "align_boundary")["xmax"]
      }))
    }
    inargs$ymin <- ymin
    if(is.null(inargs$ymin)) {
      inargs$ymin <- max(sapply(X = rastInfo, FUN = function(curCov) {
        attr(curCov, "align_boundary")["ymin"]
      }))
    }
    inargs$ymax <- ymax
    if(is.null(inargs$ymax)) {
      inargs$ymax <- min(sapply(X = rastInfo, FUN = function(curCov) {
        attr(curCov, "align_boundary")["ymax"]
      }))
    }
    if(inargs$xmax <= inargs$xmin || inargs$ymax <= inargs$ymin) {
      stop("invalid bounding box coordinates: possible non-overlapping covariate rasters")
    }
    # Set the filenames to store the aligned covariate rasters
    if(is.null(inargs$filename) || length(inargs$filename) <= 0) {
      inargs$filename <- tempdir(check = TRUE)
    }
    infilenames <- sapply(X = 1:length(inspec), FUN = function(curIndex, filenames, covnames) {
      curFilename <- filenames[curIndex]
      if(dir.exists(curFilename)) {
        # If the filename is a directory then make a file for each covariate
        if(is.null(covnames) || is.na(covnames[curIndex]) || covnames[curIndex] == "") {
          curFilename <- tempfile(tmpdir = curFilename, fileext = ".gif")
        } else {
          curFilename <- file.path(curFilename, paste(covnames[curIndex], "gif", sep = "."))
        }
      }
      curFilename
    }, filenames = inargs$filename[0:(length(inspec) - 1) %% length(inargs$filename) + 1], covnames = names(inspec))
    # Iterate over the specification list and align each of covariates according to the current spatial extents
    outSpec <- stats::setNames(lapply(X = 1:length(inspec), FUN = function(curIndx, spec, filenames, specargs) {
      curargs <- specargs
      curargs$filename <- filenames[curIndx]
      curargs$spec <- spec[[curIndx]]
      do.call("align.covariateSpec", curargs)
    }, spec = inspec, filenames = infilenames, specargs = inargs), names(inspec))
    outSpec <- do.call("createCovariateSpecList", outSpec)
  }
  outSpec
}