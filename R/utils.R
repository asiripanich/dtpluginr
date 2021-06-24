# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  Copyright 2016-2021 University of Melbourne
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Written by: Dr. Yiqun Chen    yiqun.c@unimelb.edu.au
# DevLogs:


#' debug print
#'
#' @param arg1 A string to be printed
#'
#' @return debug print string started with "==="
#' @export
#' @examples
#' dt_debugprint("all done")
dt_debugprint <- function(arg1) {
  print(sprintf("=== %s", arg1))
}


#' load geojson from url into a sp object
#'
#' @param url A geojson url string
#'
#' @return A sp object
#' @export
dt_loadGeoJSON2SP <- function(url) {

  # create a unique temp file name for geojson
  tmpFilePath <- sprintf("%s\\%s.geojson", globalGSCredentials$tempDirPath, UUIDgenerate(FALSE))

  # if tempDirPath not existed, create
  if (dir.exists(globalGSCredentials$tempDirPath) == FALSE) {
    dir.create(globalGSCredentials$tempDirPath, showWarnings = FALSE, recursive = TRUE)

    dt_debugprint(sprintf("%s created", globalGSCredentials$tempDirPath))
  }

  spobj <- tryCatch(
    {
      # load data from url
      con <- GET(url, timeout(36000))
      if (con$status_code != 200) {
        return(NULL)
      }
      geojson <- content(con, "text")
      # save data as a local copy
      write(geojson, tmpFilePath)

      # load it as sp object
      readOGR(tmpFilePath, disambiguateFIDs = TRUE)
    },
    error = function(cond) {
      return(NULL)
    },
    finally = {
      # remove local temp file
      file.remove(tmpFilePath)
    }
  )
  return(spobj)
}

#' load geojson from url (username/password protected) into a sp object
#'
#' @param url A geojson url string
#' @param username username required to access url
#' @param password password required to access url
#'
#' @return A sp object
#' @export
dt_loadGeoJSON2SPWithAuth <- function(url, username, password) {

  # create a unique temp file name for geojson
  tmpFilePath <- sprintf("%s\\%s.geojson", globalGSCredentials$tempDirPath, UUIDgenerate(FALSE))

  # if tempDirPath not existed, create
  if (dir.exists(globalGSCredentials$tempDirPath) == FALSE) {
    dir.create(globalGSCredentials$tempDirPath, showWarnings = FALSE, recursive = TRUE)

    dt_debugprint(sprintf("%s created", globalGSCredentials$tempDirPath))
  }

  spobj <- tryCatch(
    {
      # load data from url
      con <- GET(url, timeout(36000), authenticate(username, password))
      if (con$status_code != 200) {
        return(NULL)
      }
      geojson <- content(con, "text")

      # save data as a local copy
      write(geojson, tmpFilePath)

      # load it as sp object
      # readOGR(tmpFilePath, "OGRGeoJSON")
      readOGR(tmpFilePath, disambiguateFIDs = TRUE)
    },
    error = function(cond) {
      return(NULL)
    },
    finally = {
      # remove local temp file
      file.remove(tmpFilePath)
    }
  )
  return(spobj)
}

#' load geojson from url into a data.frame object. this is particularly useful to handle geojson wfsurl with null geometry
#'
#' @param url A geojson url string
#'
#' @return A data.frame object
#' @export
dt_loadGeoJSON2DF <- function(url) {
  dfobj <- tryCatch(
    {
      # load data from url
      con <- GET(url, timeout(36000))
      if (con$status_code != 200) {
        return(NULL)
      }
      geojson <- content(con, "text")


      tmp <- fromJSON(geojson)
      # only return the properties data frame
      tmp$features$properties
    },
    error = function(cond) {
      return(NULL)
    },
    finally = {

    }
  )
  return(dfobj)
}

#' load geojson from url (username/password protected) into a data.frame object. this is particularly useful to handle geojson wfsurl with null geometry
#'
#' @param url A geojson url string
#' @param username username required to access url
#' @param password password required to access url
#'
#' @return A data.frame object
#' @export
dt_loadGeoJSON2DFWithAuth <- function(url, username, password) {
  dfobj <- tryCatch(
    {
      # load data from url
      con <- GET(url, timeout(36000), authenticate(username, password))
      if (con$status_code != 200) {
        return(NULL)
      }
      geojson <- content(con, "text")

      tmp <- fromJSON(geojson)
      # only return the properties data frame
      tmp$features$properties
    },
    error = function(cond) {
      return(NULL)
    },
    finally = {

    }
  )
  return(dfobj)
}


#' publish a sp object to geoserver using RESTful API via curl
#'
#' @param spobj A sp object. *IMPORTANT: Each column of spobj MUST NOT have attribute name exceeding length (int)10, failed to do so may result in NO Style being generated on GeoServer*
#' @param layerprefix a prefix for layer name, will make the generated data layers in GeoServer more identifiable
#' @return A wfs url string of successfully published data layer
#' @export
dt_publishSP2GeoServer <- function(spobj, layerprefix = "my_") {
  procflag <- TRUE
  publishedWfsUrl <- NULL # if error occurs, return publishedWfsUrl as NULL

  # check if sp is null, return null if null
  if (is.null(spobj)) {
    return(NULL)
  }

  # save spobj as shp file
  tmpFileName <- sprintf("%s%s", layerprefix, UUIDgenerate(FALSE))
  tmpFilePath <- sprintf("%s\\%s", globalGSCredentials$tempDirPath, tmpFileName)
  dir.create(tmpFilePath, showWarnings = FALSE, recursive = TRUE)
  writeOGR(spobj, dsn = tmpFilePath, layer = tmpFileName, driver = "ESRI Shapefile", check_exists = TRUE, overwrite_layer = TRUE)

  # zip shp file
  # ref: for windows, install Rtools from https://cran.r-project.org/bin/windows/Rtools/ and make sure it is on the system Path
  # flag -j to store just the name of a saved file (junk the path), and do not store directory names. By default, zip will store the full path
  zip(zipfile = tmpFilePath, files = dir(tmpFilePath, full.names = TRUE), flags = "-j")

  # upload zip for geoserver
  out <- dt_addShp2DataStore(sprintf("%s.zip", tmpFilePath))
  if (nchar(out) > 0) {
    procflag <- FALSE
  }

  if (procflag) {
    # publish it as new featuretype
    out <- dt_createFeatureType(tmpFileName)
    # if(nchar(out)>0){
    # it rarely happens (current testing indicates when a new ws and new ds is created at the same time, server might raise a 500 error), it seems that the layer is still published as usual. need more investigation, just skip seeting procflag = FALSE for now
    # procflag = FALSE
    # }
  }

  # return wfs url for the uploaded datalayer
  # wfsUrlTemplate: %s/wfs?service=wfs&version=1.0.0&request=GetFeature&typeName=%s:%s&outputFormat=json
  if (procflag) {
    publishedWfsUrl <- sprintf(globalGSCredentials$wfsUrlTemplate, globalGSCredentials$gsRESTURL, globalGSCredentials$gsWORKSPACENAME, tmpFileName)
  }

  # remove tmp zip file
  file.remove(sprintf("%s.zip", tmpFilePath))
  # remove tmp shp file folder
  unlink(tmpFilePath, recursive = TRUE)

  return(publishedWfsUrl)
}

#
#' publish a sp object to geoserver with wms stlying information
#'
#' @param spobj a sp object. *IMPORTANT: Each column of spobj MUST NOT have attribute name exceeding length (int)10, failed to do so may result in NO Style being generated on GeoServer*
#' @param layerprefix a prefix for layer name, will make the generated data layers in GeoServer more identifiable
#' @param styleprefix a prefix for style name, will make the generated styles in GeoServer more identifiable
#' @param displayname a readable display name for data layer
#' @param attrname the attribute name that the style will be created for
#' @param palettename check colors defined under each palette name at http://colorbrewer2.org
#'  acceptable color PaletteNames are: YlOrRd: 9 colors PRGn: 11 colors
#'  PuOr:11 colors RdGy: 11 colors Spectral: 11 colors Grays: 9 colors PuBuGn: 9 colors RdPu: 9 colors BuPu: 9 colors YlOrBr: 9 colors Greens: 9 colors
#'  BuGn: 9 colors Accents: 8 colors GnBu: 9 colors PuRd: 9 colors Purples: 9 colors RdYlGn: 11 colors Paired: 12 colors Blues: 9 colors RdBu: 11 colors
#'  Oranges: 9 colors RdYlBu: 11 colors PuBu: 9 colors OrRd: 9 colors Set3: 12 colors Set2: 8 colors Set1: 9 colors Reds: 9 colors PiYG: 11 colors
#'  Dark2: 8 colors YlGn: 9 colors BrBG: 11 colors YlGnBu: 9 colors
#'  Pastel2: 8 colors Pastel1: 9 colors
#'
#' @param colorreverseorder true or false(default), whether to reverse the color order defined in a palette
#' @param colornum the number of colors to be classified. default 5.  in range of 2-15
#' @param classifier acceptable classifier function names are: Jenks(default), EqualInterval, Quantile, StandardDeviation
#' @param bordercolor border color for polygon layers, default "black", can be white, gray or black.
#' @param borderwidth border stroke width (integer value) for polygon layers. in range [1, 10], default 1
#' @param bordervisible border visibility for polygon layers. TRUE/FALSE, default, TRUE
#' @param styletype is the attribute for styling type, acceptable values are "single", "graduated" or "categorised", default value "single"
#
#' @return a list contains single element to be included in geolayers
#' @export
dt_publishSP2GeoServerWithStyle <- function(spobj,
                                            layerprefix = "my_",
                                            styleprefix = "my_stl_",
                                            attrname,
                                            layerdisplyname = "",
                                            palettename = "Reds",
                                            colorreverseorder = FALSE,
                                            colornum = 5,
                                            classifier = "Jenks",
                                            bordercolor = "black",
                                            borderwidth = 1,
                                            bordervisible = TRUE,
                                            styletype = "single") {

  # TODO: this is the place to add any additional column (i.e. normalised values) to spobj

  # make sure spobj is projected in EPSG:4326 (WGS84) before publishing it.
  spobj <- spTransform(spobj, CRS(proj4string_epsg4326))

  # check if any colname has over 10 character
  oldnames <- colnames(spobj@data)
  newnames <- colnames(spobj@data)
  lengthlimit <- 10
  for (i in 1:length(newnames)) {
    if (nchar(newnames[i]) > lengthlimit) { # if longer than lengthlimit, trim it
      newnames[i] <- substr(newnames[i], 1, lengthlimit)
    }
  }
  # further check if there is any duplicate attribute name after trimming
  autonum <- 1
  prevdupidx <- 0
  while (anyDuplicated(newnames) > 0) {
    dupidx <- anyDuplicated(newnames)
    if (dupidx == prevdupidx) {
      autonum <- autonum + 1
      if (autonum > 10) {
        autonum <- 1
      }
    }

    newnames[dupidx] <- sprintf("%s%i", substr(newnames[dupidx], 1, nchar(newnames[dupidx]) - 1), autonum)
    prevdupidx <- dupidx
  }

  # set trimmed colname for sp obj
  colnames(spobj@data) <- newnames

  # check if any attrname in attrname_vec is trimmed or renamed, update to the new name before creating styles
  for (j in 1:length(oldnames)) {
    if (attrname == oldnames[j]) {
      attrname <- newnames[j]
    }
  }


  # publish sp to geoserver
  outputWfsUrl <- dt_publishSP2GeoServer(spobj, layerprefix)
  if (is.null(outputWfsUrl)) {
    return(NULL)
  }

  # calculte wms style
  wmsStyleResult <- fromJSON(dt_createWMSStyle(
    wfsurl = outputWfsUrl,
    styleprefix = styleprefix,
    attrname = attrname,
    palettename = palettename,
    colorreverseorder = colorreverseorder,
    colornum = colornum,
    classifier = classifier,
    bordercolor = bordercolor,
    borderwidth = borderwidth,
    bordervisible = bordervisible,
    styletype = styletype
  ))
  wmsStyleparams <- list()
  if (wmsStyleResult$status == 0) {
    wmsStyleparams <- wmsStyleResult$data
  }


  # create content of the 1st element

  geolayers_element <- list(
    layername = dt_getLayerNameFromWFSUrl(wfsurl = outputWfsUrl),
    layerdisplayname = layerdisplyname,
    bbox = dt_getBbox(spobj),
    wfs = list(url = outputWfsUrl, styleparams = list()),
    wms = list(url = globalGSCredentials["wmsUrlTemplate"][[1]], styleparams = wmsStyleparams)
  )

  return(list(geolayers_element))
}


#' publish a sp object to geoserver with multiple wms stlying information
#'
#' @param spobj a sp object. *IMPORTANT: Each column of spobj MUST NOT have attribute name exceeding length (int)10, failed to do so may result in NO Style being generated on GeoServer*
#' @param layerprefix a prefix for layer name, will make the generated data layers in GeoServer more identifiable
#' @param styleprefix a prefix for style name, will make the generated styles in GeoServer more identifiable
#' @param displayname_vec a vector of readable display names for data layer
#' @param attrname_vec a vector of attribute names that the style will be created for
#' @param palettename_vec a vector of palette names. check colors defined under each palette name at http://colorbrewer2.org
#'  acceptable color PaletteNames are: YlOrRd: 9 colors PRGn: 11 colors
#'  PuOr:11 colors RdGy: 11 colors Spectral: 11 colors Grays: 9 colors PuBuGn: 9 colors RdPu: 9 colors BuPu: 9 colors YlOrBr: 9 colors Greens: 9 colors
#'  BuGn: 9 colors Accents: 8 colors GnBu: 9 colors PuRd: 9 colors Purples: 9 colors RdYlGn: 11 colors Paired: 12 colors Blues: 9 colors RdBu: 11 colors
#'  Oranges: 9 colors RdYlBu: 11 colors PuBu: 9 colors OrRd: 9 colors Set3: 12 colors Set2: 8 colors Set1: 9 colors Reds: 9 colors PiYG: 11 colors
#'  Dark2: 8 colors YlGn: 9 colors BrBG: 11 colors YlGnBu: 9 colors
#'  Pastel2: 8 colors Pastel1: 9 colors
#' @param colorreverseorder_vec a vector of color reverse order. true or false(default), whether to reverse the color order defined in a palette
#' @param colornum_vec a vector of the number of colors to be classified. default 5.  in range of 2-15
#' @param classifier_vec a vector of classification function name. acceptable classifier function names are: Jenks(default), EqualInterval, Quantile, StandardDeviation
#' @param bordercolor_vec a vector of border color for polygon layers, default "black", can be white, gray or black.
#' @param borderwidth_vec a vector of border stroke width (integer value) for polygon layers. in range [1, 10], default 1
#' @param bordervisible_vec a vector of border visibility for polygon layers. TRUE/FALSE, default, TRUE
#' @param styletype_vec a vector of styling type, default "single"
#
#' @return a list elements to be included in geolayers
#' @export
dt_publishSP2GeoServerWithMultiStyles <- function(spobj,
                                                  layerprefix = "my_",
                                                  styleprefix = "my_stl_",
                                                  attrname_vec = c(""),
                                                  layerdisplyname_vec = c(""),
                                                  palettename_vec = c("Reds"),
                                                  colorreverseorder_vec = c(FALSE),
                                                  colornum_vec = c(5),
                                                  classifier_vec = c("Jenks"),
                                                  bordercolor_vec = c("black"),
                                                  borderwidth_vec = c(1),
                                                  bordervisible_vec = c(TRUE),
                                                  styletype_vec = c("single")) {
  # check data
  if (length(attrname_vec) == 0) {
    dt_debugprint("length of attrname_vec should not be 0")
    return(NULL)
  }
  # get the number of styles
  n_style <- length(attrname_vec)

  # check the length of other parameter vectors, fill any missing elements by duplicating the value of first element
  n_vec <- length(palettename_vec)
  if (n_vec < n_style) {
    for (i in 1:(n_style - n_vec)) {
      palettename_vec <- c(palettename_vec, palettename_vec[1])
    }
  }

  n_vec <- length(colorreverseorder_vec)
  if (n_vec < n_style) {
    for (i in 1:(n_style - n_vec)) {
      colorreverseorder_vec <- c(colorreverseorder_vec, colorreverseorder_vec[1])
    }
  }

  n_vec <- length(colornum_vec)
  if (n_vec < n_style) {
    for (i in 1:(n_style - n_vec)) {
      colornum_vec <- c(colornum_vec, colornum_vec[1])
    }
  }

  n_vec <- length(classifier_vec)
  if (n_vec < n_style) {
    for (i in 1:(n_style - n_vec)) {
      classifier_vec <- c(classifier_vec, classifier_vec[1])
    }
  }

  n_vec <- length(bordercolor_vec)
  if (n_vec < n_style) {
    for (i in 1:(n_style - n_vec)) {
      bordercolor_vec <- c(bordercolor_vec, bordercolor_vec[1])
    }
  }

  n_vec <- length(borderwidth_vec)
  if (n_vec < n_style) {
    for (i in 1:(n_style - n_vec)) {
      borderwidth_vec <- c(borderwidth_vec, borderwidth_vec[1])
    }
  }

  n_vec <- length(bordervisible_vec)
  if (n_vec < n_style) {
    for (i in 1:(n_style - n_vec)) {
      bordervisible_vec <- c(bordervisible_vec, bordervisible_vec[1])
    }
  }

  n_vec <- length(styletype_vec)
  if (n_vec < n_style) {
    for (i in 1:(n_style - n_vec)) {
      styletype_vec <- c(styletype_vec, styletype_vec[1])
    }
  }

  # TODO: this is the place to add any additional column (i.e. normalised values) to spobj

  # make sure spobj is projected in EPSG:4326 (WGS84) before publishing it.
  spobj <- spTransform(spobj, CRS(proj4string_epsg4326))

  # check if any colname has over 10 character
  oldnames <- colnames(spobj@data)
  newnames <- colnames(spobj@data)
  lengthlimit <- 10
  for (i in 1:length(newnames)) {
    if (nchar(newnames[i]) > lengthlimit) { # if longer than lengthlimit, trim it
      newnames[i] <- substr(newnames[i], 1, lengthlimit)
    }
  }
  # further check if there is any duplicate attribute name after trimming
  autonum <- 1
  prevdupidx <- 0
  while (anyDuplicated(newnames) > 0) {
    dupidx <- anyDuplicated(newnames)
    if (dupidx == prevdupidx) {
      autonum <- autonum + 1
      if (autonum > 10) {
        autonum <- 1
      }
    }

    newnames[dupidx] <- sprintf("%s%i", substr(newnames[dupidx], 1, nchar(newnames[dupidx]) - 1), autonum)
    prevdupidx <- dupidx
  }

  # set trimmed colname for sp obj
  colnames(spobj@data) <- newnames

  # check if any attrname in attrname_vec is trimmed or renamed, update to the new name before creating styles
  for (i in 1:length(attrname_vec)) {
    for (j in 1:length(oldnames)) {
      if (attrname_vec[i] == oldnames[j]) {
        attrname_vec[i] <- newnames[j]
      }
    }
  }


  # publish sp to geoserver
  outputWfsUrl <- dt_publishSP2GeoServer(spobj, layerprefix)
  if (is.null(outputWfsUrl)) {
    dt_debugprint("error occurs in dt_publishSP2GeoServer method, try it again")
    return(NULL)
  }

  # loop all styles to be created
  geolayers_elements <- list()

  for (i in 1:n_style) {
    attrname <- attrname_vec[i]
    palettename <- palettename_vec[i]
    colorreverseorder <- colorreverseorder_vec[i]
    colornum <- colornum_vec[i]
    classifier <- classifier_vec[i]
    bordercolor <- bordercolor_vec[i]
    borderwidth <- borderwidth_vec[i]
    bordervisible <- bordervisible_vec[i]
    styletype <- styletype_vec[i]

    # calculte wms style
    wmsStyleResult <- fromJSON(dt_createWMSStyle(
      wfsurl = outputWfsUrl,
      styleprefix = styleprefix,
      attrname = attrname,
      palettename = palettename,
      colorreverseorder = colorreverseorder,
      colornum = colornum,
      classifier = classifier,
      bordercolor = bordercolor,
      borderwidth = borderwidth,
      bordervisible = bordervisible,
      styletype = styletype
    ))

    wmsStyleparams <- list()
    if (wmsStyleResult$status == 0) {
      wmsStyleparams <- wmsStyleResult$data
    }


    geolayers_element <- list(
      layername = dt_getLayerNameFromWFSUrl(wfsurl = outputWfsUrl),
      layerdisplayname = layerdisplyname_vec[i],
      bbox = dt_getBbox(spobj),
      wfs = list(url = outputWfsUrl, styleparams = list()),
      wms = list(url = globalGSCredentials["wmsUrlTemplate"][[1]], styleparams = wmsStyleparams)
    )

    geolayers_elements <- append(geolayers_elements, list(geolayers_element))
  }

  return(geolayers_elements)
}



#
#' publish a sp object to geoserver with wms stlying information
#'
#' @param spobj a sp object. *IMPORTANT: Each column of spobj MUST NOT have attribute name exceeding length (int)10, failed to do so may result in NO Style being generated on GeoServer*
#' @param layerprefix a prefix for layer name, will make the generated data layers in GeoServer more identifiable
#' @param displayname a readable display name for data layer
#
#' @return a list contains single element to be included in geolayers
#' @export
dt_publishSP2GeoServerWithName <- function(spobj,
                                           layerprefix = "my_stl_",
                                           layerdisplyname = "") {

  # TODO: this is the place to add any additional column (i.e. normalised values) to spobj

  # make sure spobj is projected in EPSG:4326 (WGS84) before publishing it.
  spobj <- spTransform(spobj, CRS(proj4string_epsg4326))

  # publish sp to geoserver
  outputWfsUrl <- dt_publishSP2GeoServer(spobj, layerprefix)
  if (is.null(outputWfsUrl)) {
    return(NULL)
  }

  # create content of the 1st element

  geolayers_element <- list(
    layername = dt_getLayerNameFromWFSUrl(wfsurl = outputWfsUrl),
    layerdisplayname = layerdisplyname,
    bbox = dt_getBbox(spobj),
    wfs = list(url = outputWfsUrl, styleparams = list()),
    wms = list(url = globalGSCredentials["wmsUrlTemplate"][[1]])
  )

  return(geolayers_element)
}



#' create a featuretype in geoserver datastore, which makes the uploaded shpfile 'published'
#'
#' @param filename
#'
#' @return empty string if success or error message
#' @export
dt_createFeatureType <- function(filename) {
  ftContentRaw <- dt_getFeatureType()

  if (is.null(ftContentRaw)) {
    return("fail to load FeatureType")
  }

  ftContentXML <- xmlInternalTreeParse(ftContentRaw)

  if (is.null(ftContentXML)) {
    return("fail to load FeatureType")
  }

  isExisting <- (length(getNodeSet(ftContentXML, sprintf("/featureTypes/featureType[name='%s']", filename))) > 0)

  # dt_debugprint(sprintf("new FeatureType: %s",filename))
  # dt_debugprint(sprintf("current FeatureTypeContent: %s",ftContentRaw))

  # if workspace already exists, do nothing
  if (isExisting) {
    dt_debugprint(sprintf("FeatureType: %s already exists, skip creation", filename))
    return("")
  }


  url <- sprintf(
    "%s/rest/workspaces/%s/datastores/%s/featuretypes.xml",
    globalGSCredentials$gsRESTURL,
    globalGSCredentials$gsWORKSPACENAME,
    globalGSCredentials$gsDATASTORESNAME
  )

  body <- sprintf(
    "<featureType><enabled>true</enabled><metadata /><keywords /><metadataLinks /><attributes /><name>%s</name><title>%s</title><srs>EPSG:4326</srs><projectionPolicy>FORCE_DECLARED</projectionPolicy></featureType>",
    filename,
    filename
  )

  con <- POST(url, timeout(36000), authenticate(globalGSCredentials$gsRESTUSER, globalGSCredentials$gsRESTPW), accept("application/xml"), content_type("application/xml"), body = body)

  if (con$status_code != 201) {
    dt_debugprint(sprintf("fail to create featuretype: %s", filename))
    dt_debugprint(sprintf("response code: %i", con$status_code))
    dt_debugprint(sprintf("response content: %s", content(con, "text")))

    return("")
  }

  dt_debugprint(sprintf("featuretype: %s created", filename))
  return("")
}

#' get all featuretypes defined in the given datastore of the given workspace in geoserver (workspace and datastore are defined the globalGSCredentials)
#'
#' @return xml string if success or empty string
#' @export
dt_getFeatureType <- function() {
  url <- sprintf(
    "%s/rest/workspaces/%s/datastores/%s/featuretypes.xml",
    globalGSCredentials$gsRESTURL,
    globalGSCredentials$gsWORKSPACENAME,
    globalGSCredentials$gsDATASTORESNAME
  )

  con <- GET(url, timeout(36000), authenticate(globalGSCredentials$gsRESTUSER, globalGSCredentials$gsRESTPW), accept("application/xml"), content_type("application/xml"))

  if (con$status_code != 200) {
    return(NULL)
  }
  return(content(con, "text"))
}

#' create a new workspace
#'
#' @param wsname workspace name
#'
#' @return empty string if success or error message
#' @export
dt_createWorkspace <- function(wsname) {
  wsContentRaw <- dt_getWorkspace()

  if (is.null(wsContentRaw)) {
    return("fail to load workspaces")
  }

  wsContentXML <- xmlInternalTreeParse(wsContentRaw)

  if (is.null(wsContentXML)) {
    return("fail to load workspaces")
  }

  isExisting <- (length(getNodeSet(wsContentXML, sprintf("/workspaces/workspace[name='%s']", wsname))) > 0)

  # if workspace already exists, do nothing
  if (isExisting) {
    dt_debugprint(sprintf("workspace: %s already exists, skip creation", wsname))
    return()
  }


  url <- sprintf("%s/rest/workspaces", globalGSCredentials$gsRESTURL)

  body <- sprintf("<workspace><name>%s</name></workspace>", wsname)

  con <- POST(url, timeout(36000), authenticate(globalGSCredentials$gsRESTUSER, globalGSCredentials$gsRESTPW), accept("application/xml"), content_type("application/xml"), body = body)

  if (con$status_code != 201) {
    dt_debugprint(sprintf("fail to create workspace: %s", wsname))
    return()
  }

  dt_debugprint(sprintf("workspace: %s created", wsname))
  return()
}


#' get all workspaces
#'
#' @return empty string if success or error message
#' @export
dt_getWorkspace <- function() {
  url <- sprintf("%s/rest/workspaces", globalGSCredentials$gsRESTURL)

  con <- GET(url, timeout(36000), authenticate(globalGSCredentials$gsRESTUSER, globalGSCredentials$gsRESTPW), accept("application/xml"), content_type("application/xml"))

  if (con$status_code != 200) {
    return(NULL)
  }
  return(content(con, "text"))
}

#' upload a shpfile (zip) to geoserver
#'
#' @param filepath
#'
#' @return empty string if success or error message
#' @export
dt_addShp2DataStore <- function(filepath) {

  # create workspace if it doesn't exist
  dt_createWorkspace(globalGSCredentials$gsWORKSPACENAME)

  url <- sprintf(
    "%s/rest/workspaces/%s/datastores/%s/file.shp",
    globalGSCredentials$gsRESTURL,
    globalGSCredentials$gsWORKSPACENAME,
    globalGSCredentials$gsDATASTORESNAME
  )

  con <- PUT(url, timeout(36000), authenticate(globalGSCredentials$gsRESTUSER, globalGSCredentials$gsRESTPW), accept("*/*"), content_type("application/zip"), body = upload_file(filepath))

  if (con$status_code != 201) {
    dt_debugprint(sprintf("fail to upload shpfile: %s", filepath))
    return("")
  }

  dt_debugprint(sprintf("shpfile: %s uploaded", filepath))
  return("")
}


#
#' create wms style parameters for a given wfs datalayer
#'
#' @param wfsurl wfs url for the data layer
#' @param attrname the attribute name that the style will be created for
#' @param styleprefix a prefix for style name, will make the generated styles in GeoServer more identifiable
#' @param palettename check colors defined under each palette name at http://colorbrewer2.org
#'  acceptable color PaletteNames are: YlOrRd: 9 colors PRGn: 11 colors
#'  PuOr:11 colors RdGy: 11 colors Spectral: 11 colors Grays: 9 colors PuBuGn: 9 colors RdPu: 9 colors BuPu: 9 colors YlOrBr: 9 colors Greens: 9 colors
#'  BuGn: 9 colors Accents: 8 colors GnBu: 9 colors PuRd: 9 colors Purples: 9 colors RdYlGn: 11 colors Paired: 12 colors Blues: 9 colors RdBu: 11 colors
#'  Oranges: 9 colors RdYlBu: 11 colors PuBu: 9 colors OrRd: 9 colors Set3: 12 colors Set2: 8 colors Set1: 9 colors Reds: 9 colors PiYG: 11 colors
#'  Dark2: 8 colors YlGn: 9 colors BrBG: 11 colors YlGnBu: 9 colors
#'  Pastel2: 8 colors Pastel1: 9 colors
#'
#' @param colorreverseorder true or false(default), whether to reverse the color order defined in a palette
#' @param colornum the number of colors to be classified. default 5.  in range of 2-15
#' @param classifier acceptable classifier function names are: Jenks(default), EqualInterval, Quantile, StandardDeviation
#' @param bordercolor border color for polygon layers, default "black", can be white, gray or black.
#' @param borderwidth border stroke width (integer value) for polygon layers. in range [1, 10], default 1
#' @param bordervisible border visibility for polygon layers. TRUE/FALSE, default, TRUE
#' @param styletype is styling typle, default "single"
#'
#' @return
#' @export
dt_createWMSStyle <- function(wfsurl, styleprefix = "my_stl_", attrname, palettename = "Reds", colorreverseorder = FALSE, colornum = 5, classifier = "Jenks", bordercolor = "black", borderwidth = 1, bordervisible = TRUE, styletype = "single") {
  wfsurl <- sprintf("%s%s%s", wfsurl, "&propertyName=", attrname)

  createStyleUrl <- sprintf("%s%s%s", WMSStyleCreateUrl, "?attrname=", attrname)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&styleprefix=", styleprefix)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&palettename=", palettename)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&colorreverseorder=", colorreverseorder)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&colornum=", colornum)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&classifier=", classifier)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&bordercolor=", bordercolor)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&borderwidth=", borderwidth)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&bordervisible=", bordervisible)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&styletype=", styletype)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&url=", URLencode(wfsurl, reserved = TRUE))
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&gsresturl=", URLencode(globalGSCredentials$gsRESTURL, reserved = TRUE))
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&gsusername=", globalGSCredentials$gsRESTUSER)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&gspassword=", globalGSCredentials$gsRESTPW)
  createStyleUrl <- sprintf("%s%s%s", createStyleUrl, "&gsws=", globalGSCredentials$gsWORKSPACENAME)


  con <- GET(createStyleUrl, timeout(36000))
  if (con$status_code != 200) {
    return("{\"status\":1}")
  }
  return(content(con, "text"))
}


#' find utm zone for given longitude
#'
#' @param long Longitude
#'
#' @return UTM zone code
#' @export
dt_long2UTM <- function(long) {
  (floor((long + 180) / 6) %% 60) + 1
}


#' convert data.frame to json list structure
#'
#' @param df
#'
#' @return list
#' @export
dt_df2jsonlist <- function(df) {
  result <- list()
  for (i in 1:nrow(df)) {
    rowlist <- list()
    for (attrname in colnames(df)) {
      if (is.factor(df[i, attrname])) {
        rowlist[attrname] <- as.character(df[i, attrname])
      }
      else {
        rowlist[attrname] <- df[i, attrname]
      }
    }
    result[[i]] <- rowlist
  }
  return(result)
}


#' get layname from wfs url
#'
#' @return
#' @export
dt_getLayerNameFromWFSUrl <- function(wfsurl) {
  result <- ""
  components <- strsplit(wfsurl, "&")[[1]]

  for (i in 1:length(components)) {
    if (startsWith(components[i], "typeName")) {
      result <- strsplit(components[i], "=")[[1]][2]
      break
    }
  }

  return(result)
}


#' get boundingbox of a sp object
#'
#' @param spobj
#'
#' @return list(minX, minY, maxX, maxY)
#' @export
dt_getBbox <- function(spobj) {
  return(list(spobj@bbox[1, 1], spobj@bbox[2, 1], spobj@bbox[1, 2], spobj@bbox[2, 2]))
}


#' reproject(transform) a sp object to a proper utm crs
#'
#' @param spobj
#'
#' @return a reprojected sp object
#' @export
dt_project2UTM <- function(spobj) {

  # get bbox of sp
  bbox <- dt_getBbox(spobj)

  # find the centre x and y
  centreX <- (bbox[[1]] + bbox[[3]]) / 2.0
  centreY <- (bbox[[2]] + bbox[[4]]) / 2.0

  # determie the utm zone number by centre y
  utmzone <- dt_long2UTM(centreX)

  # construct a valid proj4string_utm
  proj4string_utm <- ""
  if (centreY > 0) # in the Northern Hemisphere
    {
      proj4string_utm <- sprintf(proj4string_utm_template, utmzone, "")
    } else {
    proj4string_utm <- sprintf(proj4string_utm_template, utmzone, "+south ")
  }

  return(spTransform(spobj, CRS(proj4string_utm)))
}

#' reproject(transform) a sp object to a WGS84 (EPSG:4326)
#'
#' @param spobj
#'
#' @return a reprojected sp object
#' @export
dt_project2WGS84 <- function(spobj) {
  return(spTransform(spobj, CRS(proj4string_epsg4326)))
}


#' get the geometry type of sp object
#'
#' @param spobj
#'
#' @return geomtype, one of Polygon, LineString, Point
#' @export
dt_getGeomType <- function(spobj) {
  spobjClass <- tolower(class(spobj)[1])

  if (regexpr("polygon", spobjClass)[1] > 0) {
    return("Polygon")
  } else if (regexpr("line", spobjClass)[1] > 0) {
    return("LineString")
  } else if (regexpr("point", spobjClass)[1] > 0) {
    return("Point")
  } else {
    return("Geometry")
  }
}

#' get GeoServer Credentials from server and initialise variables for current devkey
#'
#' @return
#' @export
dt_initGeoServerCredentials <- function(dk) {

  # assign dk to devkey
  devkey <<- dk

  # get raw JSON string
  url <- sprintf("%s%s", credUrl, devkey)
  con <- GET(url, timeout(36000))
  if (con$status_code != 200) {
    return(NULL)
  }
  rawEncryptedJSON <- content(con, "text")

  rawEncryptedObj <- fromJSON(rawEncryptedJSON)



  # decode data from base64 string
  base64DecodedCredentials <- base64decode(rawEncryptedObj$data)

  # generate the key for decryption
  key <- charToRaw(substr(devkey, 1, 16))
  aes <- AES(key, mode = "ECB")

  # decrypt GeoServer credentials as plain text
  decryptedCredentialsJSON <- aes$decrypt(base64DecodedCredentials, raw = FALSE)

  # find the position of last }, there might be some trailing garbage in the end of the string
  g <- regexpr("\\}[^\\}]*$", decryptedCredentialsJSON)
  decryptedCredentialsJSON <- substr(decryptedCredentialsJSON, 1, g[1])

  # assign it to globalGSCredentials
  globalGSCredentials <<- fromJSON(decryptedCredentialsJSON)

  # add wmsUrlTemplate
  globalGSCredentials["wmsUrlTemplate"] <<- sprintf("%s/%s/wms", globalGSCredentials$gsRESTURL, globalGSCredentials$gsWORKSPACENAME)

  # overwrite tempDirPath, always put the tmpdata within the same folder of script
  globalGSCredentials["tempDirPath"] <<- sprintf("%s/tmpdata", getwd())
}

#' sync and update job outputs
#'
#' @param joboutputs
#' @param jobsuccess true or false
#' @param jobuuid
#'
#' @return
#' @export
dt_updateJob <- function(joboutputs, jobsuccess, jobuuid) {
  if (tolower(trimws(jobuuid) == "fake-job-uuid")) {
    dt_debugprint("fake-job-uuid provided for testing. updateJob call skipped.")
  } else {
    url <- jobUpdateUrl
    body <- toJSON(list(joboutputs = joboutputs, jobsuccess = jobsuccess, jobuuid = jobuuid, devkey = devkey), auto_unbox = TRUE)
    dt_debugprint(body)
    con <- POST(url, timeout(36000), body = body, encode = "raw")
    content(con, "text")


    if (con$status_code != 200) {
      dt_debugprint(sprintf("fail to update job: %s, errmsg: %s", jobuuid, content(con, "text")))
    } else {
      dt_debugprint(sprintf("job: %s updated", jobuuid))
    }
  }

  return()
}


#' publish a geotiff to geoserver
#'
#' @param filepath
#' @param datalayername
#'
#' @return if success, a published datalayername; otherwise, empty string
#' @export
dt_publishTiffDataLayer <- function(filepath, datalayername = UUIDgenerate(FALSE)) {
  file <- file(filepath, "rb")
  on.exit(close(file))


  # make the raster store same name as the data layer
  url <- sprintf(
    "%s/rest/workspaces/%s/coveragestores/%s_tiff/file.geotiff?configure=false&coverageName=%s",
    globalGSCredentials$gsRESTURL,
    globalGSCredentials$gsWORKSPACENAME,
    datalayername,
    datalayername
  )

  con <- PUT(url, timeout(36000), authenticate(globalGSCredentials$gsRESTUSER, globalGSCredentials$gsRESTPW), accept("*/*"), content_type("geotif/geotiff"), body = upload_file(filepath))

  if (con$status_code != 201) {
    dt_debugprint(sprintf("fail to upload tiff file: %s", filepath))
    return("")
  }

  return(sprintf("%s:%s", globalGSCredentials$gsWORKSPACENAME, datalayername))
}


#' add a default style (precalculalted and stored in geoserver) for a layer (vector or tiff)
#'
#' @param stylename style name (without workspace infomation)
#' @param datalayername datalyer name, workspace information will be automatically attached based on devkey account
#' @param isglobalstyle if TRUE, workspace information will not be attached. set this to TRUE if the style is hard coded in Geoserver. This is useful for creating common styles (such as shadow intensity, rainfall and temperature) at the application/built-in model level, rather than individual job level
#' @return if success, a published datalayername; otherwise, empty string
#' @export
dt_addDefaultStyleToDataLayer <- function(stylename, datalayername, isglobalstyle = FALSE) {

  # ref https://boundlessgeo.com/2012/10/adding-layers-to-geoserver-using-the-rest-api/
  # ref https://gis.stackexchange.com/questions/94313/how-to-set-default-style-of-layer-using-rest-api-in-geoserver

  defaultStyle <- ""
  if (isglobalstyle) {
    defaultStyle <- sprintf("<layer><defaultStyle><name>%s</name></defaultStyle></layer>", stylename)
  } else {
    defaultStyle <- sprintf("<layer><defaultStyle><name>%s:%s</name></defaultStyle></layer>", globalGSCredentials$gsWORKSPACENAME, stylename)
  }

  # just in case the datalayer name has workspace part
  rawlayername <- unlist(strsplit(datalayername, ":"))
  if (length(rawlayername) > 1 && rawlayername[1] == globalGSCredentials$gsWORKSPACENAME) {
    datalayername <- rawlayername[2]
  }

  url <- sprintf(
    "%s/rest/layers/%s:%s",
    globalGSCredentials$gsRESTURL,
    globalGSCredentials$gsWORKSPACENAME,
    datalayername
  )

  con <- PUT(url, timeout(36000), authenticate(globalGSCredentials$gsRESTUSER, globalGSCredentials$gsRESTPW), accept("*/*"), content_type("application/xml"), body = defaultStyle)

  if (con$status_code != 200) {
    dt_debugprint(sprintf("fail to add style %s to datalayer: %s", stylename, datalayername))
    return("")
  }

  return(sprintf("%s:%s", globalGSCredentials$gsWORKSPACENAME, datalayername))
}


#' delete a coverage store (related to current mydevkey) and all layers it contains on geoserver (a bit dangerous)
#'
#' @return if success tiff store name; otherwise empty string
#' @export
dt_deleteTiffDataStore <- function() {
  url <- sprintf(
    "%s/rest/workspaces/%s/coveragestores/%s_tiff?recurse=true",
    globalGSCredentials$gsRESTURL,
    globalGSCredentials$gsWORKSPACENAME,
    globalGSCredentials$gsDATASTORESNAME
  )

  con <- DELETE(url, timeout(36000), authenticate(globalGSCredentials$gsRESTUSER, globalGSCredentials$gsRESTPW), accept("*/*"))

  if (con$status_code != 200) {
    dt_debugprint(sprintf("fail to delete tiff store: %s_tiff", globalGSCredentials$gsDATASTORESNAME))
    return("")
  }

  return(sprintf("%s_tiff", globalGSCredentials$gsDATASTORESNAME))
}

#' delete a coverage layer based on layername
#'
#' @return if success, return the deleted datalayername; otherwise, empty string
#' @export
dt_deleteTiffDataLayer <- function(datalayername) {
  url <- sprintf(
    "%s/rest/workspaces/%s/coveragestores/%s_tiff/coverages/%s?recurse=true",
    globalGSCredentials$gsRESTURL,
    globalGSCredentials$gsWORKSPACENAME,
    globalGSCredentials$gsDATASTORESNAME,
    datalayername
  )

  con <- DELETE(url, timeout(36000), authenticate(globalGSCredentials$gsRESTUSER, globalGSCredentials$gsRESTPW), accept("*/*"))

  if (con$status_code != 200) {
    dt_debugprint(sprintf("fail to delete tiff layer: %s_tiff", datalayername))
    return("")
  }

  return(sprintf("%s:%s", globalGSCredentials$gsWORKSPACENAME, datalayername))
}
