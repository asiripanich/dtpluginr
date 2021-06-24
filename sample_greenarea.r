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
# 
# sample datasets wfs url:
# (1)greenarea: https://ds2.digitwin.com.au:8443/geoserver/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=livedata:greenspace_psma2019_sa4_melbourne_inner&outputFormat=json
# 
# (2)pop (Melbourne sa1): https://ds2.digitwin.com.au:8443/geoserver/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=livedata:population_abs2016_sa1_melbourne_inner&outputFormat=json
#
# DevLogs:
#

# Run a test (make sure you get a vaild devKey and all prerequisites are met as described in the README of https://bitbucket.org/digitwin/dt-plugin-r/):
#
# Rscript E:\01_UniProjects\Digitwin\SourceCodeRepos\dt-plugin-r\sample_greenarea.R "fake-job-uuid" "https://ds2.digitwin.com.au:8443/geoserver/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=livedata:greenspace_psma2019_sa4_melbourne_inner&outputFormat=json" "https://ds2.digitwin.com.au:8443/geoserver/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=livedata:population_abs2016_sa1_melbourne_inner&outputFormat=json" 100000

# v2.1 2021-06-24
# (1) update test data wfs urls and utils.publishSP2GeoServerWithMultiStyles parameters

# v2.0 2019-08-30
# (1) update script with new utils funtions 

# v1.2 2018-07-18
# (1) add layerprefix and styleprefix for the outputs 

# v1.1 2018-07-15
# (1) update urls 

# v1.0 2017-07-18
# (1) init commitment 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(maptools) 
library(rgdal)
library(rgeos)
library(jsonlite)
library(doParallel)

# using 4 cores for parallel computing
registerDoParallel(cores=4) 

#### DO NOT CHNAGE/DELETE THIS FUNCTION
LocationOfThisScript = function() # Function LocationOfThisScript returns the location of this .R script (may be needed to source other files in same dir)
{
  this.file = NULL
  # This file may be 'sourced'
  for (i in -(1:sys.nframe())) {
    if (identical(sys.function(i), base::source)) this.file = (normalizePath(sys.frame(i)$ofile))
  }
  
  if (!is.null(this.file)) return(dirname(this.file))
  
  # But it may also be called from the command line
  cmd.args = commandArgs(trailingOnly = FALSE)
  cmd.args.trailing = commandArgs(trailingOnly = TRUE)
  cmd.args = cmd.args[seq.int(from=1, length.out=length(cmd.args) - length(cmd.args.trailing))]
  res = gsub("^(?:--file=(.*)|.*)$", "\\1", cmd.args)
  
  # If multiple --file arguments are given, R uses the last one
  res = tail(res[res != ""], 1)
  if (0 < length(res)) return(dirname(res))
  
  # Both are not the case.
  return(NULL)
}


#### DO NOT CHNAGE/DELETE THIS FUNCTION
setwd(LocationOfThisScript())

# ATTENTION:
# devKey is used for storing and publishing plugin outputs in Digitwin GeoServers so that the outputs can be viewed, used, downloaded by others
# To obtain a devKey for Digitwin plugin development, please contact UoM Digitwin dev team.

myDevKey = "" # DO NOT CHANGE THIS VARIABLE NAME

# this the main wrapper method which handles the arguments check and call the plugin calculation method
# to trigger this in cmd line, just run : RScript path\sample_code.R "arg1" "arg2"


# load utils methods for use
source("utils.R")


# calcuate green area index for city of melbourne using meshblock population
execIndicatorGreenArea <- function(jobuuid, greenarea_wfsurl, pop_wfsurl, pop_basenum){
  
  # check if myDevKey is set
  if(nchar(myDevKey)==0){
    utils.debugprint("devKey is not provided.")
    return(FALSE)
  }
  
  # ATTENTION: this function MUST be called first before calling any other utils functions
  utils.initGeoServerCredentials(myDevKey)
  
  
  utils.debugprint(getwd())
  # insert input parameter check logic here. make sure to convert data into right data type since by default the inputs are all treated as strings
  utils.debugprint(sprintf("para1: %s", jobuuid))
  utils.debugprint(sprintf("para2: %s", URLdecode(greenarea_wfsurl)))
  utils.debugprint(sprintf("para3: %s", URLdecode(pop_wfsurl)))
  utils.debugprint(sprintf("para3: %i", as.integer(pop_basenum))) # convert string to int
  
  # the follow two lines are for testing
  #greenarea_wfsurl = "https://ds2.digitwin.com.au:8443/geoserver/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=livedata:greenspace_psma2019_sa4_melbourne_inner&outputFormat=json"
  #pop_wfsurl = "https://ds2.digitwin.com.au:8443/geoserver/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=livedata:population_abs2016_sa1_melbourne_inner&outputFormat=json"
  
  # load spatial object direct from geojson
  sp_greenarea = utils.loadGeoJSON2SP(URLdecode(greenarea_wfsurl))
  # check if data layer can be successfully loaded
  if(is.null(sp_greenarea)){
    utils.debugprint("fail to load data layer for greenarea")
    utils.updateJob(list(message="fail to load data layer for greenarea"), FALSE, jobuuid)
    return(FALSE)
  }
  
  sp_pop = utils.loadGeoJSON2SP(URLdecode(pop_wfsurl))
  # check if data layer can be successfully loaded
  if(is.null(sp_pop)){
    utils.debugprint("fail to load data layer for population")
    utils.updateJob(list(message="fail to load data layer for population"), FALSE, jobuuid)
    return(FALSE)
  }
  
  # green area index calculation base is for 100,000 persons
  pop_basenum = as.integer(pop_basenum)
  if(pop_basenum<0|| pop_basenum > 99999999) pop_basenum = 100000
  
  # # # # # # # # # # # # # 
  # implement indicator logic here, such as
  # 1. spatial data operations like projection, intersection, union, 
  # 2. statistics generation
  # 3. etc.
  # # # # # # # # # # # # # 
  
  # project(transform) sp into UTM to enable area calculation
  sp_greenarea_prj = utils.project2UTM(sp_greenarea)
  
  # # # # # # # # # # # # # # # # # # # # # # 
  # calculation greenarea for each population polygon 
  # # # # # # # # # # # # # # # # # # # # # # 
  
  # project(transform) sp into UTM to enable area calculation, since the orginal population polygin in WGS84 coordinate reference system
  sp_pop_prj = utils.project2UTM(sp_pop)
  
  # add two more attributes for sp_pop_prj, one is for the actual size of greenarea, the other is for the greenarea index
  sp_pop_prj@data[,"gaarea"] = 0.0
  sp_pop_prj@data[,"idxval"] = 0.0
  
  # ==== main loop starts here ====
  # for each population polygon, find all green areas it intersects and calcuate the size of intersected area
  # two methods are implemented:
    
  # ===================================
  # method 1 : using parallel computing
  # ===================================
  
  # bulid foreach result as a matrix containing 3 columns storing values for gaarea, idxval and mb_code11 respectively
  result = foreach(i=1:nrow(sp_pop_prj), .combine = rbind, .export=c("SpatialPolygons","over","gIntersection","gArea","gIsValid")) %dopar% {

    # get the geometry polgyon of population, return 0 for gaarea and idxval if geometry is NULL
    if(is.null(sp_pop_prj@polygons[i])){
      out = c(0, 0)
    }else{

      geom_pop = SpatialPolygons(sp_pop_prj@polygons[i], proj4string=sp_pop_prj@proj4string)

      # accumulate the total size of intersected greenarea for the current population geometry
      intersectedGreenArea = 0.0

      # this 'over' method is much faster to find all intersected green area polygons of current pop polygon
      # temporarily save all intersected greenarea into a sub spatialdataframe
      intersectedGADF = sp_greenarea_prj[!is.na(over(sp_greenarea_prj,sp_pop_prj[i,]))[,1],]

      # if intersected with one or more greenarea polygon, calculate and accumulate the intersected area for each population meshblock
      if(nrow(intersectedGADF)>0){

        for(j in nrow(intersectedGADF):1){

          geom_greenarea = SpatialPolygons(intersectedGADF@polygons[j], proj4string=intersectedGADF@proj4string)
          if(!gIsValid(geom_greenarea)) next
          
          # do the actual intersction process
          intsectedGeom = gIntersection(geom_pop, geom_greenarea)
          # accumulate the size of intersected greenarea
          intersectedGreenArea = intersectedGreenArea + gArea(intsectedGeom)

        }
      }

      # check population attribute, make sure it is valid
      population = sp_pop_prj@data[i,"persons"]

      if(is.null(population)||is.na(population)) population=0

      # for those polygons with 0 population, assign idxval = 0
      idx_val = 0
      if(population>0){
        idx_val = intersectedGreenArea / (population / (pop_basenum * 1.0))
      }

      out = c(intersectedGreenArea, idx_val)
    }
  }

  # assign calculated values back to sp_pop_prj@data. use as.numberic() to assure the values are numeric
  sp_pop_prj@data[,"gaarea"] = as.numeric(result[,1])
  sp_pop_prj@data[,"idxval"] = as.numeric(result[,2])
  
  # ===================================
  # method 2: using normal for loop
  # ===================================
  
  # this process takes long time to accomplish. 
  # in RStudio, use Ctrl+Shift+C to uncomment/comment it for testing
  
  
  # for(i in nrow(sp_pop_prj):1){
  # 
  #   utils.debugprint(sprintf("processing [%i/%i]", i, nrow(sp_pop_prj)))
  # 
  #   # get the geometry polgyon of population, skip if it is NULL
  #   if(is.null(sp_pop_prj@polygons[i])){
  #     next
  #   }
  #   geom_pop = SpatialPolygons(sp_pop_prj@polygons[i], proj4string=sp_pop_prj@proj4string)
  # 
  #   # accumulate the total size of intersected greenarea for the current population geometry
  #   intersectedGreenArea = 0.0
  # 
  #   # this 'over' method is much faster to find all intersected green area polygons of current pop polygon
  #   # temporarily save all intersected greenarea into a sub spatialdataframe
  #   intersectedGADF = sp_greenarea_prj[!is.na(over(sp_greenarea_prj,sp_pop_prj[i,]))[,1],]
  # 
  #   # if intersected with one or more greenarea polygon, calculate and accumulate the intersected area for each population meshblock
  #   if(nrow(intersectedGADF)>0){
  # 
  #     for(j in nrow(intersectedGADF):1){
  # 
  #       geom_greenarea = SpatialPolygons(intersectedGADF@polygons[j], proj4string=intersectedGADF@proj4string)
  # 
  #       # do the actual intersction process
  #       intsectedGeom = gIntersection(geom_pop, geom_greenarea)
  #       # accumulate the size of intersected greenarea
  #       intersectedGreenArea = intersectedGreenArea + gArea(intsectedGeom)
  # 
  #     }
  #   }
  # 
  #   # check population attribute, make sure it is valid
  #   population = sp_pop_prj@data[i,"persons"]
  # 
  #   if(is.null(population)||is.na(population)) population=0
  # 
  #   # for those polygons with 0 population, assign idxval = 0
  #   if(population>0){
  #     sp_pop_prj@data[i,"idxval"] = intersectedGreenArea / (population / (pop_basenum * 1.0))
  #   }
  #   # assgin intersectedGreenArea to gaarea attribute
  #   sp_pop_prj@data[i,"gaarea"] = intersectedGreenArea
  # 
  # }
  
  
  # ==== main loop ends here ====
  
  # this example shows how to publish a geolayer by creating two wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  geolayers_gaindex = utils.publishSP2GeoServerWithMultiStyles(spobj=sp_pop_prj, 
                                                           layerprefix="greenarea_",
                                                           styleprefix="greenarea_stl_",
                                                           attrname_vec=c("gaarea","idxval"),
														                               layerdisplyname_vec=c("gaarea_gaarea","gaarea_idxval"),
                                                           palettename_vec=c("Reds","Blues"), 
                                                           colorreverseorder_vec=c(FALSE,FALSE), 
                                                           colornum_vec=c(6,8), 
                                                           classifier_vec=c("Jenks","Jenks"),
                                                           bordercolor_vec=c("black", "gray"),
                                                           borderwidth_vec=c(1, 2),
                                                           bordervisible_vec=c(TRUE, TRUE),
														                               styletype_vec=c("graduated", "graduated")
                                                          )
  
  if(is.null(geolayers_gaindex) || length(geolayers_gaindex)==0){
    utils.debugprint("fail to save data to geoserver")
    utils.updateJob(list(message="fail to save data to geoserver"), FALSE, jobuuid)
    return(FALSE)
  }
  
  # part 1.2: append the each element into geolayers list
  geolayers = list()
  geolayers = append(geolayers, geolayers_gaindex)
  
  tables_element1 = list(
    title="Your Table Title1",
    data = list(
      list(
        colname="column1",
        values=list(0,1,2,3,4,5)
      ),
      list(
        colname="column2",
        values=list("Azadeh","Sam","Benny","Soheil","Mohsen","Abbas")
      )
    )
  )
  # part 2.2: build the 2nd element
  tables_element2 = list(
    title="Your Table Title2",
    data = list(
      list(
        colname="id",
        values=list(10,11,12,13,14,15)
      ),
      list(
        colname="name",
        values=list("Yibo","Nick","Benny","Soheil","Erfan","Davood")
      )
    )
  )
  # part 2.3: put the 1st and 2nd element into tables
  tables = list(tables_element1, tables_element2)
  
  
  # part 3: build charts
  # part 3.1: build the 1st element
  # define a data frame for chart
  df1 = data.frame(month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), 
                   data1=runif(12, 5, 50),
                   data2=runif(12, 1, 20),
                   data3=runif(12, 60, 100),
                   data4=runif(12, 10, 30))
  
  charts_element1 = list(
    title="Your 1st Chart Title",
    type="columnchart",
    stacked=FALSE,
    xfield="month",
    yfield=list("data1", "data2", "data3", "data4"),
    yfieldtitle = list("IE", "Firefox", "Chrome", "Safari"),
    data=utils.df2jsonlist(df1)
  )
  
  # part 3.2: build the 2nd element
  # define a data frame for chart
  df2 = data.frame(x=runif(50, 5, 50),
                   y=runif(50, 1, 20))
  charts_element2 = list(
    title="Your 2nd Chart Title",
    type="scatterchart",
    xfield="x",
    yfield="y",
    data=utils.df2jsonlist(df2)
  )
  # part 3.3: put the 1st and 2nd element into charts
  charts = list(charts_element1, charts_element2)
  
  # part 4: put everything in outputs
  outputs = list(geolayers = geolayers, tables = tables, charts = charts, message="")
  
  # print the outputs in json format
  utils.debugprint(sprintf("outputs: %s", toJSON(outputs, auto_unbox=TRUE)))
  
  utils.updateJob(outputs, TRUE, jobuuid)
  return(TRUE)
}

#### DO NOT CHNAGE/DELETE THIS FUNCTION
args <- commandArgs(trailingOnly=TRUE)

#### CHNAGE TO YOUR OWN FUNCTION NAME AND FEED IT WITH PROPER PARAMETERS 
execIndicatorGreenArea(jobuuid=args[1], greenarea_wfsurl=args[2], pop_wfsurl=args[3], pop_basenum=args[4])