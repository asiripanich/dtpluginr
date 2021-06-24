# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# INDICATOR 11.3.: population density
#
# Author: SDI 2019 Group 7
# Modified by: Yuke Xie
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(dtpluginr)
library(maptools)
library(rgeos)
library(rgdal)
library(spatstat)
library(gridExtra)



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
  
  res = tail(res[res != ""], 1)
  if (0 < length(res)) return(dirname(res))
  
  # Both are not the case.
  return(NULL)
}

# change working directory to your own dir path where the r-geoserver.zip is unzipped to
setwd(LocationOfThisScript())

myDevKey = "" # DO NOT CHANGE THIS VARIABLE NAME



execIndicator <- function(jobuuid,pop_wfs){
  
  # test wfs url
  #act_url = "http://45.113.235.229:8080/geoserver/G2_POPULATION/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G2_POPULATION:act_population_mesh_blocks&outputFormat=JSON&version=1.0.0"
  
  # check if myDevKey is set
  if(nchar(myDevKey)==0){
    dt_debugprint("devKey is not provided.")
    return(FALSE)
  }
  
  
  dt_initGeoServerCredentials(myDevKey)
  
  
  sp_pop = dt_loadGeoJSON2SP(URLdecode(pop_wfs))
  
  #Union polygon by attribute
  sp_pop_union <- unionSpatialPolygons(sp_pop,sp_pop$sa2_5dig16)

  #Aggregate data by SA2 areas for population and areas
  sp_pop_df  <- as(sp_pop, "data.frame")
  sp_pop_pop <- aggregate(as.numeric(sp_pop_df[, 18]),by =list(sp_pop$sa2_name16), FUN = sum)
  sp_pop_area <- aggregate(as.numeric(sp_pop_df[, 17]),by =list(sp_pop$sa2_name16), FUN = sum)
  sp_pop_final <- merge(sp_pop_area,sp_pop_pop,by.x = "Group.1", by.y = "Group.1")
  
  #row.names(sp_pop_final) <- as.character(sp_pop_final$Group.1)
  
  #Transform the aggregated data to spdf
  sp_agg <- SpatialPolygonsDataFrame(sp_pop_union, sp_pop_final,match.ID=FALSE)
  
  colnames(sp_agg@data)<- c("sa2_name16","area","pop")
  #Indicator calculation
  sp_agg$density <- sp_agg$pop/sp_agg$area
  
  
  # publish sp object to GeoServer
  publishedinfo = dt_publishSP2GeoServerWithStyle(sp_agg, 
                                                     layerprefix="pop_den_",
                                                     styleprefix="pop_den_stl_",
                                                     attrname = "density", 
                                                     layerdisplyname = "population_density",
                                                     palettename="Reds", 
                                                     colorreverseorder=FALSE, 
                                                     colornum=8, 
                                                     classifier="Jenks", 
                                                     bordercolor="gray",
                                                     borderwidth=1, 
                                                     bordervisible=TRUE, 
                                                     styletype="graduated"
  )
  
  
  if(is.null(publishedinfo) || length(publishedinfo)==0){
    dt_debugprint("fail to save data to geoserver")
    dt_updateJob(list(message="fail to save data to geoserver"), FALSE, jobuuid)
    return(FALSE)
  }
  
  # part 1.2: append the each element into geolayers list
  geolayers = list()
  geolayers = append(geolayers, publishedinfo)
  
  tables_element1 = list(
    title="Indicator 11.3 - population density",
    data = list(
      list(
        colname="sa2_name",
        values= as.list(as.character(sp_agg$sa2_name16))
      ),
      list(
        colname="pop_density",
        values=  as.list(as.numeric(sp_agg$density))
      )
    )
  )
  
  # part 4: put everything in outputs
  outputs = list(geolayers = geolayers, tables = list(tables_element1))
  
  dt_updateJob(outputs, TRUE, jobuuid) 
  
  return(TRUE)
  
}


#### DO NOT CHNAGE/DELETE THIS FUNCTION
args <- commandArgs(trailingOnly=TRUE)

# generate indicator for Greater Darwin and Greater Hobart.
execIndicator(jobuuid=args[1],pop_wfs=args[2])
  