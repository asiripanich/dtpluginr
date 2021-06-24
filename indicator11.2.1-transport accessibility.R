####################################################################################
#indicator11.2.1 proportion of population that has convenient access to public transport 
#
#Author: SDI 2019 Group 8
#Modified by: Yuke Xie
####################################################################################

library(maptools)
library(rgdal)
library(rgeos)
library(jsonlite)
library(doParallel)
library(dplyr)
library(sf)



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

source("utils.R")




#' Calculates the proportion of people having convenient access to public transport
#' 
#' @param pop_wfsurl WFS URL to population dataset
#' @param transport_wfsurl WFS URL to transport dataset
#'

execIndicatorBuffer <- function(jobuuid,pop_wfsurl,transport_wfsurl){
  
  # test wfs url 
  # wfs url of transpport and polulation in Melbourne and ACT
  # transport_act_wfsurl = "http://45.113.235.229:8080/geoserver/G2_TRANSPORTATION/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G2_TRANSPORTATION:act_transportation&outputFormat=JSON&version=1.0.0."
  # pop_act_wfsurl = "http://45.113.235.229:8080/geoserver/G2_POPULATION/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G2_POPULATION:act_population_mesh_blocks&outputFormat=JSON&version=1.0.0."
  # 
  # pop_mel_wfsurl = "http://45.113.235.229:8080/geoserver/G2_POPULATION/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G2_POPULATION:gmel_population_mesh_blocks&outputFormat=JSON&version=1.0.0."
  # transport_mel_wfsurl = "http://45.113.235.229:8080/geoserver/G2_TRANSPORTATION/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G2_TRANSPORTATION:gmel_transportation&outputFormat=JSON&version=1.0.0"
  #
  
  
  # check if myDevKey is set
  if(nchar(myDevKey)==0){
    utils.debugprint("devKey is not provided.")
    return(FALSE)
  }
  
  utils.initGeoServerCredentials(myDevKey)
  
  
  # load spatial object direct from geojson
  sp_transport = utils.loadGeoJSON2SP(URLdecode(transport_wfsurl))
  
  # check if data layer can be successfully loaded
  if(is.null(sp_transport)){
    utils.debugprint("fail to load data layer for transport")
    utils.updateJob(list(message="fail to load data layer for transport"), FALSE, jobuuid)
    return(FALSE)
  }
  
  sp_pop = utils.loadGeoJSON2SP(URLdecode(pop_wfsurl))
  
  # check if data layer can be successfully loaded
  if(is.null(sp_pop)){
    utils.debugprint("fail to load data layer for population")
    utils.updateJob(list(message="fail to load data layer for population"), FALSE, jobuuid)
    return(FALSE)
  }
  
  # project(transform) sp into UTM to enable area calculation
  sp_transport_prj = utils.project2UTM(sp_transport)
  sp_pop_prj = utils.project2UTM(sp_pop)
  
  
  # Creat buffers for transport stations
  if("railway_station" %in% sp_transport_prj$fclass | "railway_halt" %in% sp_transport_prj$fclass){
    train_stops = sp_transport_prj[(sp_transport_prj$fclass == "railway_station" | sp_transport_prj$fclass == "railway_halt"),]
    buffer_trainStops = gBuffer(train_stops, byid=TRUE, width=800)
  }
  
  if("bus_station" %in% sp_transport_prj$fclass | "bus_stop" %in% sp_transport_prj$fclass){
    bus_stops = sp_transport_prj[(sp_transport_prj$fclass == "bus_station" | sp_transport_prj$fclass == "bus_stop"),]
    buffer_busStops = gBuffer(bus_stops, byid=TRUE, width=400)
  }
  
  if("tram_stop" %in% sp_transport_prj$fclass ){
    tram_stops = sp_transport_prj[sp_transport_prj$fclass == "tram_stop",]
    buffer_tramStops = gBuffer(tram_stops, byid=TRUE, width=600)
  }
  
  # Union all buffers
  if(exists("buffer_trainStops")){
    b1 = gUnaryUnion(buffer_trainStops)
    union_buffers = b1  
  }

  if(exists("buffer_busStops")){
    b2 = gUnaryUnion(buffer_busStops)
    if(exists("union_buffers"))
      union_buffers = gUnion(union_buffers,b2)
    else union_buffers = b2
  }
    
  if(exists("buffer_tramStops")){
    b3 = gUnaryUnion(buffer_tramStops)
    if(exists("union_buffers"))
      union_buffers = gUnion(union_buffers,b3)
    else union_buffers = b3
  }
    

  # Centroids of mesh blocks
  centroids_of_mesh_blocks = gCentroid(sp_pop_prj, byid=TRUE)

  # Population of mesh blocks within the buffer regions
  is_mesh_block_within_buffer = gContains(union_buffers, centroids_of_mesh_blocks, byid=TRUE)
  pop_buffer_meshblock = sp_pop_prj[c(is_mesh_block_within_buffer),]
  
  
  # Transform sp to sf
  sp_pop_prj = st_as_sf(sp_pop_prj)
  pop_buffer_meshblock = st_as_sf(pop_buffer_meshblock)
  
  # Group by sa2_name
  sp_pop_groupbySA2 = sp_pop_prj %>% group_by(sa2_name16) %>% summarise(pop_SA2= sum(as.numeric(population_total)))
  pop_buffer_groupbySA2 = pop_buffer_meshblock %>% group_by(sa2_name16) %>% summarise(popbuf_SA2= sum(as.numeric(population_total)))
                                                                  
  # add one more attributes for sp_pop_total which is for the the proportion of population that has convenient access to public transport 
  sp_pop_total = merge(as.data.frame(sp_pop_groupbySA2), as.data.frame(pop_buffer_groupbySA2),by="sa2_name16") 
  sp_pop_total=st_sf(sp_pop_total,sf_column_name = "geometry.x")
  sp_pop_total$bufferPct = 0.0
  
  

  # using 4 cores for parallel computing
  registerDoParallel(cores = 4)
  
  
  result = foreach(i=1:nrow(sp_pop_total), .combine = cbind)%dopar%{
    
    population = as.numeric(sp_pop_total$pop_SA2[i])
    buffer_pop= as.numeric(sp_pop_total$popbuf_SA2[i])
    
    # check population attribute, make sure it is valid
    if(is.null(population)||is.na(population)) {
      population = 0
      out = 0
    }else{
      buffer_pct = buffer_pop/population
      out = buffer_pct
    }
    
  }
  
  # assign calculated values back to sp_pop_total$bufferPct. use as.double() to assure the values are double
  
  sp_pop_total$bufferPct = as.double(result)
  
  
  #sf to sp
  sp_pop_final = sf:::as_Spatial(sp_pop_total)
  
  sp_pop_final$bufferPct[is.na(sp_pop_final$bufferPct)]<-0
  sp_pop_final$geometry.y<-NULL
  
  stopImplicitCluster()
  
  # # # # # # # # # # # # #
  # publish 
  # # # # # # # # # # # # #
  publishedinfo = utils.publishSP2GeoServerWithStyle(spobj=sp_pop_final,
                                                           layerprefix="trans_access_",
                                                           styleprefix="trans_access_stl_",
                                                           attrname="bufferPct",
                                                           layerdisplyname = "public transport accessibility",      
                                                           palettename="Blues",
                                                           colorreverseorder=FALSE,
                                                           colornum=8,
                                                           classifier="Jenks",
                                                           bordercolor="gray",
                                                           borderwidth=1,
                                                           bordervisible=TRUE,
                                                          styletype= "graduated"
  )

  if(is.null(publishedinfo) || length(publishedinfo)==0){
    utils.debugprint("fail to save data to geoserver")
    return(FALSE)
  }

  
  # part 1.2: append the each element into geolayers list
  geolayers = list()
  geolayers = append(geolayers, publishedinfo)
  
  tables_element1 = list(
    title="Indicator 11.2.1 - Access to Public Transport",
    data = list(
      list(
        colname="sa2_name",
        values= as.list(as.character(sp_pop_final$sa2_name16))
      ),
      list(
        colname="population",
        values=  as.list(as.numeric(sp_pop_final$pop_SA2))
      ),
      list(
        colname="public_access",
        values=  as.list(as.numeric(sp_pop_final$popbuf_SA2))
      ),
      list(
        colname="percentage",
        values=  as.list(as.numeric(sp_pop_final$bufferPct))
      )
    )
  )
  
  
  # create intervals
  ratio=""
  freq =""
  
  for(i in 1:10){
    freq[i]=sum(sp_pop_final$bufferPct<i*0.1 & sp_pop_final$bufferPct>(i-1)*0.1)
    ratio[i]=paste((i-1)*10,'-',i*10,'%')
  }
  
  
  df1 = data.frame(ratio=ratio,
                   freq=as.numeric(freq)
                   )
  
  charts_element1 = list(
    title="Access to Public Transport",
    type="columnchart",
    stacked=FALSE,
    xfield="ratio",
    yfield= "freq",
    yfieldtitle="access percentage",
    data=utils.df2jsonlist(df1)
  )
  
  # part 4: put everything in outputs
  outputs = list(geolayers = geolayers, tables = list(tables_element1),charts=list(charts_element1))
  
  utils.updateJob(outputs, TRUE, jobuuid) 


  return(TRUE)
  
  
}

#### DO NOT CHNAGE/DELETE THIS FUNCTION
args <- commandArgs(trailingOnly=TRUE)

# generate indicator for Greater Darwin and Greater Hobart.
execIndicatorBuffer(jobuuid=args[1], pop_wfsurl=args[2],transport_wfsurl=args[3])


