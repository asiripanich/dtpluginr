#############################################################
# R plugin for calculating indicator 11.2 Travel to work
# Original file: SDI 2019 Group 6
# Modified by : Yuke Xie
############################################################
library(dtpluginr)
library(maptools) 
library(rgdal)
library(rgeos)
library(jsonlite)

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
  
  return(NULL)
}

# change working directory to your own dir path where the r-geoserver.zip is unzipped to
setwd(LocationOfThisScript())


myDevKey = Sys.getenv("DIGITWIN_API_KEY") # DO NOT CHANGE THIS VARIABLE NAME

# load utils methods for use



#Function Indicator 5 for brisbane Public Transport
execIndicator <-function(jobuuid,travelmodebri_wfsurl){
 
  
  # check if myDevKey is set
  if(nchar(myDevKey)==0){
    dt_debugprint("devKey is not provided.")
    return(FALSE)
  }
  
  dt_initGeoServerCredentials(myDevKey)
  
  #the following line is for testing
  #travelmodebri_wfsurl = "http://45.113.235.73:8080/geoserver/G6_TRANSPORT/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G6_TRANSPORT:gbri_travel_to_work&outputFormat=JSON&version=1.0.0."
  
  
  # load spatial object direct from geojson
  sp_publictransport_bri = dt_loadGeoJSON2SP(URLdecode(travelmodebri_wfsurl))
  
  # check if data layer can be successfully loaded
  if(is.null(sp_publictransport_bri)){
    dt_debugprint("fail to load data layer for travel mode")
    dt_updateJob(list(message="fail to load data layer for travel mode"), FALSE, jobuuid)
    return(FALSE)
  }
  
  
  sp_publictransport_bri_prj = dt_project2UTM(sp_publictransport_bri)
  
  
  #Calculate Indicators
  sp_publictransport_bri_prj$prc_pub <- (sp_publictransport_bri_prj$total_public_transport/sp_publictransport_bri_prj$total)*100  
  sp_publictransport_bri_prj$prc_bike <- (sp_publictransport_bri_prj$bicycle/sp_publictransport_bri_prj$total)*100 
  sp_publictransport_bri_prj$prc_walk <- (sp_publictransport_bri_prj$walk/sp_publictransport_bri_prj$total)*100 
  
  sp_publictransport_bri_prj@data = sp_publictransport_bri_prj@data[c("sa2_name16","prc_pub","prc_bike","prc_walk")]
  
  #publish to GeoServer
  publishedinfo = dt_publishSP2GeoServerWithMultiStyles(
    spobj=sp_publictransport_bri_prj,
    layerprefix="travelwork_",
    styleprefix="travelwork_stl",
    layerdisplyname_vec = c("travel_to_work_publick","travel_to_work_bike","travel_to_work_walk"),
    attrname_vec=c("prc_pub","prc_bike","prc_walk"),
    palettename_vec=c("Greens","Blues","Reds"),
    colorreverseorder_vec=c(FALSE,FALSE,FALSE),
    colornum_vec=c(8,6,6),
    classifier_vec=c("Jenks","Jenks","Jenks"),
    bordercolor_vec=c("gray", "gray","gray"),
    borderwidth_vec=c(1, 1, 1),
    bordervisible_vec=c(TRUE, TRUE,TRUE),
    styletype_vec=c("graduated", "graduated","graduated")
  )


  #----------------------------------------------------------------------------
  if(is.null(publishedinfo) || length(publishedinfo)==0){
    dt_debugprint("fail to save data to geoserver")
    dt_updateJob(list(message="fail to save data to geoserver"), FALSE, jobuuid)
    return(FALSE)
  }
  
  
  # part 1.2: append the each element into geolayers list
  geolayers = list()
  geolayers = append(geolayers, publishedinfo)
  
  tables_element1 = list(
    title="Indicator 11.2 - travel to work",
    data = list(
      list(
        colname="sa2_name",
        values= as.list(as.character(sp_publictransport_bri_prj$sa2_name16))
      ),
      list(
        colname="public",
        values=  as.list(as.numeric(sp_publictransport_bri_prj$prc_pub))
      ),
      list(
        colname="bike",
        values=  as.list(as.numeric(sp_publictransport_bri_prj$prc_bike))
      ),
      list(
        colname="walk",
        values=  as.list(as.numeric(sp_publictransport_bri_prj$prc_walk))
      )
    )
  )

  
  #part 2.3: put the 1st and 2nd element into tables
  #tables = list(tables_element1)
  
  # part 3: build charts
  #replace NA value with 0
  sp_publictransport_bri_prj$prc_pub[is.na(sp_publictransport_bri_prj$prc_pub)]<-0
  sp_publictransport_bri_prj$prc_bike[is.na(sp_publictransport_bri_prj$prc_bike)]<-0
  sp_publictransport_bri_prj$prc_walk[is.na(sp_publictransport_bri_prj$prc_walk)]<-0
  
  
  # create intervals
  ratio=""
  public =""
  bike = ""
  walk = ""
  for(i in 1:10){
    public[i]=sum(sp_publictransport_bri_prj$prc_pub<i*10 & sp_publictransport_bri_prj$prc_pub>(i-1)*10)
    bike[i]=sum(sp_publictransport_bri_prj$prc_bike<i*10 & sp_publictransport_bri_prj$prc_bike>(i-1)*10)
    walk[i]=sum(sp_publictransport_bri_prj$prc_walk<i*10 & sp_publictransport_bri_prj$prc_walk>(i-1)*10)
    ratio[i]=paste((i-1)*10,'-',i*10,'%')
  }
  
  
  df1 = data.frame(ratio=ratio,
                   public=as.numeric(public),
                   bike = as.numeric(bike),
                   walk = as.numeric(walk)
  )
  
  charts_element1 = list(
    title="Travel to Work",
    type="columnchart",
    stacked=FALSE,
    xfield="ratio",
    yfield=list("public","bike","walk"),
    yfieldtitle=list("public transport","bicycle","walk"),
    data=dt_df2jsonlist(df1)
  )
  
  # part 4: put everything in outputs
  outputs = list(geolayers = geolayers, tables = list(tables_element1),charts=list(charts_element1))
  
  dt_updateJob(outputs, TRUE, jobuuid) 
  
  return(TRUE)
  
}

#### DO NOT CHNAGE/DELETE THIS FUNCTION
args <- commandArgs(trailingOnly=TRUE)

# generate indicator for Greater Darwin and Greater Hobart.
execIndicator(jobuuid=args[1], travelmodebri_wfsurl=args[2])
