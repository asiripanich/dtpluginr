#############################################################
# R plugin for calculating indicator 1.2.1 poverty 
# Original file: SDI 2019 Group 2
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
  print(length(cmd.args))
  print(length(cmd.args.trailing))
  cmd.args = cmd.args[seq.int(from=1, length.out=length(cmd.args) - length(cmd.args.trailing))]
  res = gsub("^(?:--file=(.*)|.*)$", "\\1", cmd.args)
  
  # If multiple --file arguments are given, R uses the last one
  res = tail(res[res != ""], 1)
  if (0 < length(res)) return(dirname(res))
  
  # Both are not the case.
  return(NULL)
}

# change working directory to your own dir path where the r-geoserver.zip is unzipped to
setwd(LocationOfThisScript())

# ATTENTION:
# devKey is used for storing and publishing plugin outputs in Digitwin GeoServers so that the outputs can be viewed, used, downloaded by others
# To obtain a devKey for Digitwin plugin development, please contact UoM Digitwin dev team.

myDevKey = "" # DO NOT CHANGE THIS VARIABLE NAME

# this the main wrapper method which handles the arguments check and call the plugin calculation method
# to trigger this in cmd line, just run : RScript path\sample_code.R "arg1" "arg2"


# load utils methods for use


execIndicatorPoverty <- function(jobuuid,per_income_wfsurl){
  
  # check if myDevKey is set
  if(nchar(myDevKey)==0){
    utils.debugprint("devKey is not provided.")
    return(FALSE)
  }
  
  # ATTENTION: this function MUST be called first before calling any other utils functions
  utils.initGeoServerCredentials(myDevKey)

  #test wfsurl
  #per_income_wfsurl="http://45.113.235.54:8080/geoserver/G8_INCOME/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G8_INCOME:gsyd_personal_weekly_income&outputFormat=JSON&version=1.0.0."
  
  # load spatial object direct from geojson
  sp_per_income = utils.loadGeoJSON2SP(URLdecode(per_income_wfsurl))
  
  # check if data layer can be successfully loaded
  if(is.null(sp_per_income)){
    utils.debugprint("fail to load data layer for weekly income")
    utils.updateJob(list(message="fail to load data layer for weekly income"), FALSE, jobuuid)
    return(FALSE)
  }
  
  # add two more attributes
  sp_per_income@data[,"cnt_400"] = 0.0
  sp_per_income@data[,"prc_400"] = 0.0
  
  #total_pop     <- sp_per_income@data[,20]
  total_pop <- rowSums(sp_per_income@data[,c(40,41)])
  
  #count_below   <-  sp_per_income@data[,3] + sp_per_income@data[,4] +  sp_per_income@data[,5] +  sp_per_income@data[,6] + sp_per_income@data[,7]
  count_below <- rowSums(sp_per_income@data[,c(3:12)])
  
  percent_below <- (count_below/total_pop)*100
  
  
  # assign this newly created column with "count_below"
  sp_per_income@data[,"cnt_400"] = as.numeric(count_below)
  
  # assign this newly created column with "percent_below_400"
  sp_per_income@data[,"prc_400"] = as.numeric(percent_below)
  
  sp_per_income@data = sp_per_income@data[c("sa2_main16","sa2_name16","cnt_400","prc_400")]
  
  #----------------------------------------------------------------------------
  # PUBLISHING
  # this example shows how to publish a geolayer by creating two wms styles on various attributes of the same data layer. 
  # the data layer will be only published one time, with various wms styles generated for selected attributes 
  geolayers_gaindex = utils.publishSP2GeoServerWithMultiStyles(spobj=sp_per_income, 
                                                               layerprefix="poverty_",
                                                               styleprefix="poverty_stl_",
                                                               geomtype = utils.getGeomType(sp_per_income), 
                                                               attrname_vec=c("cnt_400","prc_400"),
                                                               layerdisplyname_vec=c("poverty_count_below_400","poverty_percent_below_400"),
                                                               palettename_vec=c("Greens","Reds"), 
                                                               colorreverseorder_vec=c(FALSE,FALSE), 
                                                               colornum_vec=c(6,8), 
                                                               classifier_vec=c("Jenks","Jenks"),
                                                               bordercolor_vec=c("gray", "gray"),
                                                               borderwidth_vec=c(1, 1),
                                                               bordervisible_vec=c(TRUE, TRUE),
                                                               styletype_vec=c("graduated", "graduated")
  )
  # geolayers_gaindex = utils.publishSP2GeoServer(sp_per_income)
  #----------------------------------------------------------------------------
  if(is.null(geolayers_gaindex) || length(geolayers_gaindex)==0){
    utils.debugprint("fail to save data to geoserver")
    utils.updateJob(list(message="fail to save data to geoserver"), FALSE, jobuuid)
    return(FALSE)
  }
  
  
  # part 1.2: append the each element into geolayers list
  geolayers = list()
  geolayers = append(geolayers, geolayers_gaindex)
  
  tables_element1 = list(
    title="Indicator 1.2.1 - poverty - count",
    data = list(
      list(
        colname="sa2_name",
        values= as.list(as.character(sp_per_income$sa2_name16))
      ),
      list(
        colname="weeklyIncomeBelow400(count)",
        values= as.list(count_below)
      ),
      list(
        colname="weeklyIncomeBelow400(percent)",
        values=as.list(percent_below)
      )
    )
  )


  # part 3: build charts
  # part 3.1: build the 1st element
  # define a data frame for chart
  
  #replace NA value with 0
  sp_per_income$prc_400[is.na(sp_per_income$prc_400)]<-0
  
  
  # create intervals
  ratio=""
  freq =""
  for(i in 1:10){
    freq[i]=sum(sp_per_income$prc_400<i*10 & sp_per_income$prc_400>(i-1)*10)
    ratio[i]=paste((i-1)*10,'-',i*10,'%')
  }
    
  
    
  df1 = data.frame(ratio=ratio,
                   count=as.numeric(freq)
                   )

  charts_element1 = list(
      title="Poverty Ratio",
    type="columnchart",
    stacked=FALSE,
    xfield="ratio",
    yfield="count",
    yfieldtitle="weekly income below 400",
    data=utils.df2jsonlist(df1)
  )

  
  # part 4: put everything in outputs
  outputs = list(geolayers = geolayers, tables = list(tables_element1),charts = list(charts_element1),message="")
  
  # print the outputs in json format
  #utils.debugprint(sprintf("outputs: %s", toJSON(outputs, auto_unbox=TRUE)))
  
  utils.updateJob(outputs, TRUE, jobuuid) 
  
  return(TRUE)
}
 

  
#### DO NOT CHNAGE/DELETE THIS FUNCTION
args <- commandArgs(trailingOnly=TRUE)

#### CHNAGE TO YOUR OWN FUNCTION NAME AND FEED IT WITH PROPER PARAMETERS 
execIndicatorPoverty(jobuuid=args[1], per_income_wfsurl=args[2])
