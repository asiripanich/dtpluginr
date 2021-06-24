#############################################################
# R plugin for calculating indicator 4.3.1 Equal Education
# Original file: SDI 2019 Group 4
# Modified by : Yuke Xie
############################################################

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
source("utils.R")

execIndicatorEducation <- function(jobuuid,wfs_url){
  
  
  # check if myDevKey is set
  if(nchar(myDevKey)==0){
    utils.debugprint("devKey is not provided.")
    return(FALSE)
  }
  
  # ATTENTION: this function MUST be called first before calling any other utils functions
  utils.initGeoServerCredentials(myDevKey)
  
  # Greater Darwin SA2's higher education gender age layer url obtained from Group7 GeoServer on Central GeoNetwork
  # below is the test wfs url
  # wfs_url = "http://45.113.234.50:8080/geoserver/G5_EDUCATION/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G5_EDUCATION:gdar_sa2_higher_education_gender_age&outputFormat=JSON&version=1.0.0."
  
  
  edu_gender = utils.loadGeoJSON2SP(URLdecode(wfs_url))
  
  # check if data layer can be successfully loaded
  if(is.null(edu_gender)){
    utils.debugprint("fail to load data layer for higher education")
    utils.updateJob(list(message="fail to load data layer for higher education"), FALSE, jobuuid)
    return(FALSE)
  }
  
  edu_gender$high_total = edu_gender$higher_education_25_64_male + edu_gender$higher_education_25_64_female
  
  edu_gender$ttl_25_64 =  edu_gender$total_25_64_male + edu_gender$total_25_64_female
  
  edu_gender$high_ratio = edu_gender$high_total / edu_gender$ttl_25_64
  
  edu_gender_final = utils.project2UTM(edu_gender)

  edu_gender_final@data = edu_gender_final@data[c("sa2_name16","high_total","high_ratio")]    
  
  publishedinfo = utils.publishSP2GeoServerWithStyle(spobj=edu_gender_final,
                                                     layerprefix="education_",   
                                                     styleprefix="education_stl_",
                                                     attrname="high_ratio",
                                                     layerdisplyname = "higher_education_ratio",
                                                     palettename="Reds", 
                                                     colorreverseorder=FALSE, 
                                                     colornum=8, 
                                                     classifier="Jenks",
                                                     bordercolor = "gray",
                                                     borderwidth = 1,
                                                     bordervisible = TRUE,
                                                     styletype = "graduated"
  )
  
  if(is.null(publishedinfo) || length(publishedinfo)==0){
    utils.debugprint("fail to save data to geoserver")
    utils.updateJob(list(message="fail to save data to geoserver"), FALSE, jobuuid)
    return(FALSE)
  }
  
  
  # part 1.2: append the each element into geolayers list
  geolayers = list()
  geolayers = append(geolayers, publishedinfo)
  
  tables_element1 = list(
    title="Indicator 4.3.1 - higher education",
    data = list(
      list(
        colname="sa2_name",
        values= as.list(as.character(edu_gender_final$sa2_name16))
      ),
      list(
        colname="higher_education_ratio",
        values= as.list(as.numeric(edu_gender_final$high_ratio))
      )
    )
  )
  
  
  #part 2.3: put the 1st and 2nd element into tables
  tables = list(tables_element1)
  
  # part 3: build charts
  #replace NA value with 0
  edu_gender_final$high_ratio[is.na(edu_gender_final$high_ratio)]<-0
  
  # create intervals
  ratio=""
  freq =""
  for(i in 1:10){
    freq[i]=sum(edu_gender_final$high_ratio<i*0.1 & edu_gender_final$high_ratio>(i-1)*0.1)
    ratio[i]=paste((i-1)*10,'-',i*10,'%')
  }
  
  
  df1 = data.frame(ratio=ratio,
                   count=as.numeric(freq)
  )
  
  charts_element1 = list(
    title="Higher Education Ratio",
    type="columnchart",
    stacked=FALSE,
    xfield="ratio",
    yfield="count",
    yfieldtitle="Higher Education",
    data=utils.df2jsonlist(df1)
  )
  
  #chart 2
  df2=data.frame(female = edu_gender$higher_education_25_64_female,
                 male = edu_gender$higher_education_25_64_male)
  
  charts_element2 = list(
    title="Higher Education Comparison",
    type="scatterchart",
    xfield="female",
    yfield="male",
    xaxistitle= "Female",
    yaxistitle= "Male",
    data=utils.df2jsonlist(df2)
  )
  
  
  # part 3.3: put the 1st and 2nd element into charts
  charts = list(charts_element1, charts_element2)
  
  # part 4: put everything in outputs
  outputs = list(geolayers = geolayers, tables = tables,charts = charts,message="")
  
  utils.updateJob(outputs, TRUE, jobuuid) 
  
  return(TRUE)
}



#### DO NOT CHNAGE/DELETE THIS FUNCTION
args <- commandArgs(trailingOnly=TRUE)

#### CHNAGE TO YOUR OWN FUNCTION NAME AND FEED IT WITH PROPER PARAMETERS 
execIndicatorEducation(jobuuid=args[1], wfs_url=args[2])




