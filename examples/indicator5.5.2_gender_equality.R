# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script is for evaluate Sustainable Development Goal 5: Gender Equality.
# The indicator evaluated in this script is INDICATOR 5.5.NEW2.
# INDICATOR 5.5.NEW2: Full-time average weekly earnings and gender pay gap
#
# Author: SDI 2019 Group 5
# Modified by: Yuke Xie
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# loading necessary packages
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
  
  res = tail(res[res != ""], 1)
  if (0 < length(res)) return(dirname(res))
  
  # Both are not the case.
  return(NULL)
}

# change working directory to your own dir path where the r-geoserver.zip is unzipped to
setwd(LocationOfThisScript())

myDevKey = "" # DO NOT CHANGE THIS VARIABLE NAME




# the follow two lines are test wfs url addresses
#ghob_wfsurl_gender = 'http://45.113.234.194:8080/geoserver/G1_INCOME/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G1_INCOME:ghob_gender__fulltime_weekly_income_2016&outputFormat=JSON&version=1.0.0'
#gdar_wfsurl_gender = 'http://45.113.234.194:8080/geoserver/G1_INCOME/wfs?request=GetFeature&request=GetFeature&service=WFS&typename=G1_INCOME:gdar_gender_fulltime_weekly_income_2016&outputFormat=JSON&version=1.0.0'

# calcuate No Poverty index for city of Hobart/Darwin using weekly income
execIndicatorGenderEqual <- function(jobuuid,sp_gender_wfsurl){
 
  
  # check if myDevKey is set
  if(nchar(myDevKey)==0){
    dt_debugprint("devKey is not provided.")
    return(FALSE)
  }

  
  dt_initGeoServerCredentials(myDevKey)
  
  sp_gender = dt_loadGeoJSON2SP(URLdecode(sp_gender_wfsurl))
  
  
  # check if data layer can be successfully loaded
  if(is.null(sp_gender)){
    dt_debugprint("fail to load data layer for weekly income")
    dt_updateJob(list(message="fail to load data layer for weekly income"), FALSE, jobuuid)
    return(FALSE)
  }

  
  # add the attribute for sp_gender for indicator 5.5.NEW2
  sp_gender@data[,'genEqual'] = 0.0
  
  
  
  # get the totoal income for male and female in each sa2 area
  female_total = (75 * sp_gender$female_1_149 + 225 * sp_gender$female_150_299 + 350 * sp_gender$female_300_399 
  + 450 * sp_gender$female_400_499 + 575 * sp_gender$female_500_649 + 725 * sp_gender$female_650_799
  + 900 * sp_gender$female_800_999 + 1125 * sp_gender$female_1000_1249 + 1375 * sp_gender$female_1250_1499
  + 1625 * sp_gender$female_1500_1749 + 1875 * sp_gender$female_1750_1999 + 2500 * sp_gender$female_2000_2999
  + 3000 * sp_gender$female_3000_or_more)
  
  male_total = (75 * sp_gender$male_1_149 + 225 * sp_gender$male_150_299 + 350 * sp_gender$male_300_399 
  + 450 * sp_gender$male_400_499 + 575 * sp_gender$male_500_649 + 725 * sp_gender$male_650_799
  + 900 * sp_gender$male_800_999 + 1125 * sp_gender$male_1000_1249 + 1375 * sp_gender$male_1250_1499
  + 1625 * sp_gender$male_1500_1749 + 1875 * sp_gender$male_1750_1999 + 2500 * sp_gender$male_2000_2999
  + 3000 * sp_gender$male_3000_or_more)
  
  
  # get the total female and male counts in each sa2 area
  female_count = (sp_gender$female_1_149 + sp_gender$female_150_299 + sp_gender$female_300_399
  + sp_gender$female_400_499 + sp_gender$female_500_649 + sp_gender$female_650_799
  + sp_gender$female_800_999 + sp_gender$female_1000_1249 + sp_gender$female_1250_1499
  + sp_gender$female_1500_1749 + sp_gender$female_1750_1999 + sp_gender$female_2000_2999
  + sp_gender$female_3000_or_more)
  
  male_count = (sp_gender$male_1_149 + sp_gender$male_150_299 + sp_gender$male_300_399
  + sp_gender$male_400_499 + sp_gender$male_500_649 + sp_gender$male_650_799
  + sp_gender$male_800_999 + sp_gender$male_1000_1249 + sp_gender$male_1250_1499
  + sp_gender$male_1500_1749 + sp_gender$male_1750_1999 + sp_gender$male_2000_2999
  + sp_gender$male_3000_or_more)
  
  
  # get the weekly average income for male and female in different sa2 area
  female_avg = female_total/female_count
  male_avg = male_total/male_count
  
  
  #get the indicator which is the deduction of male and female avg weekly income
  index_genderequal = male_avg - female_avg
  
  
  # assign calculated values back to sp_gender_proj@data. use as.numberic() to assure the values are numeric
  sp_gender@data[,'genEqual'] = as.numeric(index_genderequal)
  sp_gender@data = sp_gender@data[c("sa2_main16","sa2_name16","genEqual")]

  # publish sp object to GeoServer
  publishedinfo = dt_publishSP2GeoServerWithStyle(sp_gender, 
                                                     layerprefix="genEqu_",
                                                     styleprefix="genEqu_stl_",
                                                     attrname = "genEqual", 
                                                     layerdisplyname = "gender_equality",
                                                     palettename="Reds", 
                                                     colorreverseorder=FALSE, 
                                                     colornum=6, 
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
    title="Indicator 4.3.1 - higher education",
    data = list(
      list(
        colname="sa2_name",
        values= as.list(as.character(sp_gender$sa2_name16))
      ),
      list(
        colname="higher_education_ratio",
        values= as.list(index_genderequal)
      )
    )
  )
  
  
  # part 3: build charts
  #replace NA value with 0
  sp_gender$genEqual[is.na(sp_gender$genEqual)]<-0
  

  #chart 2
  df2=data.frame(female = female_avg,
                 male = male_avg)
  
  charts_element2 = list(
    title="Full Time Weekly Income Comparison",
    type="scatterchart",
    xfield="female",
    yfield="male",
    xaxistitle= "Female",
    yaxistitle= "Male",
    data=dt_df2jsonlist(df2)
  )
  
  
  # part 3.3: put the 1st and 2nd element into charts
  charts = list(charts_element2)
  
  # part 4: put everything in outputs
  outputs = list(geolayers = geolayers, tables = list(tables_element1),charts = charts,message="")
  
  dt_updateJob(outputs, TRUE, jobuuid) 
  
  return(TRUE)
}

#### DO NOT CHNAGE/DELETE THIS FUNCTION
args <- commandArgs(trailingOnly=TRUE)

# generate indicator for Greater Darwin and Greater Hobart.
execIndicatorGenderEqual(jobuuid=args[1], sp_gender_wfsurl=args[2])