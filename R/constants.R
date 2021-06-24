# this variable contains all credentials for accessing a geoserver instance to publish data layers and create styles
globalGSCredentials = list()
devkey = ifelse(Sys.getenv("DIGITWIN_API_KEY") != "", Sys.getenv("DIGITWIN_API_KEY"), "")
BaseServiceUrl = "https://digitwin.com.au/services"
credUrl = paste(BaseServiceUrl,"/plugins/getgscredentials?devkey=", sep = "") 
jobUpdateUrl = paste(BaseServiceUrl,"/jobs/update", sep = "")
WMSStyleCreateUrl = paste(BaseServiceUrl,"/styling/sld/create", sep = "")#DON'T MODIFY THIS LINE
proj4string_epsg4326 = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #DON'T MODIFY THIS LINE
proj4string_utm_template = "+proj=utm +zone=%i %s+ellps=WGS84 +datum=WGS84 +units=m +no_defs" #DON'T MODIFY THIS LINE
