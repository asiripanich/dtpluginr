# README #

This repo is for creating Digitwin plugins using R.

"sample_greenarea.r" contains a sample source code of creating greenarea indicator using R and GeoServer.

"utils.r" contains all utility functions to interact GeoServer with R as simple as possible. 


All codes are tested in R 4.10 environment on windows and linux.


### How do I get set up? ###

A couple of softwares and tools are required to create Digitwin plugins usring R:

(1) Install R (version 4.10 or above) from the official website

https://cran.r-project.org/


(2) RStudio is recommended for developing R scripts. The "RStudio Desktop Open Source License" version just works great.

https://www.rstudio.com/products/rstudio/download/


(3) Install Rtools (for Windows developers only) from here 

https://cran.r-project.org/bin/windows/Rtools/ 

since zip.exe in the Rtools packages will be used in the "utils.r", please make sure "YOUR_Rtools_DIRECTORY/bin" is included in the system "Path" environment variable.

(4) When testing your indicator script, RScript.exe which locates in "YOUR_R_DIRECTORY\bin" will be used, make sure this directory is included in the system "Path" environment variable.

(5) Download the source code from bitbucket

(6) install R packages on your dev environment. In RStudio, go to "Tools"->"Install Packages", make sure the "Install dependencies" option is checked, then install the following packages one by one:

'sp', maptools', 'rgdal', 'rgeos', 'jsonlite, 'httr', 'uuid'

you might need to close and reopen RStudio to use the installed packages

(7) "devKey" is used for storing and publishing plugin outputs in Digitwin GeoServers so that the outputs can be viewed, used, downloaded by others. To obtain a devKey for Digitwin plugin development, please contact UoM Digitwin dev team



### Who do I talk to? ###

If you have any questions or suggestions, please contact:

Dr Yiqun Chen

Centre for Disaster Management & Public Safety

Centre for Spatial Data Infrastructures & Land Administration

The University of Melbourne

E: yiqun.c@unimelb.edu.au