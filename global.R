#######################################################################
# The global setup for the imageRun shiny app. 
# 
# The imageRun app is developed and maintained by Bijan Seyednasrollah.
# The main initial development was done in November, 2017.
#
# Most recent release: https://github.com/bnasr/imageRun
#######################################################################

source('aux.R')

list.of.packages <- c(
  'rgdal',
  'shiny',
  # 'shinyjs',
  'shinyFiles',
  'shinythemes',
  'rjson',
  'RJSONIO',
  'sp',
  'rgdal',
  'raster',
  'jpeg',
  'data.table'
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.rstudio.com/')

for(p in list.of.packages) library(p, character.only = T)

LOCAL_RUN <- T
if(system('hostname', intern=T)%in%c('phenocam')&
   system('whoami', intern=T)%in%c('shiny')) LOCAL_RUN <- F

PRINT_LOGS <- T
