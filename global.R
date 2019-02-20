#######################################################################
# The global setup for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

source('aux.R')


list.of.packages <- c(
  'data.table',
  'imager',
  'jsonlite',
  'jpeg',
  'png',
  'plotly',
  'shiny',
  'shinyFiles',
  'shinyjs',
  'shinythemes',
  'raster',
  'rgdal',
  'readr',
  'sp',
  'tiff',
  'tools'
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) 
  install.packages(new.packages, 
                   repos='http://cran.rstudio.com/')

sapply(list.of.packages, library, character.only = T)

LOCAL_RUN <- T
if(system('hostname', intern=T)%in%c('phenocam')&
   
   system('whoami', intern=T)%in%c('shiny')) LOCAL_RUN <- F

PRINT_LOGS <- T

dir.create('images', showWarnings = FALSE)
