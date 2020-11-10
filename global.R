#######################################################################
# The global setup for the TRIAD shiny app. 
# 
# The TRIAD app is developed and maintained by Bijan Seyednasrollah.
#
# TRIAD is the Tree Ring Image Analysis and Dataset
#
# Most recent release: https://github.com/bnasr/TRIAD
#######################################################################

# This is the version of the Tree Ring Image Analysis and Database 
TRIADversion <- 'Generated with the Tree Ring Image Analysis and Database (TRIAD) v0.1.1.1.'

# loading auxiliary functions that are called in server.R and ui.R
source ('funcs.R')

# list of required packages
list.of.packages <- c (
  'abind',
  'data.table',
  'dplR',
  'dplyr',
  'DT',
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
  'readxl',
  'sp',
  'tiff',
  'tibble',
  'tools',
  'zoo'
)

# identify new (not installed) packages
new.packages <- list.of.packages [!(list.of.packages %in% installed.packages () [,"Package"])]

# install new (not installed) packages from CRAN
if (length (new.packages)) 
  install.packages (new.packages, 
                    repos = 'http://cran.rstudio.com/')

# load all of the required libraries
sapply (list.of.packages, library, character.only = T)

# a MACRO to monitor local runs vs. web-based runs
LOCAL_RUN <- TRUE
if (system ('hostname', intern = T) %in% c ('phenocam') &
    system ('whoami',   intern = T) %in% c ('shiny')) LOCAL_RUN <- F

# MACRO to control log outputs
PRINT_LOGS <- TRUE

# temporary archive directory
ARCHIVE_DIR <- 'images/'
  
# create a directory for uploaded images, this is probably needed only for development stages
dir.create (ARCHIVE_DIR, showWarnings = FALSE)

# colours for ploting labels
colours <- tibble (
  type   = c ('Normal','Linker','Pith','Misc','Missing'), 
  colour = c ('yellow','cornflowerblue','#a41034','#91b9a4','#a41034')
)

# set maximum image size in MB
maxImageSize <- 200