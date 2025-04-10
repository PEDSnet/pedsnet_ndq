
#' Make sure the `pedsnet_ndq.Rproj` file is opened prior to executing this script
#' This will ensure the working directory is populated appropriately

# Load required packages
# devtools::install_github('PEDSnet/argos')
# devtools::install_github('PEDSnet/ndq')
# devtools::install_github('ssdqa/ssdqa.gen')
library(argos)
library(srcr)
library(ndq)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DBI)
library(dbplyr)
library(lubridate)
library(ssdqa.gen)

# Source file with wrapper function
source(file.path('setup', 'argos_wrapper.R'))

###' `Set site name` ###
site <- 'my_site' ## if a site column exists in your CDM,
                  ## make sure this matches how it is represented there

# Establish connection to database
initialize_session(session_name = 'ndq_assessment',
                   db_conn = Sys.getenv('PEDSNET_BASE_CONFIG'),
                   is_json = TRUE,
                   cdm_schema = paste0(site, '_pedsnet'), ## replace with location of CDM data
                   results_schema = 'dqa_rox', ## replace with location of results schema
                                               ## MUST BE STORED ON SAME DATABASE AS CDM
                   retain_intermediates = FALSE,
                   db_trace = FALSE, ## set to TRUE for SQL code to print to the console (like verbose)
                   results_tag = '')

###' `Set additional configs` ###

config('qry_site', site)

config('current_version','v57') ## set current instantiation PEDSnet version

# location of prior CDM or results data (for data cycle changes check)
# config('db_src_prev', srcr(Sys.getenv('PEDSNET_PREV_CONFIG')))

# needed if executing check_dc and pointing to previous CDM instance
# config('cdm_schema_prev', 'dcc_pedsnet')
# needed if executing check_dc and pointing to previous results
# config('results_schema_prev', 'dqa_rox')

# config('previous_version','v55')

# Source cohort_* files
for (fn in list.files('code', 'cohort_.+\\.R', full.names = TRUE)){
  source(fn)
  }
rm(fn)
