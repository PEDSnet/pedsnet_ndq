
#' Make sure the `pedsnet_ndq.Rproj` file is opened prior to executing this script
#' This will ensure the working directory is populated appropriately

# Load required packages
# devtools::install_github('PEDSnet/argos')
# devtools::install_github('PEDSnet/ndq')
# devtools::install_github('ssdqa/ssdqa.gen')
library(srcr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DBI)
library(dbplyr)
library(lubridate)
library(ssdqa.gen)
library(argos)

# Source file with wrapper function
source(file.path('setup', 'argos_wrapper.R'))
source(file.path('setup', 'argos_edits.R'))

library(ndq)

###' `Set site name` ###
site <- 'lphi_10pct' ## if a site column exists in your CDM,
                  ## make sure this matches how it is represented there

# Establish connection to database
sf_cdm <- initialize_session(session_name = 'ndq_assessment',
                             db_conn = Sys.getenv('PEDSNET_SNOW_CONFIG'),
                             is_json = TRUE,
                             cdm_schema = 'UMO_SYNTHETIC_DATA_SHARE_VIEW', ## replace with location of CDM data
                             results_schema = 'TEST_SCHEMA',
                             retain_intermediates = FALSE,
                             db_trace = FALSE, ## set to TRUE for SQL code to print to the console (like verbose)
                             results_tag = '')

sf_rslt <- initialize_session(session_name = 'ndq_assessment',
                              db_conn = Sys.getenv('PEDSNET_RSLT_CONFIG'),
                              is_json = TRUE,
                              cdm_schema = 'UMO_SYNTHETIC_DATA_SHARE_VIEW', ## replace with location of CDM data
                              results_schema = 'TEST_SCHEMA',
                              retain_intermediates = FALSE,
                              db_trace = FALSE, ## set to TRUE for SQL code to print to the console (like verbose)
                              results_tag = '')

set_argos_default(sf_cdm)


###' `Set additional configs` ###

config('qry_site', site)
config('cdm_case', 'upper')

config('current_version','10pct_sample')

config('table_names', list(

  condition = 'CONDITION',
  death = 'DEATH',
  death_cause = 'DEATH_CAUSE',
  demographic = 'DEMOGRAPHIC',
  diagnosis = 'DIAGNOSIS',
  dispensing = 'DISPENSING',
  encounter = 'ENCOUNTER',
  enrollment = 'ENROLLMENT',
  harvest = 'HARVEST',
  hash_token = 'HASH_TOKEN',
  immunization = 'IMMUNIZATION',
  lab_result_cm = 'LAB_RESULT_CM',
  lds_address_history = 'LDS_ADDRESS_HISTORY',
  med_admin = 'MED_ADMIN',
  obs_clin = 'OBS_CLIN',
  obs_gen = 'OBS_GEN',
  pcornet_trial = 'PCORNET_TRIAL',
  prescribing = 'PRESCRIBING',
  pro_cm = 'PRO_CM',
  procedures = 'PROCEDURES',
  provider = 'PROVIDER',
  vital = 'VITAL'

))

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
