
#' Make sure the `pedsnet_ndq.Rproj` file is opened prior to executing this script
#' This will ensure the working directory is populated appropriately

# Load required packages
# devtools::install_github('PEDSnet/argos')
# devtools::install_github('PEDSnet/ndq')
# devtools::install_github('ssdqa/squba.gen')
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
library(squba.gen)
# library(RPresto)
library(RPostgres)

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
                   results_schema = 'my_dq_schema', ## replace with location of results schema
                                                    ## MUST BE STORED ON SAME DATABASE AS CDM
                   vocabulary_schema = 'vocabulary',
                   retain_intermediates = FALSE,
                   db_trace = FALSE, ## set to TRUE for SQL code to print to the console (like verbose)
                   results_tag = '')

###' `Set additional configs` ###

argos::config('qry_site', site)

argos::config('current_version','v58') ## set current instantiation PEDSnet version

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


assignInNamespace(x = 'get_data_type',
                  value = get_data_type <- function(obj){
                    if (is.factor(obj)) return("TEXT")
                    if (inherits(obj, "POSIXt")) return("TIMESTAMPTZ")
                    if (inherits(obj, "Date")) return("DATE")
                    if (inherits(obj, "difftime")) return("TIME")
                    if (inherits(obj, "integer64")) return("BIGINT")
                    switch(typeof(obj),
                           integer = "INTEGER",
                           double = "DOUBLE PRECISION",
                           character = "VARCHAR",
                           logical = "BOOLEAN",
                           list = "BYTEA",
                           stop("Unsupported type", call. = FALSE)
                    )
                  },
                  ns = 'RPostgres')
