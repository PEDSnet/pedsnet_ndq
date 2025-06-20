
#' Make sure the `pedsnet_ndq.Rproj` file is opened prior to executing this script
#' This will ensure the working directory is populated appropriately

# Load required packages
# devtools::install_github('PEDSnet/argos')
# devtools::install_github('PEDSnet/ndq')
# devtools::install_github('ssdqa/ssdqa.gen')
# install.packages('dbplyr')
# install.packages('DBI')
# install.packages('RPresto')
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
library(RPresto)

# Source file with wrapper function
source(file.path('setup', 'argos_wrapper.R'))

###' `Set site name` ###
site <- 'lurie' ## if a site column exists in your CDM,
                  ## make sure this matches how it is represented there

source(Sys.getenv('PEDSNET_TRINO_HTTR'))

# Establish connection to database
initialize_session(session_name = 'ndq_assessment',
                   db_conn = Sys.getenv('PEDSNET_TRINO_CONFIG'),
                   is_json = TRUE,
                   cdm_schema = 'pedsnet_dcc_v58', ## replace with location of CDM data
                   results_schema = 'ndq_trino_test', ## replace with location of results schema
                                               ## MUST BE STORED ON SAME DATABASE AS CDM
                   retain_intermediates = TRUE,
                   vocabulary_schema = 'v55_vocabulary',
                   db_trace = TRUE, ## set to TRUE for SQL code to print to the console (like verbose)
                   results_tag = '')

###' `Set additional configs` ###

config('qry_site', site)

config('current_version','v58') ## set current instantiation PEDSnet version

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

## RPresto edits
.sqlCreateTableAs <-  function(con, name, sql, with = NULL, ...) {
  name <- DBI::dbQuoteIdentifier(con, name)
  print(name)
  DBI::SQL(paste0(
    "CREATE TABLE ", name, "\n",
    if (!is.null(with)) paste0(with, "\n"),
    "AS\n",
    sql
  ))
}

setMethod("sqlCreateTableAs", signature("PrestoConnection"), .sqlCreateTableAs)

assignInNamespace(
  ".compute_tbl_presto",
  function(x, name, temporary = FALSE, ..., cte = FALSE) {
    if (rlang::is_bare_character(x) || dbplyr::is.ident(x) || dbplyr::is.sql(x)) {
      name <- unname(name)
    }
    if (identical(cte, TRUE)) {
      if (inherits(x$lazy_query, "lazy_base_remote_query")) {
        stop(
          "No operations need to be computed. Aborting compute.",
          call. = FALSE
        )
      }
      con <- dbplyr::remote_con(x)
      # We need to speicify sql_options here so that use_presto_cte is passed to
      # db_sql_render correctly
      # (see https://github.com/tidyverse/dbplyr/issues/1394)
      sql <- dbplyr::db_sql_render(
        con = dbplyr::remote_con(x), sql = x,
        sql_options = dbplyr::sql_options(), use_presto_cte = FALSE
      )
      con@session$addCTE(name, sql, replace = TRUE)
    } else {
      sql <- dbplyr::db_sql_render(
        dbplyr::remote_con(x), x, use_presto_cte = TRUE
      )
      name <- dbplyr::db_compute(
        dbplyr::remote_con(x), name, sql, temporary = temporary, ...
      )
    }
    name
  }, ns='RPresto'
)
