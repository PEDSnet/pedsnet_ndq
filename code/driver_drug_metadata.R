
library(argos)
library(srcr)
library(ndq)
library(dplyr)

site_list <- c('seattle', 'stanford', 'national', 'nationwide',
               'nemours', 'lurie', 'cchmc', 'chop', 'colorado',
               'texas')

for(k in site_list){

## still have to do the cs and texas (not ready yet)
site_nm <- k
db_version <- 'v63'

source(Sys.getenv('PEDSNET_TRINO_HTTR'))

dm_conn <- argos$new('dm_dq')$init_session(db_src = srcr::srcr(Sys.getenv('PEDSNET_TRINO_CONFIG')),
                                           base_dir = getwd(),
                                           cdm_schema = paste0('pedsnet_dcc_', db_version),
                                           results_schema = 'dqa_rox',
                                           vocabulary_schema = paste0(db_version, '_vocabulary'),
                                           subdirs = list('specs' = 'specs_drugmeta',
                                                          'results' = 'results'),
                                           cdm = 'pedsnet',
                                           retain_intermediates = TRUE,
                                           cache_enabled = FALSE,
                                           results_name_tag = '')

pg_conn <- argos$new('pg_dq')$init_session(db_src = srcr::srcr(Sys.getenv('PEDSNET_BASE_CONFIG_NDQ')),
                                           base_dir = getwd(),
                                           cdm_schema = 'pedsnet_dcc_v63',
                                           results_schema = 'dqa_rox',
                                           vocabulary_schema = 'v61_vocabulary',
                                           subdirs = list('specs' = 'specs_drugmeta',
                                                          'results' = 'results'),
                                           cdm = 'pedsnet',
                                           retain_intermediates = TRUE,
                                           cache_enabled = FALSE,
                                           results_name_tag = '')

set_argos_default(dm_conn)

config('qry_site', site_nm)
config('current_version', db_version)

output_tbl_append <- function(data, name = NA, local = FALSE,
                              file = ifelse(pg_conn$config('results_target') !=
                                              'file', FALSE, TRUE),
                              db = ifelse(pg_conn$config('results_target') !=
                                            'file', TRUE, FALSE),
                              results_tag = TRUE, ...) {

  if (is.na(name)) name <- quo_name(enquo(data))

  if(db_exists_table(pg_conn$config('db_src'), name = intermed_name(name,
                                                            temporary = FALSE))) {

    tmp <- pg_conn$results_tbl(name) %>% collect()
    new_tbl <-
      dplyr::union(tmp,
                   data)
    pg_conn$output_tbl(data=new_tbl,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  } else {
    pg_conn$output_tbl(data=data,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  }


}

argos$public_methods$load_codeset <- function(name,
                                              col_types = 'iccc',
                                              table_name = name,
                                              indexes = list('concept_id'),
                                              full_path = FALSE,
                                              db = self$config('db_src'),
                                              .chunk_size = 5000) {

  if (self$config('cache_enabled')) {
    if (is.null(self$config('_codesets'))) self$config('_codesets', list())
    cache <- self$config('_codesets')
    if (! is.null(cache[[name]])) return(cache[[name]])
  }
  codes <-
    self$copy_to_new(db,
                     self$read_codeset(name, col_types = col_types,
                                       full_path = full_path),
                     name = table_name,
                     overwrite = TRUE,
                     indexes = indexes,
                     .chunk_size = .chunk_size)

  if (self$config('cache_enabled')) {
    cache[[name]] <- codes
    self$config('_codesets', cache)
  }

  codes
}

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

## Overall Metadata Presence
uc_drugmeta <- check_uc(uc_tbl = read_codeset('drugmeta_uc_table', 'cccc') %>%
                          filter(check_id == 'drugs-doseunits'),
                        omop_or_pcornet = 'omop',
                        by_year = FALSE,
                        produce_mapped_list = FALSE)

uc_drugmeta2 <- check_uc(uc_tbl = read_codeset('drugmeta_uc_table', 'cccc') %>%
                           filter(check_id != 'drugs-doseunits'),
                        unmapped_values = c(NA, ' '),
                        omop_or_pcornet = 'omop',
                        by_year = FALSE,
                        produce_mapped_list = FALSE)

uc_drugmeta <- uc_drugmeta %>% union(uc_drugmeta2)

## Combinations of Metadata Fields
total_n <- uc_drugmeta %>% distinct(total_rows) %>% pull()

rqds <- cdm_tbl('drug_exposure') %>%
  filter(drug_type_concept_id == 38000177,
         site == site_nm) %>%
  filter(!is.na(refills) & !is.na(quantity) & !is.na(frequency)) %>%
  group_by(site) %>%
  summarise(unmapped_rows = n()) %>%
  mutate(check_description = 'Prescription Refills, Quantity, and Frequency',
         check_name = 'uc_drugs-refquantfreq',
         database_version = db_version,
         check_type = 'uc',
         total_rows = as.numeric(total_n),
         unmapped_prop = 1 - round(as.numeric(unmapped_rows) / as.numeric(total_n), 2)) %>%
  collect()

dsdu <- cdm_tbl('drug_exposure') %>%
  filter(drug_type_concept_id == 38000177,
         site == site_nm) %>%
  filter(!is.na(effective_drug_dose) &
           !dose_unit_concept_id %in% c(44814650L, 0L, 44814653L, 44814649L)) %>%
  group_by(site) %>%
  summarise(unmapped_rows = n()) %>%
  mutate(check_description = 'Prescription Dosage & Dose Unit',
         check_name = 'uc_drugs-dose_unit',
         database_version = db_version,
         check_type = 'uc',
         total_rows = as.numeric(total_n),
         unmapped_prop = 1 - round(as.numeric(unmapped_rows) / as.numeric(total_n), 2)) %>%
  collect()

uc_drugmeta %>%
  union(rqds) %>%
  union(dsdu) %>%
  output_tbl_append('uc_drug_metadata')

sig_ref <- cdm_tbl('drug_exposure') %>%
  filter(drug_type_concept_id == 38000177) %>%
  select(site, sig, quantity, refills) %>%
  filter(!is.na(sig) & sig != ' ') %>%
  mutate(raw_sig = regexp_extract(sig, '(?<=SIG>)(.*?)(?=<\\/SIG)'),
         raw_quantity = regexp_extract(sig, '(?<=QUANTITY>)(.*?)(?=<\\/QUANTITY)'),
         raw_refills = regexp_extract(sig, '(?<=REFILLS>)(.*?)(?=<\\/REFILLS)')) %>%
  mutate(raw_sig = ifelse(raw_sig == '' | raw_sig == '@', NA_character_, raw_sig),
         raw_quantity = ifelse(raw_quantity == '' | raw_quantity == '@', NA_character_, raw_quantity),
         raw_refills = ifelse(raw_refills == '' | raw_refills == '@', NA_character_, raw_refills))

sig_total_n <- sig_ref %>% summarise(tot = n()) %>% pull(tot)

rawsig_ump <- sig_ref %>%
  filter(!is.na(raw_sig)) %>%
  summarise(unmapped_row = n()) %>%
  mutate(check_description = 'Parsed SIG Value',
         check_name = 'sig-raw_sig') %>% collect()

rawqt_ump <- sig_ref %>%
  filter(!is.na(raw_quantity)) %>%
  summarise(unmapped_row = n()) %>%
  mutate(check_description = 'Parsed Quantity Value',
         check_name = 'sig-raw_qt') %>% collect()

rawqt_qt_ump <- sig_ref %>%
  filter(!is.na(raw_quantity) & is.na(quantity)) %>%
  summarise(unmapped_row = n()) %>%
  mutate(check_description = 'Parsed Quantity is present where Quantity isnt',
         check_name = 'sig-struc_qt') %>% collect()

rawrf_ump <- sig_ref %>%
  filter(!is.na(raw_refills)) %>%
  summarise(unmapped_row = n()) %>%
  mutate(check_description = 'Parsed Refills Value',
         check_name = 'sig-raw_rf') %>% collect()

rawrf_rf_ump <- sig_ref %>%
  filter(!is.na(raw_refills) & is.na(refills)) %>%
  summarise(unmapped_row = n()) %>%
  mutate(check_description = 'Parsed Refill is present where Refill isnt',
         check_name = 'sig-struc_rf') %>% collect()

rawsig_ump %>%
  union(rawqt_ump) %>%
  union(rawqt_qt_ump) %>%
  union(rawrf_ump) %>%
  union(rawrf_rf_ump) %>%
  mutate(total_denom = as.numeric(sig_total_n),
         prop = unmapped_row / total_denom) %>%
  output_tbl('sig_parse_drug_meta')


## Specific Drug Types (Plausibility)
uc_rslt <- list()
col_opt <- list()

drug_list <- c('insulin', 'rx_albuterol', 'rx_glp1ra_scdf', 'rx_fluoxetine_scdf', 'rx_bactrim',
               'rx_hydroxyurea', 'rx_raasi', 'rx_predniso_lo_ne_scdf', 'rx_methylphenidate_scdf',
               'rx_amoxicillin_scdf')

for(i in drug_list){

  message(i)

  cdst <- get_descendants(load_codeset(i),
                          table_name = paste0(i, 'desc'))

  temp_drug_tbl <- cdm_tbl('drug_exposure') %>%
    filter(drug_type_concept_id == 38000177,
           site == site_nm) %>%
    inner_join(cdst, by = c('drug_concept_id' = 'concept_id'))

  output_tbl(temp_drug_tbl, 'temp_drug_tbl')

  uc_drugmeta <- check_uc(uc_tbl = read_codeset('drugmeta_uc_table', 'ccc') %>%
                            mutate(table = 'temp_drug_tbl',
                                   schema = 'result') %>%
                            filter(check_id == 'drugs-doseunits'),
                          omop_or_pcornet = 'omop',
                          by_year = FALSE,
                          produce_mapped_list = FALSE) %>%
    mutate(drug_type = i)

  uc_drugmeta2 <- check_uc(uc_tbl = read_codeset('drugmeta_uc_table', 'cccc') %>%
                            mutate(table = 'temp_drug_tbl',
                                   schema = 'result') %>%
                             filter(check_id != 'drugs-doseunits'),
                          unmapped_values = c(NA, ' '),
                          omop_or_pcornet = 'omop',
                          by_year = FALSE,
                          produce_mapped_list = FALSE) %>%
    mutate(drug_type = i)

  uc_drugmeta <- uc_drugmeta %>% union(uc_drugmeta2)

  total_n <- uc_drugmeta %>% distinct(total_rows) %>% pull()

  rqds <- results_tbl('temp_drug_tbl') %>%
    filter(!is.na(refills) & !is.na(quantity) & !is.na(frequency)) %>%
    group_by(site) %>%
    summarise(unmapped_rows = n()) %>%
    mutate(check_description = 'Prescription Refills, Quantity, and Frequency',
           check_name = 'uc_drugs-refquantfreq',
           database_version = db_version,
           check_type = 'uc',
           total_rows = as.numeric(total_n),
           unmapped_prop = 1 - round(as.numeric(unmapped_rows) / as.numeric(total_n), 2),
           drug_type = i) %>%
    collect()

  dsdu <- results_tbl('temp_drug_tbl') %>%
    filter(!is.na(effective_drug_dose) &
             !dose_unit_concept_id %in% c(44814650L, 0L, 44814653L, 44814649L)) %>%
    group_by(site) %>%
    summarise(unmapped_rows = n()) %>%
    mutate(check_description = 'Prescription Dosage & Dose Unit',
           check_name = 'uc_drugs-dose_unit',
           database_version = db_version,
           check_type = 'uc',
           total_rows = as.numeric(total_n),
           unmapped_prop = 1 - round(as.numeric(unmapped_rows) / as.numeric(total_n), 2),
           drug_type = i) %>%
    collect()

  uc_final <- uc_drugmeta %>%
    union(rqds) %>%
    union(dsdu)

  ## Numeric Range Summaries
  dose_range <- results_tbl('temp_drug_tbl') %>%
    mutate(dose_unit_concept_id = ifelse(dose_unit_concept_id %in% c(44814650L, 0L, 44814653L, 44814649L),
                                         0, dose_unit_concept_id)) %>%
    filter(!is.na(effective_drug_dose)) %>%
    group_by(site, dose_unit_concept_id) %>%
    summarise(min_val = as.numeric(min(effective_drug_dose)),
              max_val = as.numeric(max(effective_drug_dose)),
              q1_val = as.numeric(approx_percentile(effective_drug_dose, 0.25)),
              q3_val = as.numeric(approx_percentile(effective_drug_dose, 0.75)),
              med_val = as.numeric(approx_percentile(effective_drug_dose, 0.5)),
              mean_val = as.numeric(mean(effective_drug_dose)),
              sd_val = as.numeric(sd(effective_drug_dose))) %>%
    mutate(drug_type = i,
           dose_unit_concept_id = as.integer(dose_unit_concept_id),
           metadata_type = 'dose')

  low_whisk <- results_tbl('temp_drug_tbl') %>%
    mutate(dose_unit_concept_id = ifelse(dose_unit_concept_id %in% c(44814650L, 0L, 44814653L, 44814649L),
                                         0, dose_unit_concept_id)) %>%
    filter(!is.na(effective_drug_dose)) %>%
    group_by(site, dose_unit_concept_id) %>%
    left_join(dose_range %>% ungroup() %>% distinct(dose_unit_concept_id, q1_val, q3_val)) %>%
    filter(effective_drug_dose <= (q1_val - (1.5 * (q3_val - q1_val)))) %>%
    summarise(low_whisk = as.numeric(max(effective_drug_dose)))

  upper_whisk <- results_tbl('temp_drug_tbl') %>%
    mutate(dose_unit_concept_id = ifelse(dose_unit_concept_id %in% c(44814650L, 0L, 44814653L, 44814649L),
                                         0, dose_unit_concept_id)) %>%
    filter(!is.na(effective_drug_dose)) %>%
    group_by(site, dose_unit_concept_id) %>%
    left_join(dose_range %>% distinct(dose_unit_concept_id, q1_val, q3_val)) %>%
    filter(effective_drug_dose >= (q3_val + (1.5 * (q3_val - q1_val)))) %>%
    summarise(upper_whisk = as.numeric(min(effective_drug_dose)))

  dose_range_final <- dose_range %>%
    left_join(low_whisk) %>%
    left_join(upper_whisk) %>% collect()

  col_list <- c('refills', 'quantity', 'days_supply', 'rx_length')

  col_rslt <- list()

  for(j in col_list){
    col_range <- results_tbl('temp_drug_tbl') %>%
      mutate(rx_length = date_diff('day', drug_exposure_start_date, drug_exposure_end_date)) %>%
      filter(!is.na(!!sym(j))) %>%
      group_by(site) %>%
      summarise(min_val = as.numeric(min(!!sym(j))),
                max_val = as.numeric(max(!!sym(j))),
                q1_val = as.numeric(approx_percentile(!!sym(j), 0.25)),
                q3_val = as.numeric(approx_percentile(!!sym(j), 0.75)),
                med_val = as.numeric(approx_percentile(!!sym(j), 0.5)),
                mean_val = as.numeric(mean(!!sym(j))),
                sd_val = as.numeric(sd(!!sym(j)))) %>%
      mutate(drug_type = i,
             metadata_type = j,
             dose_unit_concept_id = NA_integer_)

    low_whisk <- results_tbl('temp_drug_tbl') %>%
      mutate(rx_length = date_diff('day', drug_exposure_start_date, drug_exposure_end_date),
             metadata_type = j) %>%
      filter(!is.na(!!sym(j))) %>%
      group_by(site) %>%
      left_join(col_range %>% distinct(metadata_type, q1_val, q3_val)) %>%
      filter(!!sym(j) <= (q1_val - (1.5 * (q3_val - q1_val)))) %>%
      summarise(low_whisk = as.numeric(max(!!sym(j))))

    upper_whisk <- results_tbl('temp_drug_tbl') %>%
      mutate(rx_length = date_diff('day', drug_exposure_start_date, drug_exposure_end_date),
             metadata_type = j) %>%
      filter(!is.na(!!sym(j))) %>%
      group_by(site) %>%
      left_join(col_range %>% distinct(metadata_type, q1_val, q3_val)) %>%
      filter(!!sym(j) >= (q3_val + (1.5 * (q3_val - q1_val)))) %>%
      summarise(upper_whisk = as.numeric(min(!!sym(j))))

    col_range_final <- col_range %>%
      left_join(low_whisk) %>%
      left_join(upper_whisk) %>% collect()

    col_rslt[[j]] <- col_range_final
  }

  col_int <- purrr::reduce(col_rslt,
                           dplyr::union)

  col_final <- dose_range_final %>%
    union(col_int)

  uc_rslt[[i]] <- uc_final
  col_opt[[i]] <- col_final

}

uc_spec_drug <- purrr::reduce(uc_rslt,
                              dplyr::union)

col_spec_drug <- purrr::reduce(col_opt,
                               dplyr::union)

output_tbl_append(uc_spec_drug, 'uc_example_drugs')
output_tbl_append(col_spec_drug, 'qd_example_drugs')
}
