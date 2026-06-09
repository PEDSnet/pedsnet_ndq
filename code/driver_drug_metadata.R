
site <- 'blah'
db_version <- 'v62'

source(Sys.getenv('PEDSNET_TRINO_HTTR'))

dm_conn <- argos$new('dm_dq')$init_session(db_src = srcr::srcr(Sys.getenv('PEDSNET_TRINO_CONFIG')),
                                           base_dir = getwd(),
                                           cdm_schema = paste0(site, '_pedsnet'),
                                           results_schema = 'dqa_rox',
                                           vocabulary_schema = 'v61_vocabulary',
                                           subdirs = list('specs' = 'specs_drugmeta',
                                                          'results' = 'results'),
                                           cdm = 'pedsnet',
                                           retain_intermediates = TRUE,
                                           cache_enabled = FALSE,
                                           results_name_tag = '')

set_argos_default(dm_conn)

config('qry_site', site)
config('current_version', db_version)

## Overall Metadata Presence
uc_drugmeta <- check_uc(uc_tbl = read_codeset('drugmeta_uc_table'),
                        omop_or_pcornet = 'omop',
                        by_year = FALSE,
                        produce_mapped_list = FALSE)

## Combinations of Metadata Fields
total_n <- uc_drugmeta %>% distinct(total_rows) %>% pull()

rqds <- cdm_tbl('drug_exposure') %>%
  filter(drug_type_concept_id == 38000177) %>%
  filter(is.na(refills) & is.na(quantity) & is.na(days_supply)) %>%
  group_by(site) %>%
  summarise(unmapped_rows = n()) %>%
  mutate(check_description = 'Prescription Refills, Quantity, and Days Supply',
         check_name = 'uc_drugs-refquantdays',
         database_version = db_version,
         check_type = 'uc',
         total_rows = total_n,
         unmapped_prop = round(as.numeric(unmapped_rows) / as.numeric(total_n), 2)) %>%
  collect()

dsdu <- cdm_tbl('drug_exposure') %>%
  filter(drug_type_concept_id == 38000177) %>%
  filter(is.na(effective_drug_dose) &
           dose_unit_concept_id %in% c(44814650L, 0L, 44814653L, 44814649L)) %>%
  group_by(site) %>%
  summarise(unmapped_rows = n()) %>%
  mutate(check_description = 'Prescription Dosage & Dose Unit',
         check_name = 'uc_drugs-dose_unit',
         database_version = db_version,
         check_type = 'uc',
         total_rows = total_n,
         unmapped_prop = round(as.numeric(unmapped_rows) / as.numeric(total_n), 2)) %>%
  collect()

uc_drugmeta %>%
  union(rqds) %>%
  union(dsdu) %>%
  postgres_session$output_tbl('uc_drug_metadata')


## Specific Drug Types (Plausibility)
uc_rslt <- list()
col_opt <- list()

drug_list <- c('insulin', 'rx_albuterol', 'rx_glp1ra_scdf', 'rx_fluoxetine_scdf', 'rx_bactrim',
               'rx_hydroxyurea', 'rx_raasi', 'rx_predniso_lo_ne_scdf', 'rx_methylphenidate_scdf',
               'rx_amoxicillin_scdf')

for(i in drug_list){

  cdst <- get_descendants(load_codeset(i))

  temp_drug_tbl <- cdm_tbl('drug_exposure') %>%
    filter(drug_type_concept_id == 38000177) %>%
    inner_join(cdst, by = c('drug_concept_id' = 'concept_id'))

  output_tbl(temp_drug_tbl, 'temp_drug_tbl')

  uc_drugmeta <- check_uc(uc_tbl = read_codeset('drugmeta_uc_table') %>%
                            mutate(table = 'temp_drug_tbl'),
                          omop_or_pcornet = 'omop',
                          by_year = FALSE,
                          produce_mapped_list = FALSE) %>%
    mutate(drug_type = i)

  total_n <- uc_drugmeta %>% distinct(total_rows) %>% pull()

  rqds <- results_tbl('temp_drug_tbl') %>%
    filter(is.na(refills) & is.na(quantity) & is.na(days_supply)) %>%
    group_by(site) %>%
    summarise(unmapped_rows = n()) %>%
    mutate(check_description = 'Prescription Refills, Quantity, and Days Supply',
           check_name = 'uc_drugs-refquantdays',
           database_version = db_version,
           check_type = 'uc',
           total_rows = total_n,
           unmapped_prop = round(as.numeric(unmapped_rows) / as.numeric(total_n), 2),
           drug_type = i) %>%
    collect()

  dsdu <- results_tbl('temp_drug_tbl') %>%
    filter(is.na(effective_drug_dose) &
             dose_unit_concept_id %in% c(44814650L, 0L, 44814653L, 44814649L)) %>%
    group_by(site) %>%
    summarise(unmapped_rows = n()) %>%
    mutate(check_description = 'Prescription Dosage & Dose Unit',
           check_name = 'uc_drugs-dose_unit',
           database_version = db_version,
           check_type = 'uc',
           total_rows = total_n,
           unmapped_prop = round(as.numeric(unmapped_rows) / as.numeric(total_n), 2),
           drug_type = i) %>%
    collect()

  uc_final <- uc_drugmeta %>%
    union(rqds) %>%
    union(dsdu)

  ## Numeric Range Summaries
  dose_range <- results_tbl('temp_drug_tbl') %>%
    mutate(dose_unit_concept_id = ifelse(dose_unit_concept_id %in% c(44814650L, 0L, 44814653L, 44814649L),
                                         0, dose_unit_concept_id)) %>%
    group_by(site, dose_unit_concept_id) %>%
    summarise(min_val = min(effective_drug_dose, na.rm = TRUE),
              max_val = max(effective_drug_dose, na.rm = TRUE),
              q1_val = approx_percentile(effective_drug_dose, 0.25),
              q3_val = approx_percentile(effective_drug_dose, 0.75),
              med_val = approx_percentile(effective_drug_dose, 0.5),
              mean_val = mean(effective_drug_dose, na.rm = TRUE),
              sd_val = sd(effective_drug_dose, na.rm = TRUE)) %>%
    mutate(drug_type = i,
           metadata_type = 'dose') %>% collect()

  col_list <- c('refills', 'quantity', 'days_supply', 'rx_length')

  col_rslt <- list()

  for(j in col_list){
    col_range <- results_tbl('temp_drug_tbl') %>%
      mutate(rx_length = date_diff('day', drug_start_date, drug_end_date)) %>%
      group_by(site) %>%
      summarise(min_val = min(!!sym(j), na.rm = TRUE),
                max_val = max(!!sym(j), na.rm = TRUE),
                q1_val = approx_percentile(!!sym(j), 0.25),
                q3_val = approx_percentile(!!sym(j), 0.75),
                med_val = approx_percentile(!!sym(j), 0.5),
                mean_val = mean(!!sym(j), na.rm = TRUE),
                sd_val = sd(!!sym(j), na.rm = TRUE)) %>%
      mutate(drug_type = i,
             metadata_type = j,
             dose_unit_concept_id = NA_integer_) %>% collect()

    col_rslt[[j]] <- col_range
  }

  col_int <- purrr::reduce(col_rslt,
                           dplyr::union)

  col_final <- dose_range %>%
    union(col_int)

  uc_rslt[[i]] <- uc_final
  col_opt[[i]] <- col_final

}

uc_spec_drug <- purrr::reduce(uc_rslt,
                              dplyr::union)

col_spec_drug <- purrr::reduce(col_opt,
                               dplyr::union)

postgres_session$output_tbl(uc_spec_drug, 'uc_example_drugs')
postgres_session$output_tbl(col_spec_drug, 'qd_example_drugs')
