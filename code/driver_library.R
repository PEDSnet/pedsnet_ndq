
################### PRECOMPUTE TABLES 1 ######################
source(file.path(getwd(), 'code', 'precompute_tables_1.R'))
##############################################################


## Data Cycle Changes

dc_output <- check_dc(dc_tbl = read_codeset('pedsnet_dc_table', 'cccccc'),
                      omop_or_pcornet = 'omop',
                      prev_db_string = 'v58',
                      current_db_string = 'v58',
                      prev_ct_src = 'cdm',
                      prev_db = config('db_src'),
                      prev_rslt_tbl = NULL,
                      prev_rslt_schema = config('cdm_schema'),
                      check_string = 'dc')

output_tbl_append(dc_output$dc_cts, 'dc_output', file = TRUE)
output_tbl_append(dc_output$dc_meta, 'dc_meta', file = TRUE)

dc_mapping_file <- read_codeset("dc_mappings", 'cc')
output_tbl(dc_mapping_file, 'dc_mappings')

## Vocabulary Conformance

vc_output <- check_vc(vc_tbl = read_codeset('pedsnet_vc_table', 'ccccc'),
                      omop_or_pcornet = 'omop',
                      null_values = c(44814650L,0L,44814653L,44814649L),
                      check_string = 'vc')

output_tbl_append(vc_output, 'vc_output', file = TRUE)

## Valueset Conformance

vs_output <- check_vs(vs_tbl = read_codeset('pedsnet_vs_table', 'ccccc'),
                      omop_or_pcornet = 'omop',
                      null_values = c(44814650L,0L,44814653L,44814649L),
                      check_string = 'vs')

output_tbl_append(vs_output, 'vs_output', file = TRUE)

## Unmapped Concepts

uc_output <- check_uc(uc_tbl = read_codeset('pedsnet_uc_table', 'ccccc'),
                      by_year = FALSE,
                      produce_mapped_list = TRUE,
                      unmapped_values = c(44814650L,0L,
                                          44814653L, 44814649L, NA),
                      check_string = 'uc')

output_tbl_append(uc_output, 'uc_output', file = TRUE)

mapped_list <- results_tbl('uc_grpd')
output_tbl_append(mapped_list, 'uc_grpd', file = TRUE)

uc_output_year <- check_uc(uc_tbl = read_codeset('pedsnet_uc_table', 'ccccc'),
                           by_year = TRUE,
                           produce_mapped_list = FALSE,
                           unmapped_values = c(44814650L,0L,
                                               44814653L, 44814649L, NA),
                           check_string = 'uc')

output_tbl_append(uc_output_year, 'uc_by_year', file = TRUE)

## MF Visit ID

mf_output <- check_mf_visitid(mf_tbl = read_codeset('pedsnet_mf_table', 'ccccc'),
                              omop_or_pcornet = 'omop',
                              visit_tbl = cdm_tbl('visit_occurrence'),
                              check_string = 'mf_visitid')

output_tbl_append(mf_output, 'mf_visitid_output', file = TRUE)

## Best Mapped Concepts

bmc_output <- check_bmc(bmc_tbl = read_codeset('pedsnet_bmc_table', 'ccccc'),
                        omop_or_pcornet = 'omop',
                        concept_tbl = vocabulary_tbl('concept'),
                        check_string='bmc')

output_tbl_append(bmc_output$bmc_counts, 'bmc_output', file = TRUE)
output_tbl_append(bmc_output$bmc_concepts, 'bmc_concepts', file = TRUE)

## Expected Concepts Present

ecp_output <- check_ecp(ecp_tbl = read_codeset('pedsnet_ecp_table', 'ccccc') %>%
                          filter(!grepl('2010|2020', check_id)),
                        omop_or_pcornet = 'omop',
                        check_string = 'ecp')

output_tbl_append(ecp_output, 'ecp_output', file = TRUE)

######### CLEANUP CHECKPOINT #################
remove_precompute(checkpoint = 1)
##############################################

################### PRECOMPUTE TABLES 2 ######################
source(file.path(getwd(), 'code', 'precompute_tables_2.R'))
##############################################################

## Patient Facts
####### All Visits
pf_output_all <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                            filter(!grepl('ml', check_id) & check_id != 'icu'),
                          visit_type_filter = 'all',
                          visit_type_tbl = read_codeset('pedsnet_pf_visits', 'ci'),
                          omop_or_pcornet = 'omop',
                          visit_tbl=cdm_tbl('visit_occurrence'),
                          check_string='pf')
####### Inpatient Visits
pf_output_ip <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                           filter(!grepl('ml', check_id)),
                         visit_type_filter = 'inpatient',
                         visit_type_tbl = read_codeset('pedsnet_pf_visits', 'ci') %>%
                           select(-visit_source_concept_id),
                         omop_or_pcornet = 'omop',
                         visit_tbl=cdm_tbl('visit_occurrence') %>%
                           filter(visit_source_concept_id != 2000001590),
                         check_string='pf')
####### Inpatient Visits > 2 Days
pf_output_lip <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                            filter(!grepl('ml', check_id)),
                          visit_type_filter = 'long_inpatient',
                          visit_type_tbl = read_codeset('pedsnet_pf_visits', 'ci') %>%
                            select(-visit_source_concept_id),
                          omop_or_pcornet = 'omop',
                          visit_tbl = results_tbl('ip_two') %>%
                            filter(visit_source_concept_id != 2000001590),
                          check_string='pf')
####### Outpatient Visits
pf_output_op <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                           filter(!grepl('ml', check_id) & check_id != 'icu'),
                         visit_type_filter = 'outpatient',
                         visit_type_tbl = read_codeset('pedsnet_pf_visits', 'ci') %>%
                           select(-visit_source_concept_id),
                         omop_or_pcornet = 'omop',
                         visit_tbl=cdm_tbl('visit_occurrence') %>%
                           filter(visit_source_concept_id != 2000001590),
                         check_string='pf')
####### Emergency Department Visits
pf_output_ed <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                           filter(!grepl('ml', check_id) & check_id != 'icu'),
                         visit_type_filter = 'emergency',
                         visit_type_tbl = read_codeset('pedsnet_pf_visits', 'ci') %>%
                           select(-visit_source_concept_id),
                         omop_or_pcornet = 'omop',
                         visit_tbl=cdm_tbl('visit_occurrence') %>%
                           filter(visit_source_concept_id != 2000001590),
                         check_string='pf')
####### Cancelled Visits
pf_output_cld <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                            filter(!grepl('ml', check_id)),
                          visit_type_filter = 'cancelled',
                          visit_type_tbl = read_codeset('pedsnet_pf_visits', 'ci') %>%
                            select(-visit_concept_id),
                          omop_or_pcornet = 'omop',
                          visit_tbl=cdm_tbl('visit_occurrence'),
                          check_string='pf')

pf_combined <- pf_output_all %>%
  union(pf_output_ip) %>%
  union(pf_output_lip) %>%
  union(pf_output_op) %>%
  union(pf_output_ed) %>%
  union(pf_output_cld)

output_tbl_append(pf_combined, 'pf_output', file = TRUE)

# PF Mapping Descriptions (for Shiny app)
pf_mapping_file <- read_codeset('pf_mappings', 'cc')
output_tbl(pf_mapping_file, 'pf_mappings')


## Domain Concordance

dcon_output_pt <- check_dcon(dcon_tbl = read_codeset('pedsnet_dcon_table', 'cccccccccd') %>%
                               filter(!grepl('visit', check_id)),
                             compute_level = 'patient',
                             omop_or_pcornet = 'omop',
                             check_string='dcon')

dcon_output_visit <- check_dcon(dcon_tbl = read_codeset('pedsnet_dcon_table', 'cccccccccd') %>%
                                  filter(grepl('visit', check_id)),
                                compute_level = 'visit',
                                omop_or_pcornet = 'omop',
                                check_string='dcon')

dcon_combined <- dcon_output_pt[[1]] %>%
  union(dcon_output_visit[[1]])

dcon_meta <- dcon_output_pt[[2]] %>%
  union(dcon_output_visit[[2]])

output_tbl_append(dcon_combined, 'dcon_output', file = TRUE)
output_tbl(dcon_meta, 'dcon_meta', file = TRUE)


######### CLEANUP CHECKPOINT #################
remove_precompute(checkpoint = 2)
##############################################

################### PRECOMPUTE TABLES 3 ######################
source(file.path(getwd(), 'code', 'precompute_tables_3.R'))
##############################################################

## Facts Over Time

fot_output <- check_fot(fot_tbl = read_codeset('pedsnet_fot_table', 'cccc') %>%
                          filter(check_id != 'voml'),
                        omop_or_pcornet = 'omop',
                        compute_method = 'loop',
                        time_span = list('2009-01-01', today()),
                        time_period = 'month',
                        lookback_interval=1,
                        check_string = 'fot')

fot_visit_denom <- fot_output %>%
  filter(check_name == 'fot_vi') %>%
  select(site, time_end, time_start,
         row_cts, row_visits, row_pts) %>%
  rename('total_pt' = row_pts,
         'total_visit' = row_visits,
         'total_row' = row_cts)

fot_w_denom <- fot_output %>% left_join(fot_visit_denom)

output_tbl_append(fot_w_denom, 'fot_output', file = TRUE)

######### CLEANUP CHECKPOINT #################
remove_precompute(checkpoint = 3)
##############################################

# Output mapping table

check_type_mapping <- tibble('check_type_short' = c('dc', 'ecp', 'bmc', 'dcon',
                                                    'pf', 'mf_visitid', 'fot', 'uc',
                                                    'vc', 'vs'),
                             'check_type' = c('Data Cycle Changes', 'Expected Concepts Present',
                                              'Best Mapped Concepts', 'Domain Concordance',
                                              'Person Facts/Records', 'Missing Field: Visit ID', 'Facts Over Time',
                                              'Unmapped Concepts', 'Vocabulary Conformance',
                                              'Value Set Conformance'),
                             'check_category' = c('Consistency', 'Correctness', 'Correctness',
                                                  'Concordance', 'Completeness', 'Completeness',
                                                  'Consistency', 'Completeness', 'Conformance',
                                                  'Conformance'))

new_mapping <- create_check_metadata(check_tbls = c('dc_output', 'ecp_output',
                                                    'bmc_output', 'dcon_output',
                                                    'pf_output', 'mf_visitid_output',
                                                    'fot_output', 'uc_output',
                                                    'vc_output', 'vs_output'),
                                     metadata_file = read_codeset('dqa_check_descriptions', 'cc') %>%
                                       left_join(check_type_mapping) %>% mutate(check_type_long = check_type,
                                                                                check_type = check_type_short), #%>%
                                       # select(-c(check_type_short)),
                                     rslt_source = 'remote') %>% mutate(check_type = check_type_long) %>%
  select(-check_type_long)

output_tbl(new_mapping, 'dqa_check_metadata')
