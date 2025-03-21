
################### PRECOMPUTE TABLES 1 ######################
source(file.path(getwd(), 'code', 'precompute_tables_1.R'))
##############################################################


## Data Cycle Changes

# dc_output <- check_dc(dc_tbl,
#                       omop_or_pcornet = 'omop',
#                       prev_db_string = 'v56',
#                       current_db_string = 'v57',
#                       prev_ct_src = 'cdm',
#                       prev_db = config('db_src_prev'),
#                       prev_rslt_tbl = 'dc_output',
#                       prev_rslt_schema = config('results_schema'),
#                       check_string = 'dc')
#
# output_tbl_append(dc_output, 'dc_output', file = TRUE)


## Vocabulary Conformance

vc_output <- check_vc(vc_tbl = read_codeset('pedsnet_vc_table', 'ccccc') %>%
                        filter(check_id != 'ml_cid'),
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

uc_output <- check_uc(uc_tbl = read_codeset('pedsnet_uc_table', 'ccccc') %>%
                        filter(!check_id %in% c('ml', 'mlu')),
                      by_year = FALSE,
                      produce_mapped_list=FALSE,
                      unmapped_values = c(44814650L,0L,
                                          44814653L, 44814649L),
                      check_string = 'uc')

output_tbl_append(uc_output, 'uc_output', file = TRUE)

## MF Visit ID

mf_output <- check_mf_visitid(mf_tbl = read_codeset('pedsnet_mf_table', 'ccccc') %>%
                                filter(check_id != 'ml'),
                              omop_or_pcornet = 'omop',
                              visit_tbl = cdm_tbl('visit_occurrence'),
                              check_string = 'mf_visitid')

output_tbl_append(mf_output, 'mf_visitid_output', file = TRUE)

## Best Mapped Concepts

bmc_output <- check_bmc(bmc_tbl = read_codeset('pedsnet_bmc_table', 'ccccc') %>%
                          filter(!grepl('fips', check_id)),
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
pf_output_all <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                            filter(!grepl('ml', check_id) & check_id != 'icu'),
                          visit_type_string = 'all',
                          omop_or_pcornet = 'omop',
                          visit_tbl=cdm_tbl('visit_occurrence'),
                          check_string='pf')

pf_output_ip <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                           filter(!grepl('ml', check_id)),
                         visit_type_string = 'inpatient',
                         omop_or_pcornet = 'omop',
                         visit_tbl=cdm_tbl('visit_occurrence') %>%
                           filter(visit_concept_id %in% c(9201L,2000000048L)),
                         check_string='pf')

pf_output_lip <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                            filter(!grepl('ml', check_id)),
                          visit_type_string = 'long_inpatient',
                          omop_or_pcornet = 'omop',
                          visit_tbl = results_tbl('ip_two'),
                          check_string='pf')

pf_output_op <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                           filter(!grepl('ml', check_id) & check_id != 'icu'),
                         visit_type_string = 'outpatient',
                         omop_or_pcornet = 'omop',
                         visit_tbl=cdm_tbl('visit_occurrence') %>%
                           filter(visit_concept_id %in% c(9202L,581399L)),
                         check_string='pf')

pf_output_ed <- check_pf(pf_tbl = read_codeset('pedsnet_pf_table', 'ccccc') %>%
                           filter(!grepl('ml', check_id) & check_id != 'icu'),
                         visit_type_string = 'emergency',
                         omop_or_pcornet = 'omop',
                         visit_tbl=cdm_tbl('visit_occurrence') %>%
                           filter(visit_concept_id %in% c(9203L,2000000048L)),
                         check_string='pf')

pf_combined <- pf_output_all %>%
  union(pf_output_ip) %>%
  union(pf_output_lip) %>%
  union(pf_output_op) %>%
  union(pf_output_ed)

output_tbl_append(pf_combined, 'pf_output')


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

dcon_combined <- dcon_output_pt %>%
  union(dcon_output_visit)

output_tbl_append(dcon_combined, 'dcon_output', file = TRUE)


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
                        time_span = list('2024-01-01', today()), ## CHANGE THIS
                        time_period = 'month',
                        lookback_months=1,
                        check_string = 'fot')

output_tbl_append(fot_output, 'fot_output', file = TRUE)

######### CLEANUP CHECKPOINT #################
remove_precompute(checkpoint = 3)
##############################################


