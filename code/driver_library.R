
################### PRECOMPUTE TABLES 1 ######################
source(file.path(getwd(), 'code', 'precompute_tables_1.R'))
##############################################################


## Data Cycle Changes

set_argos_default(sf_cdm)
dc_output <- check_dc(dc_tbl = read_codeset('pcornet_dc_table', 'cccccc'), #%>%
                        #filter(check_id != 'immsall'),
                      omop_or_pcornet = 'pcornet',
                      prev_db_string = '10pct_sample',
                      current_db_string = '10pct_sample',
                      prev_ct_src = 'cdm',
                      prev_db = sf_cdm$config('db_src'),
                      prev_rslt_tbl = 'dc_output',
                      prev_rslt_schema = sf_rslt$config('results_schema'),
                      check_string = 'dc')

output_tbl_append(dc_output$dc_cts,
                  'dc_output')
output_tbl_append(dc_output$dc_meta,
                  'dc_meta')

dc_mapping_file <- read_codeset("dc_mappings", 'cc')
output_tbl(dc_mapping_file, 'dc_mappings')

## Vocabulary Conformance -- no concept table on snowflake rn

# vc_output <- check_vc(vc_tbl = read_codeset('pedsnet_vc_table', 'ccccc') %>%
#                         filter(check_id != 'labsall-cid'),
#                       omop_or_pcornet = 'omop',
#                       null_values = c(44814650L,0L,44814653L,44814649L),
#                       check_string = 'vc')
#
# output_tbl_append(vc_output, 'vc_output', file = TRUE)

## Valueset Conformance

vs_output <- check_vs(vs_tbl = read_codeset('pcornet_vs_table', 'ccccc'),
                      omop_or_pcornet = 'pcornet',
                      null_values = c('NI', 'OT', 'UN'),
                      concept_tbl = NULL,
                      check_string = 'vs')

output_tbl_append(vs_output, 'vs_output')

## Unmapped Concepts

## no obsgen table, dropped gestational age cohort
uc_output <- check_uc(uc_tbl = read_codeset('pcornet_uc_table', 'ccccc') %>%
                        filter(check_id != 'gestage'),
                      omop_or_pcornet = 'pcornet',
                      by_year = FALSE,
                      produce_mapped_list = FALSE, ## not running this for now -- need to investigate a solution
                      unmapped_values = c('NI', 'OT', 'UN'),
                      check_string = 'uc')

uc_output <- uc_output %>% mutate(unmapped_prop = ifelse(is.na(unmapped_prop), 0, unmapped_prop))

output_tbl_append(uc_output, 'uc_output')

# mapped_list <- results_tbl('uc_grpd')
# output_tbl(mapped_list, 'uc_grpd', file = TRUE)

uc_output_year <- check_uc(uc_tbl = read_codeset('pcornet_uc_table', 'ccccc') %>%
                             filter(check_id != 'gestage'),
                           omop_or_pcornet = 'pcornet',
                           by_year = TRUE,
                           produce_mapped_list = FALSE,
                           unmapped_values = c('NI', 'OT', 'UN'),
                           check_string = 'uc')

output_tbl_append(uc_output_year, 'uc_by_year')

## MF Visit ID

mf_output <- check_mf_visitid(mf_tbl = read_codeset('pcornet_mf_table', 'ccccc'), #%>%
                                #filter(check_id != 'immsall-visitid'),
                              omop_or_pcornet = 'pcornet',
                              visit_tbl = cdm_tbl('encounter'),
                              check_string = 'mf_visitid')

output_tbl_append(mf_output, 'mf_visitid_output')

## Best Mapped Concepts

## didnt sent provider table
bmc_output <- check_bmc(bmc_tbl = read_codeset('pcornet_bmc_table', 'ccccc') %>%
                          filter(!grepl('fips|gestage|drug', check_id)),
                        best_notbest_tbl = read_codeset('pcornet_bmc_bestnotbest', 'ccic'),
                        omop_or_pcornet = 'pcornet',
                        concept_tbl = NULL,
                        check_string='bmc')

# bmc_output2 <- check_bmc(bmc_tbl = read_codeset('pcornet_bmc_table', 'ccccc') %>%
#                           filter(grepl('fips', check_id)), ## dropped gestage cht for now
#                         omop_or_pcornet = 'omop',
#                         concept_tbl = NULL,
#                         check_string='bmc')

# bmc_counts_final <- bmc_output$bmc_counts #%>% union(bmc_output2$bmc_counts)

output_tbl_append(bmc_output, 'bmc_output')

## Expected Concepts Present

ecp_output <- check_ecp(ecp_tbl = read_codeset('pcornet_ecp_table', 'ccccc') %>%
                          filter(!grepl('state|county', check_id),
                                 #cohort_table != 'nephrology_specialties',
                                 cohort_table != 'geocode_cohort'),
                        omop_or_pcornet = 'pcornet',
                        check_string = 'ecp')

output_tbl_append(ecp_output, 'ecp_output')

######### CLEANUP CHECKPOINT #################
remove_precompute(checkpoint = 1)
##############################################

################### PRECOMPUTE TABLES 2 ######################
source(file.path(getwd(), 'code', 'precompute_tables_2.R'))
##############################################################

## Clinical Fact Documentation
####### All Visits
cfd_output_all <- check_cfd(cfd_tbl = read_codeset('pcornet_cfd_table', 'ccccc') %>%
                            filter(check_id != 'icu'),
                          visit_type_filter = 'all',
                          visit_type_tbl = read_codeset('pcornet_cfd_visits', 'cc'),
                          omop_or_pcornet = 'pcornet',
                          visit_tbl=cdm_tbl('encounter'),
                          check_string='cfd') %>%
  mutate(check_name = stringr::str_replace_all(check_name, '_all', ''))
####### Inpatient Visits
cfd_output_ip <- check_cfd(cfd_tbl = read_codeset('pcornet_cfd_table', 'ccccc'), #%>%
                           #filter(check_id != 'immsall'),
                         visit_type_filter = 'inpatient',
                         visit_type_tbl = read_codeset('pcornet_cfd_visits', 'cc'),
                         omop_or_pcornet = 'pcornet',
                         visit_tbl=cdm_tbl('encounter'),
                         check_string='cfd') %>%
  mutate(check_name = stringr::str_replace_all(check_name, '_inpatient', '-visip'))

####### Inpatient Visits > 2 Days
cfd_output_lip <- check_cfd(cfd_tbl = read_codeset('pcornet_cfd_table', 'ccccc'), #%>%
                              #filter(check_id != 'immsall'),
                          visit_type_filter = 'long_inpatient',
                          visit_type_tbl = read_codeset('pcornet_cfd_visits', 'cc'),
                          omop_or_pcornet = 'pcornet',
                          visit_tbl = results_tbl('ip_two'),
                          check_string='cfd') %>%
  mutate(check_name = stringr::str_replace_all(check_name, '_long_inpatient', '-visiplong'))

####### Outpatient Visits
cfd_output_op <- check_cfd(cfd_tbl = read_codeset('pcornet_cfd_table', 'ccccc') %>%
                             filter(check_id != 'icu'),
                         visit_type_filter = 'outpatient',
                         visit_type_tbl = read_codeset('pcornet_cfd_visits', 'cc'),
                         omop_or_pcornet = 'pcornet',
                         visit_tbl=cdm_tbl('encounter'),
                         check_string='cfd') %>%
  mutate(check_name = stringr::str_replace_all(check_name, '_outpatient', '-visop'))

####### Primary Care Visits (Specialty)
cfd_output_pc <- check_cfd(cfd_tbl = read_codeset('pcornet_cfd_table', 'ccccc') %>%
                             filter(check_id != 'icu'),
                         visit_type_filter = 'primary_care',
                         visit_type_tbl = read_codeset('pcornet_cfd_visits', 'cc'),
                         omop_or_pcornet = 'pcornet',
                         visit_tbl=results_tbl('gp_specialties'),
                         check_string='cfd') %>%
  mutate(check_name = stringr::str_replace_all(check_name, '_primary_care', '-vispc'))

####### Emergency Department Visits
cfd_output_ed <- check_cfd(cfd_tbl = read_codeset('pcornet_cfd_table', 'ccccc') %>%
                             filter(check_id != 'icu'),
                         visit_type_filter = 'emergency',
                         visit_type_tbl = read_codeset('pcornet_cfd_visits', 'cc'),
                         omop_or_pcornet = 'pcornet',
                         visit_tbl=cdm_tbl('encounter'),
                         check_string='cfd') %>%
  mutate(check_name = stringr::str_replace_all(check_name, '_emergency', '-vised'))

####### Cancelled Visits
# cfd_output_cld <- check_cfd(cfd_tbl = read_codeset('pcornet_cfd_table', 'ccccc') %>%
#                               filter(check_id != 'icu', check_id != 'immsall'),
#                           visit_type_filter = 'cancelled',
#                           visit_type_tbl = read_codeset('pedsnet_cfd_visits', 'ci') %>%
#                             select(-visit_concept_id),
#                           omop_or_pcornet = 'omop',
#                           visit_tbl=cdm_tbl('visit_occurrence'),
#                           check_string='cfd') %>%
#   mutate(check_name = stringr::str_replace_all(check_name, '_cancelled', '-viscn'))

cfd_combined <- cfd_output_all %>%
  union(cfd_output_ip) %>%
  union(cfd_output_lip) %>%
  union(cfd_output_op) %>%
  union(cfd_output_ed) %>%
  #union(cfd_output_cld) #%>%
  union(cfd_output_pc)

output_tbl_append(cfd_combined, 'cfd_output')

# cfd Mapping Descriptions (for Shiny app)
cfd_mapping_file <- read_codeset('cfd_mappings', 'cc')
output_tbl(cfd_mapping_file, 'cfd_mappings')


## Domain Concordance

dcon_output_pt <- check_dcon(dcon_tbl = read_codeset('pcornet_dcon_table', 'cccccccccd') %>%
                               filter(!grepl('vis', check_id)),
                             compute_level = 'patient',
                             omop_or_pcornet = 'pcornet',
                             check_string='dcon')

dcon_output_visit <- check_dcon(dcon_tbl = read_codeset('pcornet_dcon_table', 'cccccccccd') %>%
                                  filter(grepl('vis', check_id)),
                                compute_level = 'visit',
                                omop_or_pcornet = 'pcornet',
                                check_string='dcon')

dcon_combined <- dcon_output_pt[[1]] %>%
  union(dcon_output_visit[[1]])

dcon_meta <- dcon_output_pt[[2]] %>%
  union(dcon_output_visit[[2]])

output_tbl_append(dcon_combined, 'dcon_output')
sf_rslt$output_tbl(dcon_meta, 'dcon_meta')

## Date Plausibility

dp_output <- check_dp(dp_tbl = read_codeset('pcornet_dp_table', 'ccccc'),
                      omop_or_pcornet = 'pcornet',
                      visit_tbl = cdm_tbl('encounter'),
                      dob_tbl = cdm_tbl('demographic'),
                      death_tbl = cdm_tbl('death'),
                      post_death_buffer = 30L,
                      check_string = 'dp')

output_tbl_append(dp_output, 'dp_output')

######### CLEANUP CHECKPOINT #################
remove_precompute(checkpoint = 2)
##############################################

################### PRECOMPUTE TABLES 3 ######################
source(file.path(getwd(), 'code', 'precompute_tables_3.R'))
##############################################################

## Facts Over Time

fot_output <- check_fot(fot_tbl = read_codeset('pcornet_fot_table', 'cccc') %>%
                          filter(table != 'c19_imm', !grepl('sdoh|promis', check_id)),
                        omop_or_pcornet = 'pcornet',
                        compute_method = 'group',
                        time_span = list('2009-01-01', today()),
                        time_period = 'month',
                        lookback_interval=1,
                        check_string = 'fot')

fot_visit_denom <- fot_output %>%
  filter(check_name == 'fot_visall') %>%
  select(site, time_end, time_start,
         row_cts, row_visits, row_pts) %>%
  rename('total_pt' = row_pts,
         'total_visit' = row_visits,
         'total_row' = row_cts)

fot_w_denom <- fot_output %>% left_join(fot_visit_denom)

output_tbl_append(fot_w_denom, 'fot_output')

######### CLEANUP CHECKPOINT #################
remove_precompute(checkpoint = 3)
##############################################

# Output mapping table

check_type_mapping <- tibble('check_type_short' = c('dc', 'ecp', 'bmc', 'dcon',
                                                    'cfd', 'mf_visitid', 'fot', 'uc',
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
                                                    'cfd_output', 'mf_visitid_output',
                                                    'fot_output', 'uc_output',
                                                    'vc_output', 'vs_output'),
                                     metadata_file = read_codeset('dqa_check_descriptions', 'cc') %>%
                                       left_join(check_type_mapping) %>% mutate(check_type_long = check_type,
                                                                                check_type = check_type_short), #%>%
                                       # select(-c(check_type_short)),
                                     rslt_source = 'remote') %>% mutate(check_type = check_type_long) %>%
  select(-check_type_long)

output_tbl(new_mapping, 'dqa_check_metadata')
