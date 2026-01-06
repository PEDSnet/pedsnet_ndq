
dc_output_newdomains<-results_tbl('dc_output')%>%
  filter(database_version=='v60')%>%
  select(check_name, domain)
dc_output_local<-results_tbl('dc_output')%>%
  select(-domain)%>%
  left_join(dc_output_newdomains, by = 'check_name')%>%
  collect()
dc_meta_local<-results_tbl('dc_meta')%>%collect()
## Data Cycle Changes
#### Standard Processing
dc_pp <- process_dc(dc_ct_results = dc_output_local,
                    dc_meta_results = dc_meta_local,
                    rslt_source = 'local')

# output_tbl(dc_pp, 'dc_output_pp')

#### Detect Anomalies FOR (PLOTTING IN SHINY)
dc_anom <- squba.gen::compute_dist_anomalies(df_tbl=filter(dc_pp, site!='total'),
                                             grp_vars=c('check_type', 'application'),
                                             var_col='prop_total_change',
                                             denom_cols = NULL)

dc_anom_pp <- ssdqa.gen::detect_outliers(df_tbl = dc_anom,
                                         tail_input = 'both',
                                         p_input = 0.9,
                                         column_analysis = 'prop_total_change',
                                         column_eligible = 'analysis_eligible',
                                         column_variable = 'application')

# For dev run, no outliers
# dc_anom_suppress <- dc_suppress_outlier(bind_rows(dc_anom_pp,
#                                                   filter(dc_pp,site=='total')))
dc_anom_suppress<-dc_anom_pp%>%
  bind_rows(filter(dc_pp,site=='total'))%>%
  mutate(plot_prop=prop_total_change)

output_tbl(dc_anom_suppress, 'dc_output_pp')

## Vocabulary Conformance

vc_pp <- process_vc(vc_results = 'vc_output',
                    rslt_source = 'remote')

output_tbl(vc_pp$vc_processed, 'vc_output_pp')
output_tbl(vc_pp$vc_violations, 'vc_violations')

## Valueset Conformance

vs_pp <- process_vs(vs_results = 'vs_output',
                    rslt_source = 'remote')

output_tbl(vs_pp$vs_processed, 'vs_output_pp')
output_tbl(vs_pp$vs_violations, 'vs_violations')

## Unmapped Concepts

# Overall
uc_pp <- process_uc(uc_results = 'uc_output',
                    rslt_source = 'remote')

output_tbl(uc_pp, 'uc_output_pp')

# By Year
uc_year_pp <- process_uc(uc_results = 'uc_by_year',
                         rslt_source = 'remote')

output_tbl(uc_year_pp, 'uc_by_year_pp')

# Grouped concepts
uc_grpd<-results_tbl('uc_grpd')%>%collect()
output_tbl(uc_grpd, 'uc_grpd_pp')

## MF Visit ID

mf_visitid_pp <- process_mf_visitid(mf_visitid_results = 'mf_visitid_output',
                                    rslt_source = 'remote')

output_tbl(mf_visitid_pp, 'mf_visitid_output_pp')

## Best Mapped Concepts
#### Standard Processing
bmc_pp <- process_bmc(bmc_results = 'bmc_output',
                      rslt_source = 'remote')
bmc_concepts_pp<-results_tbl('bmc_concepts')%>%
  collect()%>%
  rename(check_name_prev=check_name)%>%
  mutate(check_name=paste0('bmc_',check_name_prev))%>%
  select(-check_name_prev)
output_tbl(bmc_concepts_pp,
           name='bmc_concepts_rev')

#output_tbl(bmc_pp$bmc_output_pp, 'bmc_output_pp')
#output_tbl(bmc_pp$bmc_concepts_pp, 'bmc_output_concepts_pp')
output_tbl(bmc_pp, 'bmc_output_pp')
output_tbl(bmc_concepts_pp, 'bmc_output_concepts_pp')

#### Detect Anomalies
bmc_anom <- squba.gen::compute_dist_anomalies(df_tbl= bmc_pp%>%filter(best_notbest==1L),
                                              grp_vars=c('check_name', 'check_description',
                                                         'check_type', 'check_name_app',
                                                         'database_version'),
                                              var_col='best_row_prop',
                                              denom_cols = c('total_rows','check_name'))

bmc_anom_pp <- squba.gen::detect_outliers(df_tbl = bmc_anom,
                                          tail_input = 'both',
                                          p_input = 0.9,
                                          column_analysis = 'best_row_prop',
                                          column_eligible = 'analysis_eligible',
                                          column_variable = 'check_name')
output_tbl(bmc_anom_pp, 'bmc_anom_pp')

## Expected Concepts Present
#### Standard Processing
ecp_pp <- process_ecp(ecp_results = 'ecp_output',
                      rslt_source = 'remote')

ecp_pp_labs <- ecp_pp %>%
  left_join(read_codeset('ecp_cat_new_new', 'cc'))

output_tbl(ecp_pp_labs, 'ecp_output_pp')

#### Detect Anomalies
ecp_anom <- squba.gen::compute_dist_anomalies(df_tbl= ecp_pp_labs,
                                              grp_vars=c('check_name'),
                                              var_col='prop_with_concept',
                                              denom_cols = NULL)

ecp_anom_pp <- ssdqa.gen::detect_outliers(df_tbl = ecp_anom,
                                          tail_input = 'both',
                                          p_input = 0.9,
                                          column_analysis = 'prop_with_concept',
                                          column_eligible = 'analysis_eligible',
                                          column_variable = 'check_name')
output_tbl(ecp_anom_pp, 'ecp_anom_pp')

## Patient Facts

cfd_pp <- process_cfd(cfd_results = 'cfd_output',
                      rslt_source = 'remote')

output_tbl(cfd_pp, 'cfd_output_pp')

## Domain Concordance

dcon_pp <- process_dcon(dcon_results = 'dcon_output',
                        rslt_source = 'remote')

dcon_pp_labs <- dcon_pp %>%
  mutate(description_full = gsub('and', '/', check_description))

output_tbl(dcon_pp_labs, 'dcon_output_pp')

## Facts Over Time

fot_pp <- process_fot(fot_results = 'fot_output',
                      target_col = 'row_cts',
                      add_ratios = TRUE,
                      ratio_mult = 10000,
                      rslt_source = 'remote')

output_tbl(fot_pp$fot_heuristic_pp, 'fot_heuristic_pp')
output_tbl(fot_pp$fot_heuristic_summary_pp, 'fot_heuristic_summary_pp')
output_tbl(fot_pp$fot_ratios, 'fot_ratios_pp')

## Date Plausibility
dp_pp<-process_dp(dp_results = 'dp_output',
                  rslt_source='remote')
# dp_pp_desc<-dp_pp%>%
#   left_join(read_codeset('dp_name_desc', col_types='cccc'), by = c('check_description', 'check_name', 'implausible_type'))
output_tbl(dp_pp,
           name='dp_output_pp')

dp_anom <- squba.gen::compute_dist_anomalies(df_tbl= dp_pp,
                                             grp_vars=c('check_name', 'implausible_type', 'check_name_app'),
                                             var_col='prop_implausible',
                                             denom_cols = NULL)

dp_anom_pp <- squba.gen::detect_outliers(df_tbl=dp_anom,
                                         tail_input = 'both',
                                         p_input = 0.9,
                                         column_analysis = 'prop_implausible',
                                         column_eligible = 'analysis_eligible',
                                         column_variable =c('check_name', 'implausible_type', 'check_name_app'))
output_tbl(dp_anom_pp,
           'dp_anom_pp')

