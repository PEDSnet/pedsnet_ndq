
## Data Cycle Changes
### thresholds
#### Standard Processing
dc_meta<-results_tbl('dc_meta')%>%collect()
dc_meta_local<-results_tbl('dc_meta')%>%select(check_name, check_description)%>%collect()
dc_output<-results_tbl('dc_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
dc_pp <- process_dc(dc_ct_results = dc_output,
                    dc_meta_results = dc_meta,
                    rslt_source = 'local')%>%
  left_join(dc_meta_local, by = 'check_name')


#### Detect Anomalies FOR (PLOTTING IN SHINY)
dc_anom <- squba.gen::compute_dist_anomalies(df_tbl=filter(dc_pp, site!='total'),
                                             grp_vars=c('check_type', 'application'),
                                             var_col='prop_total_change',
                                             denom_cols = NULL)

dc_anom_pp <- squba.gen::detect_outliers(df_tbl = dc_anom,
                                         tail_input = 'both',
                                         p_input = 0.9,
                                         column_analysis = 'prop_total_change',
                                         column_eligible = 'analysis_eligible',
                                         column_variable = 'application')

dc_anom_suppress <- dc_suppress_outlier(bind_rows(dc_anom_pp,
                                                  filter(dc_pp,site=='total')))

output_tbl(dc_anom_suppress, 'dc_output_pp')

### dashboard
#### Standard Processing
dc_output_d<-results_tbl('dc_output')%>%filter(site!='national_v63')%>%collect()%>%
  group_by(site, database_version, domain, check_name, check_type)%>%
  slice_min(n=1, order_by=total_ct, with_ties=FALSE)%>%
  ungroup()

dc_d_pp <- process_dc(dc_ct_results = dc_output_d,
                    dc_meta_results = dc_meta,
                    rslt_source = 'local')%>%
  left_join(dc_meta_local, by = 'check_name')


#### Detect Anomalies FOR (PLOTTING IN SHINY)
dc_anom_d <- squba.gen::compute_dist_anomalies(df_tbl=filter(dc_d_pp, site!='total'),
                                             grp_vars=c('check_type', 'application'),
                                             var_col='prop_total_change',
                                             denom_cols = NULL)

dc_anom_d_pp <- squba.gen::detect_outliers(df_tbl = dc_anom_d,
                                         tail_input = 'both',
                                         p_input = 0.9,
                                         column_analysis = 'prop_total_change',
                                         column_eligible = 'analysis_eligible',
                                         column_variable = 'application')

dc_anom_suppress_d <- dc_suppress_outlier(bind_rows(dc_anom_d_pp,
                                                  filter(dc_d_pp,site=='total')))

output_tbl(dc_anom_suppress_d, 'dc_output_d_pp')


## Vocabulary Conformance
### thresholds
vc_output<-results_tbl('vc_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
vc_pp <- process_vc(vc_results = vc_output,
                    rslt_source = 'local')

# adding "0" rows
vc_pp_viol_all<-vc_pp$vc_processed%>%
  distinct(site, table_application, measurement_column,
           check_type, check_name, total_denom_ct,
           check_description) %>%
  full_join(vc_pp$vc_violations)%>%
  mutate(tot_ct=case_when(is.na(tot_ct)~0,
                          TRUE~tot_ct),
         tot_prop=case_when(is.na(tot_prop)~0,
                            TRUE~tot_prop),
         check_name_app=paste0(check_name, "_rows"))

output_tbl(vc_pp$vc_processed, 'vc_output_pp')
output_tbl(vc_pp_viol_all, 'vc_violations_pp')

### dashboard
vc_d<-results_tbl('vc_output')%>%filter(site!='national_v63')%>%collect()

vc_d_pp <- process_vc(vc_results = vc_d,
                      rslt_source = 'local')
# adding "0" rows
vc_d_pp_viol_all<-vc_d_pp$vc_processed%>%
  distinct(site, table_application, measurement_column,
           check_type, check_name, total_denom_ct,
           check_description) %>%
  full_join(vc_d_pp$vc_violations)%>%
  mutate(tot_ct=case_when(is.na(tot_ct)~0,
                          TRUE~tot_ct),
         tot_prop=case_when(is.na(tot_prop)~0,
                            TRUE~tot_prop),
         check_name_app=paste0(check_name, "_rows"))

output_tbl(vc_d_pp$vc_processed, 'vc_output_d_pp')
output_tbl(vc_d_pp_viol_all, 'vc_violations_d_pp')



## Valueset Conformance
### for thresholds
vs_output<-results_tbl('vs_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
vs_pp <- process_vs(vs_results = vs_output,
                    rslt_source = 'local')
# adding "0" rows
vs_pp_viol_all<-vs_pp$vs_processed%>%
  distinct(site, table_application, measurement_column,
           check_type, check_name, total_denom_ct,
           check_description) %>%
  full_join(vs_pp$vs_violations)%>%
  mutate(tot_ct=case_when(is.na(tot_ct)~0,
                          TRUE~tot_ct),
         tot_prop=case_when(is.na(tot_prop)~0,
                            TRUE~tot_prop),
         check_name_app=paste0(check_name, "_rows"))

output_tbl(vs_pp$vs_processed, 'vs_output_pp')
output_tbl(vs_pp_viol_all, 'vs_violations_pp')

### for dashboard
vs_d<-results_tbl('vs_output')%>%filter(site!='national_v63')%>%collect()
vs_d_pp <- process_vs(vs_results = vs_d,
                      rslt_source = 'local')

# adding "0" rows
vs_d_pp_viol_all<-vs_d_pp$vs_processed%>%
  distinct(site, table_application, measurement_column,
           check_type, check_name, total_denom_ct,
           check_description) %>%
  full_join(vs_d_pp$vs_violations)%>%
  mutate(tot_ct=case_when(is.na(tot_ct)~0,
                          TRUE~tot_ct),
         tot_prop=case_when(is.na(tot_prop)~0,
                            TRUE~tot_prop),
         check_name_app=paste0(check_name, "_rows"))

output_tbl(vs_d_pp$vs_processed, 'vs_output_d_pp')
output_tbl(vs_d_pp_viol_all, 'vs_violations_d_pp')



## Unmapped Concepts
### thresholds
# Overall
uc_output<-results_tbl('uc_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
uc_pp <- process_uc(uc_results = uc_output,
                      rslt_source = 'local')

output_tbl(uc_pp, 'uc_output_pp')

# By Year
uc_year_output<-results_tbl('uc_by_year')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
uc_year_pp <- process_uc(uc_results = uc_year_output,
                           rslt_source = 'local')

output_tbl(uc_year_pp, 'uc_by_year_pp')

# Grouped concepts
uc_grpd<-results_tbl('uc_grpd')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))

output_tbl(uc_grpd, 'uc_grpd_pp')

### dashboard
# Overall
uc_d<-results_tbl('uc_output')%>%filter(site!='national_v63')%>%collect()
uc_d_pp <- process_uc(uc_results = uc_d,
                    rslt_source = 'local')

output_tbl(uc_d_pp, 'uc_output_d_pp')

# By Year
uc_year_d<-results_tbl('uc_by_year')%>%filter(site!='national_v63')%>%collect()
uc_year_d_pp <- process_uc(uc_results = uc_year_d,
                         rslt_source = 'local')

output_tbl(uc_year_d_pp, 'uc_by_year_d_pp')

# Grouped concepts
uc_grpd_d<-results_tbl('uc_grpd')%>%filter(site!='national_v63')%>%collect()

output_tbl(uc_grpd_d, 'uc_grpd_d_pp')



## MF Visit ID
### thresholds
mf_visitid<-results_tbl('mf_visitid_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
mf_visitid_pp <- process_mf_visitid(mf_visitid_results = mf_visitid,
                                    rslt_source = 'local')

output_tbl(mf_visitid_pp, 'mf_visitid_output_pp')

### dashboard
mf_visitid_d<-results_tbl('mf_visitid_output')%>%filter(site!='national_v63')%>%collect()
mf_visitid_d_pp <- process_mf_visitid(mf_visitid_results = mf_visitid_d,
                                    rslt_source = 'local')

output_tbl(mf_visitid_d_pp, 'mf_visitid_output_d_pp')

## Best Mapped Concepts
### thresholds
#### Standard Processing
bmc_output<-results_tbl('bmc_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
bmc_pp <- process_bmc(bmc_results = bmc_output,
                      rslt_source = 'local')
bmc_conceptlevel<-results_tbl('bmc_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))

output_tbl(bmc_pp, 'bmc_output_pp')
output_tbl(bmc_conceptlevel, 'bmc_output_concepts_pp')

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

### dashboard
#### Standard Processing
bmc_output_d<-results_tbl('bmc_output')%>%filter(site!='national_v63')%>%collect()
bmc_d_pp <- process_bmc(bmc_results = bmc_output_d,
                        rslt_source = 'local')
bmc_conceptlevel_d<-results_tbl('bmc_output')%>%filter(site!='national_v63')%>%collect()

output_tbl(bmc_d_pp, 'bmc_output_d_pp')
output_tbl(bmc_conceptlevel_d, 'bmc_output_concepts_d_pp')

#### Detect Anomalies
bmc_anom_d <- squba.gen::compute_dist_anomalies(df_tbl= bmc_d_pp%>%filter(best_notbest==1L),
                                              grp_vars=c('check_name', 'check_description',
                                                         'check_type', 'check_name_app',
                                                         'database_version'),
                                              var_col='best_row_prop',
                                              denom_cols = c('total_rows','check_name'))
bmc_anom_d_pp <- squba.gen::detect_outliers(df_tbl = bmc_anom_d,
                                          tail_input = 'both',
                                          p_input = 0.9,
                                          column_analysis = 'best_row_prop',
                                          column_eligible = 'analysis_eligible',
                                          column_variable = 'check_name')
output_tbl(bmc_anom_d_pp, 'bmc_anom_d_pp')

## Expected Concepts Present
### thresholds
#### Standard Processing
ecp_output <- results_tbl('ecp_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
ecp_pp <- process_ecp(ecp_results = ecp_output,
                      rslt_source = 'local')

ecp_pp_w_cat <- ecp_pp %>%
  left_join(read_codeset('ecp_cat_new', 'cc'))

output_tbl(ecp_pp_w_cat, 'ecp_output_pp')

#### Detect Anomalies
ecp_anom <- squba.gen::compute_dist_anomalies(df_tbl= ecp_pp_w_cat,
                                              grp_vars=c('check_name'),
                                              var_col='prop_with_concept',
                                              denom_cols = NULL)

ecp_anom_pp <- squba.gen::detect_outliers(df_tbl = ecp_anom,
                                          tail_input = 'both',
                                          p_input = 0.9,
                                          column_analysis = 'prop_with_concept',
                                          column_eligible = 'analysis_eligible',
                                          column_variable = 'check_name')
output_tbl(ecp_anom_pp, 'ecp_anom_pp')

### dashboard
#### Standard Processing
ecp_output_d <- results_tbl('ecp_output')%>%filter(site!='national_v63')%>%collect()
ecp_d_pp <- process_ecp(ecp_results = ecp_output,
                      rslt_source = 'local')

ecp_pp_w_cat_d <- ecp_d_pp %>%
  left_join(read_codeset('ecp_cat_new', 'cc'))

output_tbl(ecp_pp_w_cat_d, 'ecp_output_d_pp')

#### Detect Anomalies
ecp_anom_d <- squba.gen::compute_dist_anomalies(df_tbl= ecp_pp_w_cat_d,
                                              grp_vars=c('check_name'),
                                              var_col='prop_with_concept',
                                              denom_cols = NULL)

ecp_anom_d_pp <- squba.gen::detect_outliers(df_tbl = ecp_anom_d,
                                          tail_input = 'both',
                                          p_input = 0.9,
                                          column_analysis = 'prop_with_concept',
                                          column_eligible = 'analysis_eligible',
                                          column_variable = 'check_name')
output_tbl(ecp_anom_d_pp, 'ecp_anom_d_pp')

## Clinical Fact Documentation
### thresholds
cfd_output <- results_tbl('cfd_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
cfd_pp <- process_cfd(cfd_results = cfd_output,
                      rslt_source = 'local')

output_tbl(cfd_pp, 'cfd_output_pp')

### dashboard
cfd_output_d <- results_tbl('cfd_output')%>%filter(site!='national_v63')%>%collect()
cfd_d_pp <- process_cfd(cfd_results = cfd_output_d,
                        rslt_source = 'local')

output_tbl(cfd_d_pp, 'cfd_output_d_pp')

## Domain Concordance
### thresholds
dcon_output<-results_tbl('dcon_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
dcon_pp <- process_dcon(dcon_results = dcon_output,
                        rslt_source = 'local')

output_tbl(dcon_pp, 'dcon_output_pp')

### dashboard
dcon_output_d<-results_tbl('dcon_output')%>%filter(site!='national_v63')%>%collect()
dcon_d_pp <- process_dcon(dcon_results = dcon_output_d,
                        rslt_source = 'local')

output_tbl(dcon_d_pp, 'dcon_output_d_pp')

## Facts Over Time
### thresholds
fot_output<-results_tbl('fot_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
fot_pp <- process_fot(fot_results = fot_output,
                      target_col = 'row_cts',
                      add_ratios = TRUE,
                      ratio_mult = 10000,
                      rslt_source = 'local')

output_tbl(fot_pp$fot_heuristic_pp, 'fot_heuristic_pp')
output_tbl(fot_pp$fot_heuristic_summary_pp, 'fot_heuristic_summary_pp')
output_tbl(fot_pp$fot_ratios, 'fot_ratios_pp')

### dashboard
fot_output_d<-results_tbl('fot_output')%>%filter(site!='national_v63')%>%collect()
fot_d_pp <- process_fot(fot_results = fot_output_d,
                      target_col = 'row_cts',
                      add_ratios = TRUE,
                      ratio_mult = 10000,
                      rslt_source = 'local')

output_tbl(fot_d_pp$fot_heuristic_pp, 'fot_heuristic_d_pp')
output_tbl(fot_d_pp$fot_heuristic_summary_pp, 'fot_heuristic_summary_d_pp')
output_tbl(fot_d_pp$fot_ratios, 'fot_ratios_d_pp')

## Date Plausibility
### thresholds
dp_output<-results_tbl('dp_output')%>%filter(site!='national')%>%collect()%>%
  mutate(site=case_when(site=='national_v63'~'national',
                        TRUE~site))
dp_pp<-process_dp(dp_results = dp_output,
                  rslt_source='local')

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

### dashboard
dp_output_d<-results_tbl('dp_output')%>%filter(site!='national_v63')%>%collect()
dp_d_pp<-process_dp(dp_results = dp_output_d,
                  rslt_source='local')

output_tbl(dp_d_pp,
           name='dp_output_d_pp')

dp_anom_d <- squba.gen::compute_dist_anomalies(df_tbl= dp_d_pp,
                                             grp_vars=c('check_name', 'implausible_type', 'check_name_app'),
                                             var_col='prop_implausible',
                                             denom_cols = NULL)

dp_anom_d_pp <- squba.gen::detect_outliers(df_tbl=dp_anom_d,
                                         tail_input = 'both',
                                         p_input = 0.9,
                                         column_analysis = 'prop_implausible',
                                         column_eligible = 'analysis_eligible',
                                         column_variable =c('check_name', 'implausible_type', 'check_name_app'))
output_tbl(dp_anom_d_pp,
           'dp_anom_d_pp')

