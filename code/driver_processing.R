# NEW label replacement for v58:
dashboard_labels<-read_codeset('dashboard_labels',col_types = 'ccc')

## Data Cycle Changes
#### Standard Processing
dc_pp <- process_dc(dc_ct_results = 'dc_output',
                    dc_meta_results = 'dc_meta',
                    rslt_source = 'remote')

# output_tbl(dc_pp, 'dc_output_pp')

#### Detect Anomalies FOR (PLOTTING IN SHINY)
dc_anom <- squba.gen::compute_dist_anomalies(df_tbl=filter(dc_pp, site!='total')%>%select(-old_check_name),
                                             grp_vars=c('check_type', 'application'),
                                             var_col='prop_total_change',
                                             denom_cols = NULL)

dc_anom_pp <- ssdqa.gen::detect_outliers(df_tbl = dc_anom,
                                         tail_input = 'both',
                                         p_input = 0.9,
                                         column_analysis = 'prop_total_change',
                                         column_eligible = 'analysis_eligible',
                                         column_variable = 'application')

dc_anom_suppress <- dc_suppress_outlier(bind_rows(dc_anom_pp,
                                                  filter(dc_pp%>%select(-old_check_name),site=='total')))

dc_pp_newnames<-add_desc(tbl=dc_anom_suppress,
                         join_col_name='domain',
                         check_type_name='dc',
                         label_file = dashboard_labels)

output_tbl(dc_pp_newnames, 'dc_output_pp')

## Vocabulary Conformance

vc_pp <- process_vc(vc_results = 'vc_output',
                    rslt_source = 'remote')

output_tbl(vc_pp$vc_processed, 'vc_output_pp')
output_tbl(vc_pp$vc_violations, 'vc_violations_pp')

## Valueset Conformance

vs_pp <- process_vs(vs_results = 'vs_output',
                    rslt_source = 'remote')

output_tbl(vs_pp$vs_processed, 'vs_output_pp')
output_tbl(vs_pp$vs_violations, 'vs_violations_pp')

## Unmapped Concepts
uc_tmp<-results_tbl('uc_output')%>%select(-old_check_name)
uc_pp <- process_uc(uc_results = uc_tmp,
                    rslt_source = 'local')
uc_pp_desc<-add_desc(tbl=uc_pp,
                     join_col_name = 'measure',
                     check_type_name = 'uc',
                     label_file=dashboard_labels)
uc_byyr_pp<-process_uc(uc_results='uc_by_year',
                       rslt_source = 'remote')
uc_byyr_desc<-add_desc(tbl=uc_byyr_pp,
                     join_col_name = 'unmapped_description',
                     check_type_name='uc',
                     label_file=dashboard_labels)

output_tbl(uc_pp_desc, 'uc_output_pp')
output_tbl(uc_byyr_desc, 'uc_by_year_pp')

## MF Visit ID
mf_visitid_tmp<-results_tbl('mf_visitid_output')%>%select(-old_check_name)
mf_visitid_pp <- process_mf_visitid(mf_visitid_results = mf_visitid_tmp,
                                    rslt_source = 'local')
mf_visitid_desc<-add_desc(tbl=mf_visitid_pp,
                          join_col_name='domain',
                          check_type_name='mf',
                          label_file=dashboard_labels)

output_tbl(mf_visitid_desc, 'mf_visitid_output_pp')

## Best Mapped Concepts
#### Writing lookup table to schema
bmc_concepts<-read_codeset('bmc_concepts',col_types = 'cci')
output_tbl(bmc_concepts, 'bmc_concepts')

#### Standard Processing
bmc_pp <- process_bmc(bmc_results = 'bmc_output', #'bmc_gen_output',
                      bmc_concepts_labelled = 'bmc_concepts', ## with `include` column added that indicates not best concepts with 0
                      rslt_source = 'remote')
bmc_output_desc<-add_desc(tbl=bmc_pp,
                          join_col_name='check_desc',
                          check_type_name='bmc',
                          label_file = dashboard_labels)

output_tbl(bmc_output_desc, 'bmc_output_pp')

#### Detect Anomalies
bmc_anom <- squba.gen::compute_dist_anomalies(df_tbl= bmc_output_desc %>% filter(include == 1L),
                                              grp_vars=c('check_name', 'check_desc',
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

# PATCH change check_desc in bmc_output_concept
bmc_tmp<-add_desc(tbl=results_tbl('bmc_output_concepts_pp')%>%collect(),
                  join_col_name='check_desc',
                  check_type_name='bmc',
                  label_file=dashboard_labels)%>%select(-old_check_name)
output_tbl(bmc_tmp,'bmc_output_concepts_pp')

## Expected Concepts Present
#### Standard Processing
ecp_pp <- process_ecp(ecp_results = 'ecp_output',
                      rslt_source = 'remote')
ecp_pp_desc<-add_desc(tbl=ecp_pp,
                      join_col_name='concept_group',
                      check_type_name='ecp',
                      label_file = dashboard_labels)%>%
  left_join(read_codeset('ecp_cat_new','cc'))%>%select(-old_check_name)

output_tbl(ecp_pp_desc, 'ecp_output_pp')

#### Detect Anomalies
ecp_anom <- ssdqa.gen::compute_dist_anomalies(df_tbl= ecp_pp_desc,
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

## Clinical Fact Documentation

cfd_pp <- process_cfd(cfd_results = 'cfd_output',
                    rslt_source = 'remote')
cfd_pp_desc<-add_desc(tbl=cfd_pp,
                      join_col_name = 'check_desc_neat',
                      check_type_name='cfd',
                      label_file = dashboard_labels)%>%
  select(-old_check_name)

output_tbl(cfd_pp, 'cfd_output_pp')

## Domain Concordance
dcon_tmp<-results_tbl('dcon_output')%>%select(-old_check_name)
dcon_name_xwalk<-results_tbl('dcon_output')%>%distinct(check_name, old_check_name)%>%collect()
dcon_pp <- process_dcon(dcon_results = dcon_tmp,
                        rslt_source = 'local')
dcon_props_nm<-dcon_pp$dcon_props%>%
  left_join(dcon_name_xwalk, by = 'check_name')

dcon_pp_desc<-add_desc(tbl=dcon_props_nm,
                       join_col='old_check_name',
                       check_type_name='dcon',
                       label_file = dashboard_labels)%>%
  rename(description_full=old_check_name)

output_tbl(dcon_pp_desc, 'dcon_output_pp')
output_tbl(dcon_pp$dcon_cohort_map, 'dcon_meta')

## Facts Over Time

fot_pp <- process_fot(fot_results = 'fot_output',
                      target_col = 'row_cts',
                      add_ratios = TRUE,
                      ratio_mult = 10000,
                      rslt_source = 'remote')
fot_heur_desc<-add_desc(tbl=fot_pp$fot_heuristic_pp,
                        join_col_name='check_desc',
                        check_type_name='fot',
                        label_file=dashboard_labels)
fot_ratios_desc<-add_desc(tbl=fot_pp$fot_ratios,
                          join_col_name = 'check_desc',
                          check_type_name='fot',
                          label_file=dashboard_labels)

output_tbl(fot_heur_desc, 'fot_heuristic_pp')
output_tbl(fot_pp$fot_heuristic_summary_pp, 'fot_heuristic_summary_pp')
output_tbl(fot_ratios_desc, 'fot_ratios_pp')
