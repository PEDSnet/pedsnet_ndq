dashboard_labels<-read_codeset('dashboard_labels',col_types = 'ccc')%>%
  left_join(read_codeset('dcon_name_xwalk', col_types = 'cc'),
            by = c('old_desc'='old_check_name'))%>%
  mutate(old_desc=coalesce(new_check_name, old_desc))%>%
  select(-new_check_name)

## Data Cycle Changes
#### Standard Processing
dc_patch<-results_tbl('dc_output')%>%
  mutate(check_name=case_when(domain=='condition_op_order'~'dc_condsop-condsorder',
                          domain=='condition_op_billing'~'dc_condsop-condsbill',
                          TRUE~check_name))
dc_meta<-results_tbl('dc_meta')
dc_pp <- process_dc(dc_ct_results = dc_patch,
                    dc_meta_results = dc_meta,
                    rslt_source = 'local')

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

# Overall
uc_pp <- process_uc(uc_results = 'uc_output',
                    rslt_source = 'remote')
uc_pp_desc<-add_desc(tbl=uc_pp,
                     join_col_name = 'measure',
                     check_type_name = 'uc',
                     label_file=dashboard_labels)

output_tbl(uc_pp_desc, 'uc_output_pp')

# By Year
uc_year_pp <- process_uc(uc_results = 'uc_by_year',
                         rslt_source = 'remote')
uc_year_pp_desc<-add_desc(tbl=uc_year_pp,
                       join_col_name = 'unmapped_description',
                       check_type_name='uc',
                       label_file=dashboard_labels)

output_tbl(uc_year_pp_desc, 'uc_by_year_pp')

# Grouped concepts
uc_grpd_pp <- add_desc(tbl=results_tbl('uc_grpd')%>% collect(),
                       join_col_name='unmapped_description',
                       check_type_name='uc',
                       label_file=dashboard_labels)
output_tbl(uc_grpd_pp, 'uc_grpd_pp')

## MF Visit ID

mf_visitid_pp <- process_mf_visitid(mf_visitid_results = 'mf_visitid_output',
                                    rslt_source = 'remote')
mf_visitid_desc<-add_desc(tbl=mf_visitid_pp,
                          join_col_name='domain',
                          check_type_name='mf',
                          label_file=dashboard_labels)


output_tbl(mf_visitid_desc, 'mf_visitid_output_pp')

## Best Mapped Concepts
#### Standard Processing
bmc_concepts<-read_codeset('bmc_concepts',col_types = 'cci')
bmc_cat_labels<-bmc_concepts%>%distinct(check_name, include)%>%
  rename(best_notbest=include)
bmc_result_valueset<-results_tbl('bmc_output')%>%
  distinct(check_name, concept)%>%
  left_join(bmc_cat_labels, by = 'check_name', copy=TRUE)%>%
  left_join(bmc_concepts, by = c('check_name', 'concept'), copy=TRUE)%>%
  rename(op=include)%>%
  collect()%>%
  mutate(include=case_when(best_notbest==1&op==1~1L,
                           best_notbest==1&(is.na(op)|op==1)~0L,
                           best_notbest==0&op==0~0L,
                           best_notbest==0&is.na(op)~1L,
                           TRUE~NA_integer_))%>%
  select(check_name, concept, include)
output_tbl(bmc_result_valueset,
           'bmc_concepts_v59',
           file=TRUE,
           db=FALSE)
# manually looked through them here
bmc_concepts_v59<-read_csv(file.path('results','bmc_concepts_v59.csv'), col_types = 'ccii')%>%
  select(-questionable)
output_tbl(bmc_concepts_v59,
           'bmc_concepts')
bmc_pp <- process_bmc(bmc_results = 'bmc_output', #'bmc_gen_output',
                      bmc_concepts_labelled = 'bmc_concepts', ## with `include` column added that indicates not best concepts with 0
                      rslt_source = 'remote')
bmc_output_desc<-add_desc(tbl=bmc_pp$bmc_output_pp,
                          join_col_name='check_desc',
                          check_type_name='bmc',
                          label_file = dashboard_labels)
output_tbl(bmc_output_desc, 'bmc_output_pp')
bmc_concepts_desc<-add_desc(tbl=bmc_pp$bmc_concepts_pp,
                          join_col_name='check_desc',
                          check_type_name='bmc',
                          label_file = dashboard_labels)
output_tbl(bmc_concepts_desc, 'bmc_output_concepts_pp')

#### Detect Anomalies
bmc_anom <- squba.gen::compute_dist_anomalies(df_tbl= bmc_output_desc%>%filter(include==1L),
                                              grp_vars=c('check_name', 'check_desc',
                                                         'check_type', 'check_name_app',
                                                         'database_version'),
                                              var_col='best_row_prop',
                                              denom_cols = c('total_rows', 'check_name'))

bmc_anom_pp <- ssdqa.gen::detect_outliers(df_tbl = bmc_anom,
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
#
# ecp_pp_labs <- ecp_pp %>%
#   left_join(read_codeset('ecp_cat_new', 'cc'))

ecp_pp_desc<-add_desc(tbl=ecp_pp%>%mutate(check_name_keep=check_name),
                      join_col_name='check_name',
                      check_type_name='ecp',
                      label_file = dashboard_labels)%>%
  select(-concept_group)%>%
  rename(concept_group=check_name,
         check_name=check_name_keep)%>%
  left_join(read_codeset('ecp_cat_new_new','cc'))

output_tbl(ecp_pp_desc, 'ecp_output_pp')

#### Detect Anomalies
ecp_anom <- squba.gen::compute_dist_anomalies(df_tbl= ecp_pp_desc,
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

## Clinical Fact Documentation

cfd_pp <- process_cfd(cfd_results = 'cfd_output',
                      rslt_source = 'remote')
cfd_pp_desc<-add_desc(tbl=cfd_pp,
                      join_col_name = 'check_desc_neat',
                      check_type_name='cfd',
                      label_file = dashboard_labels)

output_tbl(cfd_pp_desc, 'cfd_output_pp')

## Domain Concordance

dcon_pp <- process_dcon(dcon_results = 'dcon_output',
                        rslt_source = 'remote')

dcon_pp_desc<-add_desc(tbl=dcon_pp%>%mutate(cn=check_name),
                       join_col='check_name',
                       check_type_name='dcon',
                       label_file = dashboard_labels)%>%
  rename(description_full=check_name,
         check_name=cn)

# dcon_pp_labs <- dcon_pp %>%
#   mutate(description_full = gsub('and', '/', check_desc))

output_tbl(dcon_pp_desc, 'dcon_output_pp')

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

## Date plausibility
dp_pp<-process_dp(dp_results = 'dp_output',
                  rslt_source='remote')
dp_pp_desc<-dp_pp%>%
  left_join(read_codeset('dp_name_desc', col_types='cccc'), by = c('check_description', 'check_name', 'implausible_type'))
output_tbl(dp_pp_desc,
           name='dp_output_pp')
