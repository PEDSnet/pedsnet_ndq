
remove_precompute <- function(checkpoint){

  rslt_schm <- config('results_schema')

  ck1_list <- c('c19_dx_lab_current', 'c19_dx_lab_prev', 'payer_with_date',
               'op_prov_spec', 'op_cs_spec','pdl_pts', 'ip_admit', 'geocode_cohort',
                'fips_tract', 'fips_block_group', 'lohis_tract','lohis_block_group',
               'gest_age_cohort', 'c19_dx', 'c19_viral_labs', 'ecp_2010_geocodes',
               'ecp_2020_geocodes', 'ecp_alanine_transaminase', 'ecp_anc', 'ecp_blood_culture_labs',
               'ecp_blood_culture_px', 'ecp_cholesterol_all', 'ecp_creatinine_serum',
               'ecp_head_circumference', 'ecp_height', 'ecp_weight', 'ecp_hemoglobin',
               'ecp_hvs', 'ecp_influenza', 'ecp_rsv', 'ecp_platelets', 'ecp_rapid_strep',
               'ecp_smoking_tobacco', 'ecp_sodium', 'ecp_urine_protein_qual')

  ck2_list <- c('procs_drugs', 'procs_drugs_labs', 'icu_transfer', 'payer_w_pid',
                'ip_two', 'nephrology_specialties', 'oncology_specialties')

  ck3_list <- c('voml', 'vipdp', 'vodi', 'prvo', 'c19_imm', 'gp_specialties', 'wc_codes',
                'c19_immunizations')

  drop_tbls <- function(tbl_list,
                        schema = rslt_schm){

    for(i in tbl_list){
      db_remove_table(name = in_schema(rslt_schm, i))
    }

  }

  if(checkpoint == 1){

    add_list <- dbGetQuery(config('db_src'),
                           paste0("SHOW TABLES IN ", config('results_schema'))) %>%
      filter(grepl('sdoh_|promis_|valueset_|ecp_|temp_', Table),
             Table != 'ecp_output') %>%
      pull(Table)

    drop_tbls(tbl_list = ck1_list %>% append(add_list))
    cli::cli_inform(str_wrap(paste0('The following tables have been removed from
                                    your results_schema: ', paste(ck1_list %>% append(add_list),
                                                                  collapse = ', '))))
  }else if(checkpoint == 2){

    add_list <- dbGetQuery(config('db_src'),
                           paste0("SHOW TABLES IN ", config('results_schema'))) %>%
      filter(grepl('dx_|rx_|px_|lab_|temp_|insulin|general|t1d|oncology$|nephrology$', Table)) %>%
      pull(Table)

    drop_tbls(tbl_list = ck2_list %>% append(add_list))
    cli::cli_inform(str_wrap(paste0('The following tables have been removed from
                                    your results_schema: ', paste(ck2_list %>% append(add_list),
                                                                  collapse = ', '))))
  }else if(checkpoint == 3){
    drop_tbls(tbl_list = ck3_list)
    cli::cli_inform(str_wrap(paste0('The following tables have been removed from
                                    your results_schema: ', paste(ck3_list, collapse = ', '))))
  }

}
