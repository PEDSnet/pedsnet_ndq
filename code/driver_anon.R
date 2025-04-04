
########################### SITE ANONYMIZATION ##################################

pp_tbl_names <- pull_site_tables()
tbls_anon <- attach_anon_id(all_sites_tbl=results_tbl('dc_output'),
                            tbls_to_anon=pp_tbl_names)

output_list_to_db_collect(tbls_anon,
                          append=FALSE)



