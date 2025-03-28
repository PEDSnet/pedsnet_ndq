
## Data Cycle Changes

# dc_pp <- process_dc(dc_ct_results = 'dc_output',
#                     dc_meta_results = 'dc_meta',
#                     rslt_source = 'remote')
#
# output_tbl(dc_pp, 'dc_output_pp')

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

uc_pp <- process_uc(uc_results = 'uc_output',
                    rslt_source = 'remote')

output_tbl(uc_pp, 'uc_output_pp')

## MF Visit ID

mf_visitid_pp <- process_mf_visitid(mf_visitid_results = 'mf_visitid_output',
                                    rslt_source = 'remote')

output_tbl(mf_visitid_pp, 'mf_visitid_output_pp')

## Best Mapped Concepts

bmc_pp <- process_bmc(bmc_results = 'bmc_output',
                      bmc_concepts_labelled = 'bmc_concepts', ## with `include` column added that indicates not best concepts with 0
                      rslt_source = 'remote')

output_tbl(bmc_pp, 'bmc_output_pp')

## Expected Concepts Present

ecp_pp <- process_ecp(ecp_results = 'ecp_output',
                      rslt_source = 'remote')

output_tbl(ecp_pp, 'ecp_output_pp')

## Patient Facts

pf_pp <- process_pf(pf_results = 'pf_output',
                    rslt_source = 'remote')

output_tbl(pf_pp, 'pf_output_pp')

## Domain Concordance

dcon_pp <- process_dcon(dcon_results = 'dcon_output',
                        rslt_source = 'remote')

output_tbl(dcon_pp$dcon_props, 'dcon_output_pp')
output_tbl(dcon_pp$dcon_cohort_map, 'dcon_meta')

## Facts Over Time

fot_pp <- process_fot(fot_results = 'fot_output',
                      target_col = 'row_cts',
                      add_ratios = TRUE,
                      ratio_mult = 10000,
                      rslt_source = 'remote')

output_tbl(fot_pp$fot_heuristic_pp, 'fot_heuristic_pp')
output_tbl(fot_pp$fot_heuristic_summary_pp, 'fot_heuristic_summary_pp')
output_tbl(fot_pp$fot_ratios, 'fot_ratios_pp')
