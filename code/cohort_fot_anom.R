#' Function to apply Facts over Time anomaly detection method
#' @param fot_data output from NDQ FOT processing,
#'        expected to have columns: site | check_name | time_start | row_pts | total_pt
#' @param lookback_timeframe date on which to end observation,
#'        intended to remove recent incomplete data
apply_fot_anom<-function(fot_data,
                         lookback_timeframe){

  # add patient rate
  fd_rate<-fot_data%>%
    select(site,check_name,time_start,row_pts,total_pt) %>%
    mutate(rate_per100=round(row_pts/total_pt, 4)*100) %>%
    arrange(time_start)%>%
    filter(time_start<lookback_timeframe)

  # apply STL decomposition + anomaly detection on remainder (via tk_anomaly_diagnostics)
  fot_tk <- fd_rate %>%
    group_by(site,check_name)%>%
    tk_anomaly_diagnostics(.date_var =time_start, .value=rate_per100) %>%
    ungroup()%>%
    # not sure why rate_deseason added to duplicate seasadj
    mutate(time_num=as.numeric(zoo::as.yearmon(time_start)),
           rate_deseason=seasadj) %>%
    inner_join(fd_rate, by = c('site', 'check_name', 'time_start')) %>%
    select(site, time_start, check_name, time_num, rate_per100, rate_deseason, seasadj, anomaly) %>%
    filter(is.finite(rate_deseason), !is.na(time_start))


  # determine breakpoints to minimize RSS across segments, where BIC selects best model
  bpoints<-strucchange::breakpoints(rate_deseason ~ time_num, data = fot_tk, h = 0.10)
  bic_vec <- as.vector(summary(bpoints)[[3]][2,])
  ks <- 0:(length(bic_vec) - 1)
  k_star <- ks[which.min(bic_vec)]
  bp_k <- breakpoints(bpoints, breaks = k_star)
  bdates <- fot_tk$time_start[bp_k$breakpoints]
  # failsafe when no breakpoints
  if(all(is.na(bdates))){
    fot_tk<-fot_tk%>%
      mutate(fit=NA_real_)
  }else{
    fit <- lm(rate_deseason ~ time_num * breakfactor(bp_k), data = fot_tk)
    robust <- coeftest(fit, vcov = NeweyWest(fit, lag = 12, prewhite = FALSE, adjust = TRUE))

    fot_tk$fit <- fitted(fit)
  }
  fot_tk<-fot_tk%>%
    mutate(breakdate=case_when(time_num%in%as.numeric(zoo::as.yearmon(bdates))~time_num,
                               TRUE~NA_real_))
  return(fot_tk)
}

#' Function to plot output from fot anomaly detection method
plot_fot_anom<-function(fot_pp){
  bdates<-fot_pp%>%filter(!is.na(breakdate))%>%distinct(breakdate)%>%pull()
  sitename<-fot_pp%>%distinct(site)%>%pull()
  cname<-fot_pp%>%distinct(check_name)%>%pull()
  ggplot(fot_pp, aes(zoo::as.yearmon(time_start), rate_deseason)) +
    geom_line(alpha = 0.6) +
    geom_line(aes(y = fit)) +
    { if (length(bdates)>=1) geom_vline(xintercept = as.numeric(bdates), linetype = "dashed") } +
    labs(title = paste("Deseasonalized rate with inflection(s)\nfor ",cname, " at ",sitename),
         subtitle = if (length(bdates)) paste("Breaks:", paste(format(zoo::as.yearmon(bdates), "%b %Y"), collapse = ", ")) else "No break selected by BIC",
         x = "Month", y = "Rate per 100 (seasonality removed)") +
    theme_minimal()
}
