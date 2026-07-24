library(timetk); library(strucchange); library(sandwich);
library(lmtest); library(ggplot2); library(dplyr); library(zoo)

# Future areas of opportunity:
# Parameterize: rate per, visits vs rows vs patients? (or standardize on one), breakpoints

# Pilot with:
# - Visits: general practice visits
# - Visits: with well child codes
# - Procedures: outpatient procedures
# - Anthropometrics: weight
# - Drugs: prescriptions
cnames<-c('fot_visgp',
          'fot_visall-condswcc-procswcc',
          'fot_procsall-visop',
          'fot_wt',
          'fot_drugspresc')
sitenames<-results_tbl('fot_output')%>%distinct(site)%>%pull()

# Loop through sites and checks of interest, apply anomaly detection methods
dat<-list()
for(i in 1:length(cnames)){
  thischeck<-cnames[i]
  fd_for_anom<-results_tbl('fot_output')%>%filter(check_name==thischeck)%>%collect()
  for(k in 1:length(sitenames)){
    thisname<-sitenames[k]
    fd_for_site<-fd_for_anom%>%filter(site==thisname)
    dat[[paste0(i, ',', k)]]<-apply_fot_anom(fd_for_site,
                                             lookback_timeframe=Sys.Date()-months(4))
  }
  k<-0
}

datred<-Reduce(union,dat)

output_tbl(datred,
           name='fot_anom_tst')
