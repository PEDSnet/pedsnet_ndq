


library(timetk); library(strucchange); library(sandwich); 
library(lmtest); library(ggplot2); library(dplyr); library(zoo)

### LOAD DATA
fot_oct2025 <- read_csv('fot_output_202510262028.csv')
fot_drugs_ipchop <- 
  fot_oct2025 %>% 
  filter(check_name == 'fot_drugsip', site == 'chop')

drugsipchop_pt_rate <- 
  fot_drugs_ipchop %>% select(time_start,row_pts,total_pt) %>% 
  mutate(rate_per100=round(row_pts/total_pt, 4)*100) %>% 
  arrange(time_start) %>% 
  filter(! time_start == '2025-09-01')

drugsipchop_pt_rate_2 <- drugsipchop_pt_rate %>% 
  arrange(time_start) %>% mutate(time_num = as.numeric(as.yearmon(time_start)))
# 2) Diagnose and extract seasonality using timetk
drugsip_tk <- drugsipchop_pt_rate_2 %>% 
  tk_anomaly_diagnostics(.date_var =time_start, .value=rate_per100) %>% 
  mutate(time_num=as.numeric(zoo::as.yearmon(time_start)),
         rate_deseason=seasadj) %>% 
  #mutate(rate_deseason=observed-season) %>% 
  inner_join(drugsipchop_pt_rate_2) %>% 
  select(time_start, time_num, rate_per100, rate_deseason, seasadj, anomaly) %>% 
  filter(is.finite(rate_deseason), !is.na(time_start))

# 3) Breakpoint scan (BIC selects number of breaks)
bp_full <- strucchange::breakpoints(rate_deseason ~ time_num, data = drugsip_tk, h = 0.10)
# Get the whole BIC vector (k = 0..K)
bic_vec <- as.vector(summary(bp_full)[[3]][2,])
ks <- 0:(length(bic_vec) - 1)
k_star <- ks[which.min(bic_vec)]
bp_k <- breakpoints(bp_full, breaks = k_star)
bdates <- drugsip_tk$time_start[bp_k$breakpoints]

# 4) Segment slopes with HAC/Newey–West SEs ---
fit     <- lm(rate_deseason ~ time_num * breakfactor(bp_k), data = drugsip_tk)
robust  <- coeftest(fit, vcov = NeweyWest(fit, lag = 12, prewhite = FALSE, adjust = TRUE))
print(data.frame(k = ks, BIC = bic_vec))
print(robust)

# 5) Plot ---
drugsip_tk$fit <- fitted(fit)
ggplot(drugsip_tk, aes(time_start, rate_deseason)) +
  geom_line(alpha = 0.6) +
  geom_line(aes(y = fit)) +
  { if (length(bdates)) geom_vline(xintercept = as.numeric(bdates), linetype = "dashed") else list() } +
  labs(title = "Deseasonalized rate (tk_anomaly_diagnostics) with inflection(s)",
       subtitle = if (length(bdates)) paste("Breaks:", paste(format(bdates, "%b %Y"), collapse = ", ")) else "No break selected by BIC",
       x = "Month", y = "Rate per 100 (seasonality removed)") +
  theme_minimal()

