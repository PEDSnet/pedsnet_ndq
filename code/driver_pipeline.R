
pipeline_issues <- results_tbl('pipeline_metadata') %>%
  left_join(results_tbl('pipeline_metrics_vocab')) %>% collect()

add_bonus_mult <- pipeline_issues %>%
  filter(is.na(groupid)) %>%
  mutate(mult_add = ifelse(dashboard_value == 5, 5, 0)) %>%
  distinct(site, mult_add)

compute_scores <- pipeline_issues %>%
  filter(!is.na(groupid)) %>%
  group_by(site, groupid) %>%
  mutate(dashboard_value = sum(dashboard_value)) %>%
  mutate(mult_score = dashboard_multiplier * dashboard_value) %>%
  group_by(site, groupid) %>%
  summarise(group_mult = sum(mult_score)) %>%
  mutate(group_score = 25 - group_mult,
         group_score = ifelse(group_score < 0, 0, group_score)) %>%
  group_by(site) %>%
  mutate(composite_score = sum(group_score)) %>%
  left_join(add_bonus_mult) %>%
  mutate(composite_score = composite_score + mult_add,
         composite_score = ifelse(composite_score > 100, 100, composite_score))




library(ggplot2)

opt1 <- compute_scores %>%
  distinct(site, composite_score)  %>%
  ggplot(aes(x = site, y = composite_score / 100, fill = site)) +
  geom_col() +
  ggsci::scale_fill_d3('category20c') +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::percent) +
  labs(y = 'Composite Score',
       x = 'Site')

ggsave(filename = file.path(getwd(), 'results', 'pipeline_graph_opt1.png'),
       opt1)


opt2 <- compute_scores %>%
  tidyr::pivot_longer(cols = c(composite_score, mult_add)) %>%
  distinct(site, name, value) %>%
  mutate(name = factor(name, levels = c('mult_add', 'composite_score'))) %>%
  ggplot(aes(x = site, y = value, fill = name)) +
  geom_col() +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(y = 'Composite Score',
       x = 'Site')

ggsave(filename = file.path(getwd(), 'results', 'pipeline_graph_opt2.png'),
       opt2)


opt3 <- compute_scores %>%
  distinct(site, composite_score)  %>%
  ggplot(aes(x = site, y = composite_score / 100, fill = site)) +
  geom_rect(aes(xmin = 0.3, xmax = Inf, ymin = 0.8, ymax = 1), fill = 'green', alpha = 0.01) +
  geom_rect(aes(xmin = 0.3, xmax = Inf, ymin = 0.5, ymax = 0.8), fill = 'yellow', alpha = 0.01) +
  geom_rect(aes(xmin = 0.3, xmax = Inf, ymin = 0, ymax = 0.5), fill = 'red', alpha = 0.01) +
  geom_col() +
  ggsci::scale_fill_d3('category20c') +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::percent) +
  labs(y = 'Composite Score',
       x = 'Site')


ggsave(filename = file.path(getwd(), 'results', 'pipeline_graph_opt3.png'),
       opt3)


opt4 <- compute_scores %>%
  tidyr::pivot_longer(cols = c(composite_score, mult_add)) %>%
  distinct(site, name, value) %>%
  mutate(name = factor(name, levels = c('mult_add', 'composite_score'))) %>%
  ggplot(aes(x = site, y = value, fill = name)) +
  geom_rect(aes(xmin = 0.3, xmax = Inf, ymin = 90, ymax = 100), fill = 'green', alpha = 0.01) +
  geom_rect(aes(xmin = 0.3, xmax = Inf, ymin = 70, ymax = 90), fill = 'yellow', alpha = 0.01) +
  geom_rect(aes(xmin = 0.3, xmax = Inf, ymin = 0, ymax = 70), fill = 'red', alpha = 0.01) +
  geom_col() +
  scale_fill_manual(values = c('#935870', '#0D4254')) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(y = 'Composite Score',
       x = 'Site')

ggsave(filename = file.path(getwd(), 'results', 'pipeline_graph_opt6.png'),
       opt4)




## UPDATES

#' current db
#' 4/5 categories
#' one box for each site,
#' stacked bar for each site, sections are categories
#'
#' over time
#' composite score for each site across data cycles
#' categories in tooltip, facets for each category
#'
#' pipeline length of time
#' current db only, length of run for each step
#' date of start finish
#' number of failures
#'


processing_checks <- argos:::qual_tbl('preprocessing_results', schema = 'dqa_preprocessing') %>%
  distinct(task_id, description) %>%
  rename('check_name' = 'task_id',
         'check_description' = 'description') %>%
  collect()
mutate(check_type = 'Pipeline Preprocessing',
       check_category = 'Completeness')
