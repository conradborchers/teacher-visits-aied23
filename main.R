# Analysis script for AIED '23 paper
# A Spatiotemporal Analysis of Teacher Practices in Supporting Student Learning and Engagement in an AI-enabled Classroom

library(tidyverse)
library(lme4)
library(lmerTest)
library(data.table)
library(Hmisc)

conflicted::conflict_prefer('summarize', 'dplyr')
conflicted::conflict_prefer('filter', 'dplyr')
conflicted::conflict_prefer('lag', 'dplyr')

source('utils.R')

##### Data Aggregation #####

# Time-stamped event file; each event has a start and end date
d <-
  #read_csv(here::here('output_files', 'analytics_data_aied23.csv')) %>%
  read_delim(here::here('events_file_aied23.txt')) %>%
  janitor::clean_names() %>% 
  (function(df){names(df) <- names(df) %>% str_remove('cf_'); return(df)}) %>%
  select(-level_dataset, -problem_name, -selection, -action, -input) %>% 
  mutate(seconds = end - start)

# Aggregate teacher allocation variables per student
d_student_attention_received <- d %>%
  arrange(start) %>%
  filter(actor == 'teacher' & str_detect(subject, 'Stu')) %>%
  group_by(day_id, subject, event) %>%
  summarize(
    count = n(),
    total_len = sum(seconds, na.rm = TRUE),
    avg_len = mean(seconds, na.rm = TRUE),
    sd_len = sd(seconds, na.rm = TRUE),
    skew_len = moments::skewness(seconds, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    event = case_when(
      event == 'Stopping' ~ 'stop',
      event == 'Talking to student: OFF-task' ~ 'talk_off_task'
    )
  ) %>%
  pivot_wider(
    names_from = c(day_id, event),
    values_from = c(count, total_len, avg_len, sd_len, skew_len)
  ) %>%
  mutate(day_1_missing = map2_lgl(count_1_stop, count_1_talk_off_task, function(a, b) {
    return(sum(is.na(c(a, b))) == 2)
  })) %>%
  mutate(day_2_missing = map2_lgl(count_2_stop, count_2_talk_off_task, function(a, b) {
    return(sum(is.na(c(a, b))) == 2)
  })) %>%
  mutate(day_3_missing = map2_lgl(count_3_stop, count_3_talk_off_task, function(a, b) {
    return(sum(is.na(c(a, b))) == 2)
  })) %>%
  (function(df) {
    df[is.na(df)] <- 0
    return(df)
  })

d_student_attention_received_overall <- d %>%
  arrange(start) %>%
  filter(actor == 'teacher' & str_detect(subject, 'Stu')) %>%
  group_by(subject, event) %>%
  summarize(
    count = n(),
    total_len = sum(seconds, na.rm = TRUE),
    avg_len = mean(seconds, na.rm = TRUE),
    sd_len = sd(seconds, na.rm = TRUE),
    skew_len = moments::skewness(seconds, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    event = case_when(
      event == 'Stopping' ~ 'stop',
      event == 'Talking to student: OFF-task' ~ 'talk_off_task'
    )
  ) %>%
  pivot_wider(
    names_from = c(event),
    values_from = c(count, total_len, avg_len, sd_len, skew_len)
  ) %>%
  (function(df) {
    df[is.na(df)] <- 0
    return(df)
  })

d_student_attention_received <-
  d_student_attention_received_overall %>%
  left_join(d_student_attention_received %>% select(subject, matches('missing')),
            by = 'subject')

# Test scores
d_learning <-
  #read_csv(here::here('raw data', 'pre_post_scores.csv')) %>%
  read_delim(here::here('pre_post_scores.txt')) %>%
  janitor::clean_names() %>%
  (function(df){names(df) <- names(df) %>% str_remove('cf_'); return(df)}) %>% 
  select(-level_dataset, -problem_name, -selection, -action, -input) %>% 
  mutate(ck_lg = (ck - ck_pre) / (16 - ck_pre),
         pk_lg = (pk - pk_pre) / (5 - pk_pre)) %>%
  mutate(pk_lg = ifelse(is.infinite(pk_lg), 0, pk_lg))

# Crosswalk from anonymized to DataShop IDs
d_mapping <- 
  #read_csv(here::here('raw data', 'IDmapping.csv'))
  read_delim(here::here('crosswalk.txt')) %>% 
  janitor::clean_names() %>%
  (function(df){names(df) <- names(df) %>% str_remove('cf_'); return(df)}) %>% 
  select(actual_user_id, anon_user_id)


# Time series student-level aggregates
d_times <- d %>%
  mutate(time_center = start + ((end - start) / 2)) %>%
  filter(str_detect(subject, "Stu")) %>%
  group_by(subject) %>%
  summarize(
    times_start = list(sort(start)),
    times_center = list(sort(time_center)),
    times_end = list(sort(end))
  ) %>%
  ungroup() %>%
  mutate(first_visit_start = map_dbl(times_start, min))

# Disengagement and struggle detector data
d_detector <-
  #read_csv(here::here('output_files', 'detector_results.csv')) %>%
  read_delim(here::here('detector_output.txt')) %>%
  janitor::clean_names() %>%
  select(-time) %>% 
  (function(df){names(df) <- names(df) %>% str_remove('cf_'); return(df)}) %>% 
  mutate(time_unix = as.numeric(time)) %>%
  left_join(d_times, by = c("student_id" = "subject")) %>%
  mutate(
    time_diff_to_last_visit = map2_dbl(time_unix, times_end, diff_last_visit),
    time_diff_to_next_visit = map2_dbl(time_unix, times_start, diff_next_visit),
    time_diff_to_closest_visit = map2_dbl(time_unix, times_center, diff_closest_visit)
  ) %>%
  mutate(
    # Flag visit differences stretching across single periods
    time_diff_to_last_visit = ifelse(
      abs(time_diff_to_last_visit) > 30 * 60,
      NA,
      time_diff_to_last_visit
    ),
    time_diff_to_next_visit = ifelse(
      abs(time_diff_to_next_visit) > 30 * 60,
      NA,
      time_diff_to_next_visit
    ),
    time_diff_to_closest_visit = ifelse(
      abs(time_diff_to_closest_visit) > 30 * 60,
      NA,
      time_diff_to_closest_visit
    )
  ) %>%
  mutate(
    is_idle = ifelse(is.na(idle), 0, ifelse(idle > 0, 1, 0)),
    is_misuse = ifelse(is.na(misuse), 0, ifelse(misuse > 0, 1, 0)),
    is_struggle = ifelse(is.na(struggle), 0, ifelse(struggle > 0, 1, 0))
  ) %>%  mutate(
    visited_before_after = case_when(
      is.na(first_visit_start) ~ 0,
      time_unix < first_visit_start ~ -1,
      time_unix >= first_visit_start ~ 1
    )
  )

# Join day to detector data
d <- d %>%
  mutate(the_date = as.POSIXct(d$start, origin = "1970-01-01") %>% as.Date())

# Joining day ID (day N) to detector data
d_date_ref <- d %>%
  filter(str_detect(subject, "Stu")) %>%
  distinct(day_id, the_date, subject)

d_detector <- d_detector %>%
  select(-day_id) %>%
  mutate(the_date = as.Date(time)) %>%
  left_join(d_date_ref, by = c("student_id" = "subject", "the_date"))

detector_vars <- d_detector %>% create_detector_variables()

# Joining Crosswalk to test data
d_learning <- d_learning %>%
  rename(animal_name = anon_student_id) %>%
  left_join(
    d_mapping %>% select(animal_name = actual_user_id, anon_user_id) %>% distinct(animal_name, anon_user_id),
    by = 'animal_name'
  )

# Joining attention, detector, and learning gain (i.e., test) data
d_model <- d_student_attention_received %>%
  left_join(d_learning, by = c('subject' = 'anon_user_id')) %>%
  left_join(detector_vars, by = c("subject" = "student_id"))

##### Main Analysis #####

# H1
d_knowledge <- d %>%
  mutate(
    concept_pre = coalesce(actor_conceptual_pre_score, subject_conceptual_pre_score),
    concept_post = coalesce(actor_conceptual_post_score, subject_conceptual_post_score),
    concept_lg = coalesce(actor_conceptual_lg, subject_conceptual_lg),
    procedural_pre = coalesce(actor_procedural_pre_score, subject_procedural_pre_score),
    procedural_post = coalesce(actor_procedural_post_score, subject_procedural_post_score),
    procedural_lg = coalesce(actor_procedural_lg, subject_procedural_lg)
  ) %>%
  mutate(student_id = ifelse(
    str_detect(actor, 'Stu'),
    actor,
    ifelse(str_detect(subject, 'Stu'), subject, NA)
  )) %>%
  filter(!is.na(student_id)) %>%
  select(
    student_id,
    concept_pre,
    concept_post,
    concept_lg,
    procedural_pre,
    procedural_post,
    procedural_lg
  ) %>%
  group_by(student_id) %>%
  summarize(
    concept_pre = unique(concept_pre),
    concept_post = unique(concept_pre),
    concept_lg = unique(concept_pre),
    procedural_pre = unique(concept_pre),
    procedural_post = unique(concept_pre),
    procedural_lg = unique(concept_pre)
  ) %>%
  ungroup()

d_knowledge_pre <- d_knowledge %>%
  mutate(procedural_pre = scale(procedural_pre),
         concept_pre = scale(concept_pre)) %>%
  rowwise() %>%
  mutate(avg_pre = (procedural_pre + concept_pre) / 2) %>%
  ungroup() %>%
  mutate(high_prior_knowledge = ifelse(avg_pre > median(avg_pre, na.rm = TRUE), 1, 0)) %>%
  select(student_id, high_prior_knowledge, avg_pre)

d_attention_allocation <- d_model %>%
  left_join(d_knowledge_pre, by = c('subject' = 'student_id')) %>%
  group_by(subject) %>%
  summarize(
    avg_pre = unique(avg_pre),
    n_visits = unique(count_stop),
    n_len_visits = unique(total_len_stop)
  ) %>%
  ungroup() %>%
  mutate(never_visited = ifelse(n_visits == 0, 1, 0)) %>%
  drop_na()

cor.test(d_attention_allocation$never_visited,
         d_attention_allocation$avg_pre)

d_attention_allocation <- d_model %>%
  left_join(d_knowledge_pre %>% select(student_id, avg_pre),
            by = c('subject' = 'student_id')) %>%
  group_by(subject) %>%
  summarize(
    avg_pre = unique(avg_pre),
    n_visits = unique(count_stop),
    n_len_visits = unique(total_len_stop)
  ) %>%
  ungroup()

cor.test(d_attention_allocation$n_visits,
         d_attention_allocation$avg_pre)

cor.test(d_attention_allocation$n_len_visits,
         d_attention_allocation$avg_pre)

# H2

d_detector <- d_detector %>%
  mutate(
    visit_lag = case_when(
      time_diff_to_next_visit <= median(time_diff_to_next_visit, na.rm = TRUE) ~ paste('Visit <= ', round(
        median(time_diff_to_next_visit, na.rm = TRUE)
      ), ' s', sep = ''),
      time_diff_to_next_visit <= quantile(time_diff_to_next_visit, 0.75, na.rm = TRUE) ~ paste('Visit <= ', round(
        quantile(time_diff_to_next_visit, 0.75, na.rm = TRUE)
      ), ' s', sep = ''),
      time_diff_to_next_visit <= 0 ~ paste('Visit <= ', 0, ' s', sep = ''),
      time_diff_to_last_visit <= median(time_diff_to_last_visit, na.rm = TRUE) ~ paste('Visit <= ', round(
        median(time_diff_to_last_visit, na.rm = TRUE)
      ), ' s', sep = ''),
      time_diff_to_last_visit > median(time_diff_to_last_visit, na.rm = TRUE) ~ paste('Visit > ', round(
        median(time_diff_to_last_visit, na.rm = TRUE)
      ), ' s', sep = ''),
      TRUE ~ 'Never Visited'
    )
  )

join_this <- d_detector$visit_lag %>%
  table() %>%
  tibble::enframe('visit_lag', 'visit_lag_count')

d_detector2 <- d_detector %>%
  left_join(join_this, by = 'visit_lag') %>%
  mutate('visit_lag_with_count' = paste(paste(
    visit_lag, format(visit_lag_count, big.mark = ","), sep = ' (N = '
  ), ')', sep = ''))

d_cor <- d_model %>%
  filter(count_stop > 0)
cor.test(d_cor$count_stop, d_cor$struggle_avg_score)
cor.test(d_cor$total_len_stop, d_cor$struggle_avg_score)

d_plot <- d_detector2 %>%
  group_by(visit_lag_with_count) %>%
  summarize(
    struggle_mean = mean(is_struggle, na.rm = TRUE),
    struggle_sd = sd(is_struggle, na.rm = TRUE),
    struggle_se = 2 * my_se(is_struggle),
    misuse_mean = mean(is_misuse, na.rm = TRUE),
    misuse_sd = sd(is_misuse, na.rm = TRUE),
    misuse_se = 2 * my_se(is_misuse),
    idle_mean = mean(is_idle, na.rm = TRUE),
    idle_sd = sd(is_idle, na.rm = TRUE),
    idle_se = 2 * my_se(is_idle)
  ) %>%
  ungroup() %>%
  pivot_longer(!visit_lag_with_count) %>%
  separate(name, sep = '_', into = c('var', 'metric')) %>%
  pivot_wider(names_from = metric, values_from = value)

the_levels <- d_plot$visit_lag_with_count %>%
  gtools::mixedsort() %>%
  unique()

d_plot$visit_lag_with_count <-
  factor(d_plot$visit_lag_with_count, levels = the_levels)

d_plot <- d_detector2 %>%
  mutate(visit_binary = case_when(
    str_detect(visit_lag_with_count, 'Never') ~ 1,!str_detect(visit_lag_with_count, 'Never') ~ 0
  )) %>%
  group_by(visited_before_after) %>%
  summarize(
    struggle_mean = mean(is_struggle, na.rm = TRUE),
    struggle_sd = sd(is_struggle, na.rm = TRUE),
    struggle_se = 2 * my_se(is_struggle),
    misuse_mean = mean(is_misuse, na.rm = TRUE),
    misuse_sd = sd(is_misuse, na.rm = TRUE),
    misuse_se = 2 * my_se(is_misuse),
    idle_mean = mean(is_idle, na.rm = TRUE),
    idle_sd = sd(is_idle, na.rm = TRUE),
    idle_se = 2 * my_se(is_idle)
  ) %>%
  ungroup() %>%
  pivot_longer(!visited_before_after) %>%
  separate(name, sep = '_', into = c('var', 'metric')) %>%
  pivot_wider(names_from = metric, values_from = value)

ggplot(d_plot,
       aes(
         x = visited_before_after,
         y = mean,
         group = var,
         color = var
       )) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = .2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = 'Frequency of Behavior', color = 'Behavior') +
  theme(legend.position = "top")

d_plot <- d_detector2 %>%
  mutate(visit_binary = case_when(
    str_detect(visit_lag_with_count, 'Never') ~ 1,!str_detect(visit_lag_with_count, 'Never') ~ 0
  )) %>%
  mutate(visit_trinary = case_when(
    str_detect(visit_lag_with_count, 'Never') ~ 0,
    str_detect(visit_lag_with_count, '<= 0') ~ -1,
    str_detect(visit_lag_with_count, '-[0-9]') ~ -1,
    TRUE ~ 1,
  )) %>%
  group_by(period_id, visit_trinary) %>%
  summarize(
    struggle_mean = mean(is_struggle, na.rm = TRUE),
    struggle_sd = sd(is_struggle, na.rm = TRUE),
    struggle_se = 2 * my_se(is_struggle),
    misuse_mean = mean(is_misuse, na.rm = TRUE),
    misuse_sd = sd(is_misuse, na.rm = TRUE),
    misuse_se = 2 * my_se(is_misuse),
    idle_mean = mean(is_idle, na.rm = TRUE),
    idle_sd = sd(is_idle, na.rm = TRUE),
    idle_se = 2 * my_se(is_idle)
  ) %>%
  ungroup() %>%
  pivot_longer(matches('_mean|_sd|_se')) %>%
  separate(name, sep = '_', into = c('var', 'metric')) %>%
  pivot_wider(names_from = metric, values_from = value)

d_plot$visit_trinary_fct <-
  factor(
    d_plot$visit_trinary,
    levels = c(0,-1, 1),
    labels = c('Not Visited', 'Pre-Visit', 'Post-Visit')
  )

# Add number of students
join_this <- d %>%
  filter(str_detect(subject, 'Stu_')) %>%
  distinct(period_id, subject) %>%
  count(period_id)
d_plot <- d_plot %>%
  left_join(join_this, by = 'period_id')

d_plot$period_id <-
  paste('Period ', d_plot$period_id, ' (N = ', d_plot$n, ')', sep = '')

# Remove gaming from this analysis
d_plot <- d_plot %>% filter(var != 'gaming')

cbPalette <-
  c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )
cbbPalette <-
  c(
    "#000000",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )

png('fig1-h1.png', width = 480 * 2.5)
ggplot(d_plot, aes(
  x = visit_trinary_fct,
  y = mean,
  group = var,
  color = var
)) +
  geom_line(size = 2.5) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = .2,
                position = position_dodge(0.05)) +
  #geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = 'Frequency of Behavior', color = 'Behavior') +
  theme(legend.position = "top") +
  theme(text = element_text(size = 28)) +
  scale_colour_manual(values = cbbPalette) +
  facet_wrap( ~ period_id, ncol = 5)
dev.off()

# For what disengagement/struggle variable is the effect of pre-to-post visit largest?
a <- d_plot %>%
  filter(visit_trinary == -1) %>%
  arrange(var, period_id) %>%
  select(period_id, var, pre = mean)

b <- d_plot %>%
  filter(visit_trinary == 1) %>%
  arrange(period_id, var) %>%
  select(period_id, var, post = mean)

eff <- inner_join(a, b, by = c('period_id', 'var')) %>%
  mutate(diff = pre - post) %>%
  mutate(diff_standard = (pre - post) / pre)

eff

eff %>%
  group_by(var) %>%
  summarize(m = mean(diff_standard)) %>%
  ungroup() %>%
  arrange(m)

# linear model
m <-
  glm(is_idle ~ scale(time_diff_to_last_visit), d_detector2, family = 'binomial')
summary(m)
sjPlot::tab_model(m)

d_plot <- d_detector2 %>%
  mutate(visit_binary = case_when(
    str_detect(visit_lag_with_count, 'Never') ~ 1,!str_detect(visit_lag_with_count, 'Never') ~ 0
  )) %>%
  mutate(visit_trinary = case_when(
    str_detect(visit_lag_with_count, 'Never') ~ 0,
    str_detect(visit_lag_with_count, '<= 0') ~ -1,
    str_detect(visit_lag_with_count, '-[0-9]') ~ -1,
    TRUE ~ 1,
  )) %>%
  group_by(visit_trinary) %>%
  summarize(
    struggle_mean = mean(is_struggle, na.rm = TRUE),
    struggle_sd = sd(is_struggle, na.rm = TRUE),
    struggle_se = 2 * my_se(is_struggle),
    misuse_mean = mean(is_misuse, na.rm = TRUE),
    misuse_sd = sd(is_misuse, na.rm = TRUE),
    misuse_se = 2 * my_se(is_misuse),
    idle_mean = mean(is_idle, na.rm = TRUE),
    idle_sd = sd(is_idle, na.rm = TRUE),
    idle_se = 2 * my_se(is_idle)
  ) %>%
  ungroup() %>%
  pivot_longer(matches('_mean|_sd|_se')) %>%
  separate(name, sep = '_', into = c('var', 'metric')) %>%
  pivot_wider(names_from = metric, values_from = value)

d_plot$visit_trinary_fct <-
  factor(
    d_plot$visit_trinary,
    levels = c(0,-1, 1),
    labels = c('Not Visited', 'Pre-Visit', 'Post-Visit')
  )
ggplot(d_plot, aes(
  x = visit_trinary_fct,
  y = mean,
  group = var,
  color = var
)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = .2,
                position = position_dodge(0.05)) +
  #geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = 'Frequency of Behavior', color = 'Behavior') +
  theme(legend.position = "top")

# H3

d_tutor <- 
  #read_tsv(here::here('raw data', 'tutor_log.tsv')) %>%
  read_delim(here::here('tutor_logs.txt')) %>%
  janitor::clean_names() %>%
  (function(df){names(df) <- names(df) %>% str_remove('cf_'); return(df)}) %>%
  mutate(time_unix = as.numeric(time))

d_times <- d %>%
  mutate(time_center = start + ((end - start) / 2)) %>%
  filter(str_detect(subject, "Stu")) %>%
  group_by(subject) %>%
  summarize(
    times_start = list(sort(start)),
    times_center = list(sort(time_center)),
    times_end = list(sort(end))
  ) %>%
  ungroup() %>%
  mutate(first_visit_start = map_dbl(times_start, min))

d_tutor <- d_tutor %>%
  left_join(d_times, by = c("anon_student_id" = "subject")) %>%
  mutate(
    visited_before_after = case_when(
      is.na(first_visit_start) ~ 0,
      time_unix < first_visit_start ~ -1,
      time_unix >= first_visit_start ~ 1
    )
  )

d_tutor <- d_tutor %>%
  mutate(
    time_diff_to_last_visit = map2_dbl(time_unix, times_end, diff_last_visit),
    time_diff_to_next_visit = map2_dbl(time_unix, times_start, diff_next_visit),
    time_diff_to_closest_visit = map2_dbl(time_unix, times_center, diff_closest_visit)
  ) %>%
  mutate(
    # Flag visit differences stretching across single periods
    time_diff_to_last_visit = ifelse(
      abs(time_diff_to_last_visit) > 30 * 60,
      NA,
      time_diff_to_last_visit
    ),
    time_diff_to_next_visit = ifelse(
      abs(time_diff_to_next_visit) > 30 * 60,
      NA,
      time_diff_to_next_visit
    ),
    time_diff_to_closest_visit = ifelse(
      abs(time_diff_to_closest_visit) > 30 * 60,
      NA,
      time_diff_to_closest_visit
    )
  )

d_tutor <- d_tutor %>%
  mutate(
    visit_lag = case_when(
      time_diff_to_next_visit <= median(time_diff_to_next_visit, na.rm = TRUE) ~ paste(
        'Visit <= ',
        median(time_diff_to_next_visit, na.rm = TRUE),
        ' s',
        sep = ''
      ),
      time_diff_to_next_visit <= quantile(time_diff_to_next_visit, 0.75, na.rm = TRUE) ~ paste(
        'Visit <= ',
        quantile(time_diff_to_next_visit, 0.75, na.rm = TRUE),
        ' s',
        sep = ''
      ),
      time_diff_to_next_visit <= 0 ~ paste('Visit <= ', 0, ' s', sep = ''),
      time_diff_to_last_visit <= quantile(time_diff_to_last_visit, 0.25, na.rm = TRUE) ~ paste(
        'Visit <= ',
        quantile(time_diff_to_last_visit, 0.25, na.rm = TRUE),
        ' s',
        sep = ''
      ),
      time_diff_to_last_visit <= median(time_diff_to_last_visit, na.rm = TRUE) ~ paste(
        'Visit <= ',
        median(time_diff_to_last_visit, na.rm = TRUE),
        ' s',
        sep = ''
      ),
      time_diff_to_last_visit > median(time_diff_to_last_visit, na.rm = TRUE) ~ paste(
        'Visit > ',
        median(time_diff_to_last_visit, na.rm = TRUE),
        ' s',
        sep = ''
      )#,
      #TRUE ~ 'Never Visited'
    )
  )

# Join the number of observations across time bins
join_this <- d_tutor$visit_lag %>%
  table() %>%
  tibble::enframe('visit_lag', 'visit_lag_count')

d_tutor2 <- d_tutor %>%
  left_join(join_this, by = 'visit_lag') %>%
  mutate('visit_lag_with_count' = paste(paste(
    visit_lag, format(visit_lag_count, big.mark = ","), sep = ' (N = '
  ), ')', sep = ''))

first_attempt_rows <- d_tutor2 %>%
  arrange(anon_student_id, time) %>%
  distinct(anon_student_id, problem_name, step_name, .keep_all = TRUE) %>%
  pull(row)

d_tutor2['is_first_attempt'] <- d_tutor2$row %in% first_attempt_rows

d_tutor2$is_last_attempt

d_plot <- d_tutor2 %>%
  filter(outcome %in% c("CORRECT", "INCORRECT", "HINT")) %>%
  mutate(is_correct = outcome == "CORRECT") %>%
  count(is_first_attempt, is_correct, visit_lag_with_count)

d_tutor3 <- d_tutor2 %>%
  filter(outcome %in% c("CORRECT", "INCORRECT", "HINT")) %>%
  mutate(is_correct = outcome == "CORRECT")

the_levels <- d_plot$visit_lag_with_count %>%
  gtools::mixedsort() %>%
  unique()

d_plot$visit_lag_with_count <-
  factor(d_plot$visit_lag_with_count, levels = the_levels)

# Predict correctness from number of opportunity

d_tutor3 <- d_tutor3 %>%
  arrange(anon_student_id, time_unix)
d_tutor3['n_opportunity'] <-
  ave(d_tutor3$time_unix, d_tutor3$anon_student_id, FUN = seq_along)

d_tutor3$visited_before_after_fct <-
  factor(
    d_tutor3$visited_before_after,
    levels = c(0,-1, 1),
    labels = c('Not Visited', 'Before First Visit', 'After First Visit')
  )
m <-
  glm(is_correct ~ n_opportunity * visited_before_after_fct,
      d_tutor3,
      family = 'binomial')
summary(m)
#sjPlot::tab_model(m)

m <-
  glm(is_correct ~ is_first_attempt * visit_lag_with_count,
      d_tutor3,
      family = 'binomial')
summary(m)

d_plot <- d_tutor2 %>%
  filter(outcome %in% c("CORRECT", "INCORRECT", "HINT")) %>%
  mutate(is_correct = outcome == "CORRECT") %>%
  mutate(attempt_at_step_cat = ifelse(attempt_at_step >= 10, '10+', as.character(attempt_at_step))) %>%
  group_by(visit_lag_with_count) %>%
  summarize(
    attempt_mean = mean(attempt_at_step),
    attempt_se = my_se(attempt_at_step),
    correct_mean = mean(is_correct),
    correct_se = my_se(is_correct)
  ) %>%
  ungroup() %>%
  filter(!str_detect(visit_lag_with_count, 'NA')) %>%
  pivot_longer(matches('_mean|_sd|_se')) %>%
  separate(name, sep = '_', into = c('var', 'metric')) %>%
  pivot_wider(names_from = metric, values_from = value)

the_levels <- d_plot$visit_lag_with_count %>%
  gtools::mixedsort() %>%
  unique()

d_plot$visit_lag_with_count <-
  factor(d_plot$visit_lag_with_count, levels = the_levels)
ggplot(d_plot, aes(x = visit_lag_with_count, y = mean, group = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = .2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line() +
  labs(x = '', y = 'Frequency of Behavior', color = 'Behavior') +
  theme(legend.position = "top") +
  facet_wrap( ~ var, scales = 'free')

# Low vs. high prior knowledge
d_tutor_tmp <- d_tutor2 %>%
  left_join(d_model %>% select(subject, pk),
            by = c('anon_student_id' = 'subject')) %>%
  mutate(high_pk = pk > median(pk, na.rm = TRUE))

d_plot <- d_tutor_tmp %>%
  filter(outcome %in% c("CORRECT", "INCORRECT", "HINT")) %>%
  mutate(is_correct = outcome == "CORRECT") %>%
  mutate(attempt_at_step_cat = ifelse(attempt_at_step >= 10, '10+', as.character(attempt_at_step))) %>%
  group_by(high_pk, visit_lag_with_count) %>%
  summarize(
    attempt_mean = mean(attempt_at_step),
    attempt_se = my_se(attempt_at_step),
    correct_mean = mean(is_correct),
    correct_se = my_se(is_correct)
  ) %>%
  ungroup() %>%
  filter(!str_detect(visit_lag_with_count, 'NA')) %>%
  pivot_longer(matches('_mean|_sd|_se')) %>%
  separate(name, sep = '_', into = c('var', 'metric')) %>%
  pivot_wider(names_from = metric, values_from = value)

d_plot$visit_lag_with_count <- d_plot$visit_lag_with_count %>%
  str_remove_all(' \\(.*$')

the_levels <- d_plot$visit_lag_with_count %>%
  gtools::mixedsort() %>%
  unique()

d_plot$visit_lag_with_count <-
  factor(d_plot$visit_lag_with_count, levels = the_levels)

d_plot$high_pk <-
  factor(
    d_plot$high_pk,
    levels = c(TRUE, FALSE),
    labels = c('High Prior Knowledge', 'Low Prior Knowledge')
  )
d_plot$var <-
  factor(
    d_plot$var,
    levels = c('correct', 'attempt'),
    labels = c('Correctness', 'Average # Repeated Errors')
  )

# Updated Figure

p1 <-
  ggplot(
    d_plot %>% filter(!is.na(high_pk)) %>% filter(var == "Correctness"),
    aes(x = visit_lag_with_count, y = mean, group = 1)
  ) +
  geom_point(size = 2) +
  geom_errorbar(
    size = 1,
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.5,
    position = position_dodge(0.05)
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(size = 1.5) +
  labs(x = '', y = 'Correctness', color = 'Behavior') +
  theme(legend.position = "top") +
  facet_wrap( ~ high_pk, nrow = 1) +
  theme(text = element_text(size = 20)) +
  scale_y_continuous(n.breaks = 10)

p2 <-
  ggplot(
    d_plot %>% filter(!is.na(high_pk)) %>% filter(var != "Correctness"),
    aes(x = visit_lag_with_count, y = mean, group = 1)
  ) +
  geom_point(size = 2) +
  geom_errorbar(
    size = 1,
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.5,
    position = position_dodge(0.05)
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(size = 1.5) +
  labs(x = '', y = 'Average # Repeated Errors', color = 'Behavior') +
  theme(legend.position = "top") +
  facet_wrap( ~ high_pk, nrow = 1) +
  theme(text = element_text(size = 20)) +
  scale_y_continuous(n.breaks = 10)

gridExtra::grid.arrange(p1, p2, ncol = 2)

# H4 Likelihood ratio tests

d_tutor_tmp2 <- d_tutor_tmp %>%
  filter(outcome %in% c("CORRECT", "INCORRECT", "HINT")) %>%
  mutate(is_correct = outcome == "CORRECT") %>%
  filter(!is.na(high_pk))

m0 <-
  glm(is_correct ~ is_first_attempt, d_tutor_tmp2, family = 'binomial')
m1 <-
  glm(is_correct ~ visit_lag_with_count + is_first_attempt,
      d_tutor_tmp2,
      family = 'binomial')
m2 <-
  glm(is_correct ~ visit_lag_with_count + high_pk + is_first_attempt,
      d_tutor_tmp2,
      family = 'binomial')
m3 <-
  glm(is_correct ~ visit_lag_with_count * high_pk + is_first_attempt,
      d_tutor_tmp2,
      family = 'binomial')

anova(m0, m1, m2, m3, test = 'Chisq')

m0 <- glm(attempt_at_step ~ 1, d_tutor_tmp2, family = 'poisson')
m1 <-
  glm(attempt_at_step ~ visit_lag_with_count, d_tutor_tmp2, family = 'poisson')
m2 <-
  glm(attempt_at_step ~ visit_lag_with_count + high_pk,
      d_tutor_tmp2,
      family = 'poisson')
m3 <-
  glm(attempt_at_step ~ visit_lag_with_count * high_pk,
      d_tutor_tmp2,
      family = 'poisson')

anova(m0, m1, m2, m3, test = 'Chisq')

# H4 AIC-based model selection

ivs <-
  names(d_student_attention_received)[names(d_student_attention_received) !=
                                        'subject']
ivs <-
  c(ivs, names(detector_vars)[names(detector_vars) != 'student_id'])
ivs <- c(ivs, 'pk')

dv = 'ck_lg'
d_forward <-
  d_model[, c(ivs, dv)]
d_forward <-
  d_forward %>% mutate_if(is.numeric, scale)
d_forward <-
  d_forward[, which(colMeans(!is.na(d_forward)) > 0.01)]
d_forward <-
  d_forward %>% drop_na()
full.model <-
  lm(paste(c(dv, ' ~ .'), collapse = ''), data = d_forward)
step.model <- MASS::stepAIC(full.model,
                            #scope = . ~ .^2, # add interactions
                            direction = "both",  # Both and backward result in the same model, forward in much more complex model -> robustness
                            trace = FALSE)
sjPlot::tab_model(step.model)

dv = 'pk_lg'
d_forward <-
  d_model[, c(ivs, dv)]
d_forward <-
  d_forward %>% mutate_if(is.numeric, scale)
d_forward <-
  d_forward[, which(colMeans(!is.na(d_forward)) > 0.01)]
d_forward <-
  d_forward %>% drop_na()
full.model <-
  lm(paste(c(dv, ' ~ .'), collapse = ''), data = d_forward)
step.model <- MASS::stepAIC(full.model,
                            #scope = . ~ .^2, # add interactions
                            direction = "both",  # Both and backward result in the same model, forward in much more complex model -> robustness
                            trace = FALSE)
sjPlot::tab_model(step.model)

# H2: Is the teacher visiting the student that has been idle/struggle longest next? How often is that the case?

# Step 1: For each time stamp, which student was idle, etc longest?
# To compute this, we exploit the diff to next visit variable by getting the visited student_id via students :: argmin(diff_to_next_vist)
# and then comparing the cumulative idleness, struggle, misuse for their most recent sequence and checking
# if the largest cumulative sum coincides with the student_id identified via the argmin step

d_detector2 <- d_detector2 %>%
  arrange(student_id, time_unix)

cumsum_reset_at_zero = function(x) {
  cs = cumsum(x)
  ans = cs - cummax((x == 0) * cs)
  return(ans)
}

d_detector2['start_idle'] <-
  d_detector2$is_idle != lag(d_detector2$is_idle, 1)
d_detector2['start_misuse'] <-
  d_detector2$is_misuse != lag(d_detector2$is_misuse, 1)
d_detector2['start_struggle'] <-
  d_detector2$is_struggle != lag(d_detector2$is_struggle, 1)

# Priorities to visit based on length of non-zero sequence length -> largest value accumulated without 0 entry because students may become unstuck
d_detector2$idle_csum <-
  ave(d_detector2$is_idle, d_detector2$student_id, FUN = cumsum_reset_at_zero)
d_detector2$misuse_csum <-
  ave(d_detector2$is_misuse, d_detector2$student_id, FUN = cumsum_reset_at_zero)
d_detector2$struggle_csum <-
  ave(d_detector2$is_struggle, d_detector2$student_id, FUN = cumsum_reset_at_zero)

d_full <- d_detector2 %>%
  select(time_unix,
         time_diff_to_next_visit,
         student_id,
         matches('csum')) %>%
  arrange(student_id, time_unix)

d_full_tmp <- d_full %>%
  left_join(d_times %>% select(subject, times_start),
            by = c('student_id' = 'subject')) %>%
  mutate(time_diff_to_next_visit = map2_dbl(time_unix, times_start, diff_next_visit))

all_visits <- d_full_tmp$times_start %>% unlist() %>% unique()

d_full <- d_full %>%
  mutate(times_start = rep(list(all_visits), nrow(.))) %>%
  mutate(time_diff_to_next_visit = map2_dbl(time_unix, times_start, diff_next_visit))

v2p <- list()
i <- 1
for (visit in all_visits) {
  d_full <- d_full %>%
    mutate(delta = visit - time_unix)
  tmp <- d_full %>%
    filter(delta <= 60 * 30 &
             delta >= 0) # take into account most recent 30 mins (30*60s)
  tmp <- tmp %>%
    group_by(student_id) %>%
    summarize(
      idle_closest = idle_csum[which.min(time_diff_to_next_visit)],
      misuse_closest = misuse_csum[which.min(time_diff_to_next_visit)],
      struggle_closest = struggle_csum[which.min(time_diff_to_next_visit)]
    ) %>%
    ungroup()
  v2p[[i]] <- tmp
  i <- i + 1
  cat(visit, '\n')
}

# visit -> student mapping
ref <- d_times %>%
  unchop(times_start) %>%
  distinct(subject, times_start)

v2s <- list()
i <- 1
for (visit in all_visits) {
  tmp <- ref %>%
    filter(times_start == visit) %>%
    pull(subject)
  v2s[[i]] <- tmp
  i <- i + 1
}

# how often are v2p and v2s values same for different behaviors?
get_priority <- function(df, ref = 'idle_closest') {
  ans <- df[order(-df[ref]), ] %>%
    head(1) %>%
    pull(student_id)
  return(ans)
}

get_overlap <- function(v1, v2) {
  return(length(base::intersect(v1, v2)) > 0)
}

idle_prio <- v2p %>%
  map_chr(get_priority)

misuse_prio <- v2p %>%
  map_chr( ~ get_priority(., ref = 'misuse_closest'))

struggle_prio <- v2p %>%
  map_chr( ~ get_priority(., ref = 'struggle_closest'))

# any of the four
any_prio <- pmap(list(idle_prio, misuse_prio, struggle_prio), c)

# Ground truth sets
check_if_in <- function(s, v) {
  return(s %in% v)
}

ans_idle <- ifelse(map2_lgl(idle_prio, v2s, check_if_in), 1, 0)
ans_misuse <- ifelse(map2_lgl(misuse_prio, v2s, check_if_in), 1, 0)
ans_struggle <-
  ifelse(map2_lgl(struggle_prio, v2s, check_if_in), 1, 0)
ans_any <- ifelse(map2_lgl(any_prio, v2s, get_overlap), 1, 0)

ref <- d_times %>%
  select(subject, times_start) %>%
  unchop(times_start) %>%
  distinct(times_start, .keep_all = TRUE) %>%
  mutate(
    visited_idle_prio = ans_idle,
    visited_misuse_prio = ans_misuse,
    visited_struggle_prio = ans_struggle,
    visited_any_prior = ans_any
  )
all.equal(ref$times_start, all_visits)

d_plotdata <- ref %>%
  left_join(d %>% filter(str_detect(subject, 'Stu_')) %>% distinct(period_id, subject), by = 'subject') %>%
  #group_by(period_id) %>%
  mutate(
    idle_conf = sum(visited_idle_prio == 1) / n(),
    idle_lower = idle_conf - qnorm(1 - 0.05 / 2) * sqrt((1 / n()) * idle_conf *
                                                          (1 - idle_conf)),
    idle_upper = idle_conf + qnorm(1 - 0.05 / 2) * sqrt((1 / n()) * idle_conf *
                                                          (1 - idle_conf)),
    
    misuse_conf = sum(visited_misuse_prio == 1) / n(),
    misuse_lower = misuse_conf - qnorm(1 - 0.05 / 2) * sqrt((1 / n()) *
                                                              misuse_conf * (1 - misuse_conf)),
    misuse_upper = misuse_conf + qnorm(1 - 0.05 / 2) * sqrt((1 / n()) *
                                                              misuse_conf * (1 - misuse_conf)),
    
    struggle_conf = sum(visited_struggle_prio == 1) / n(),
    struggle_lower = struggle_conf - qnorm(1 - 0.05 / 2) * sqrt((1 / n()) *
                                                                  struggle_conf * (1 - struggle_conf)),
    struggle_upper = struggle_conf + qnorm(1 - 0.05 / 2) * sqrt((1 / n()) *
                                                                  struggle_conf * (1 - struggle_conf)),
    
    any_conf = sum(visited_any_prior == 1) / n(),
    any_lower = any_conf - qnorm(1 - 0.05 / 2) * sqrt((1 / n()) * any_conf *
                                                        (1 - any_conf)),
    any_upper = any_conf + qnorm(1 - 0.05 / 2) * sqrt((1 / n()) * any_conf *
                                                        (1 - any_conf)),
  ) %>%
  mutate(period2 = 'all') %>%
  distinct(period2, .keep_all = TRUE) %>%
  select(period_id = period2, matches('conf|lower|upper'))

d_plot <- d_plotdata %>%
  pivot_longer(!period_id) %>%
  separate(name, into = c('behavior', 'var'), sep = '_') %>%
  pivot_wider(names_from = var, values_from = value)

ggplot(d_plot,
       aes(
         x = period_id,
         y = conf,
         group = behavior,
         color = behavior
       )) +
  geom_point(position = position_dodge(0.8)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = .2,
                position = position_dodge(0.8)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = 'Frequency of Behavior', color = 'Behavior') +
  theme(legend.position = "top")
