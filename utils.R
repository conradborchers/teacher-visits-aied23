# Collection of common functions

df2clipboard <- function(x,
                         row.names = FALSE,
                         col.names = TRUE,
                         ...) {
  con <- pipe("xclip -selection clipboard -i", open = "w")
  write.table(
    x,
    "clipboard",
    sep = "\t",
    row.names = row.names,
    col.names = col.names,
    ...
  )
  close(con)
}

clipboard <- function(x,
                      sep = "\t",
                      row.names = FALSE,
                      col.names = TRUE) {
  con <- pipe("xclip -selection clipboard -i", open = "w")
  write.table(x,
              con,
              sep = sep,
              row.names = row.names,
              col.names = col.names)
  close(con)
}

# Standard Error
my_se <- function(v)
  sd(v, na.rm = TRUE) / length(v)

create_detector_variables <- function(d_detector, add_suffix = '') {
  # Fill NA
  d_detector <- d_detector %>%
    mutate(struggle = ifelse(is.na(struggle), 0, struggle)) %>%
    mutate(idle = ifelse(is.na(idle), 0, idle)) %>%
    mutate(misuse = ifelse(is.na(misuse), 0, misuse)) %>%
    mutate(gaming = ifelse(is.na(gaming), 0, gaming))
  
  # Cleaning
  d_detector %>%
    arrange(time) %>%
    mutate(check = time - lag(time, 1)) %>%
    pull(check) %>% # lag in seconds
    (function(v) {
      return(v[v < 1000])
    }) %>%  # remove transitions that stretch across multiple days/sessions
    mean(na.rm = TRUE)
  
  # The following code produces several features leveraging the 
  # longitudinal nature of our disengagement and struggle detector variables,
  # for example, the average length of sequences of each measure, with sequences
  # being delineated by an absence of detected disengagement/struggle
  d_detector['misuse_lag'] <- d_detector$misuse %>% lag(1)
  d_detector['gaming_lag'] <- d_detector$gaming %>% lag(1)
  d_detector['idle_lag'] <- d_detector$idle %>% lag(1)
  d_detector['struggle_lag'] <- d_detector$struggle %>% lag(1)
  
  # For each variable, create average, average sequence max as magnitude, average length as duration length and number of sequences as fluctuation
  d_detector['misuse_start_new_sequence'] <-
    d_detector$misuse != 0 & d_detector$misuse_lag == 0
  d_detector['misuse_stop_new_sequence'] <-
    d_detector$misuse == 0 & d_detector$misuse_lag != 0
  d_detector['misuse_ongoing_sequence'] <- d_detector$misuse != 0
  
  # Get max values of subsequences
  d_detector <- d_detector %>% data.table()
  d_detector[, misuse_id := rleid(misuse == 0)]
  d_detector <- d_detector %>% tibble() # subsequence ID
  d_detector <- d_detector %>% data.table()
  d_detector[, misuse_max_val := max(misuse), by = misuse_id]
  d_detector <- d_detector %>% tibble() # rolling max
  d_detector['misuse_max_single'] <- 0
  d_detector$misuse_max_single[d_detector$misuse_start_new_sequence %>% map_lgl(~isTRUE(.x))] <-
    d_detector$misuse_max_val[d_detector$misuse_start_new_sequence %>% map_lgl(~isTRUE(.x))] # Vector with only max values of sequences
  
  misuse_vars <- d_detector %>%
    group_by(student_id) %>%
    dplyr::summarize(
      misuse_avg_score = mean(misuse, na.rm = TRUE),
      misuse_n_seq = sum(misuse_start_new_sequence, na.rm = TRUE),
      misuse_avg_length = sum(misuse_ongoing_sequence, na.rm = TRUE) / sum(misuse_start_new_sequence, na.rm =
                                                                             TRUE),
      misuse_avg_max_of_sequence = mean(misuse_max_single[misuse_max_single != 0], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(misuse_avg_max_of_sequence = misuse_avg_max_of_sequence %>% (function(v) {
      v[is.na(v)] <- 0
      return(v)
    })) %>%
    mutate(misuse_avg_length = misuse_avg_length %>% (function(v) {
      v[is.na(v)] <- 0
      return(v)
    }))
  
  d_detector['gaming_start_new_sequence'] <-
    d_detector$gaming != 0 & d_detector$gaming_lag == 0
  d_detector['gaming_stop_new_sequence'] <-
    d_detector$gaming == 0 & d_detector$gaming_lag != 0
  d_detector['gaming_ongoing_sequence'] <- d_detector$gaming != 0
  d_detector <-
    d_detector %>% data.table()
  d_detector[, gaming_id := rleid(gaming == 0)]
  d_detector <- d_detector %>% tibble() # subsequence ID
  d_detector <-
    d_detector %>% data.table()
  d_detector[, gaming_max_val := max(gaming), by = gaming_id]
  d_detector <- d_detector %>% tibble() # rolling max
  d_detector['gaming_max_single'] <- 0
  d_detector$gaming_max_single[d_detector$gaming_start_new_sequence %>% map_lgl(~isTRUE(.x))] <-
    d_detector$gaming_max_val[d_detector$gaming_start_new_sequence %>% map_lgl(~isTRUE(.x))] # retain singles
  
  gaming_vars <- d_detector %>%
    group_by(student_id) %>%
    dplyr::summarize(
      gaming_avg_score = mean(gaming, na.rm = TRUE),
      gaming_n_seq = sum(gaming_start_new_sequence, na.rm = TRUE),
      gaming_avg_length = sum(gaming_ongoing_sequence, na.rm = TRUE) / sum(gaming_start_new_sequence, na.rm =
                                                                             TRUE),
      gaming_avg_max_of_sequence = mean(gaming_max_single[gaming_max_single !=
                                                            0], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(gaming_avg_max_of_sequence = gaming_avg_max_of_sequence %>% (function(v) {
      v[is.na(v)] <- 0
      return(v)
    })) %>%
    mutate(gaming_avg_length = gaming_avg_length %>% (function(v) {
      v[is.na(v)] <- 0
      return(v)
    })) %>%
    mutate(gaming_avg_length = gaming_avg_length %>% (function(v) {
      v[is.infinite(v)] <- 0
      return(v)
    }))
  
  d_detector['idle_start_new_sequence'] <-
    d_detector$idle != 0 & d_detector$idle_lag == 0
  d_detector['idle_stop_new_sequence'] <-
    d_detector$idle == 0 & d_detector$idle_lag != 0
  d_detector['idle_ongoing_sequence'] <- d_detector$idle != 0
  d_detector <-
    d_detector %>% data.table()
  d_detector[, idle_id := rleid(idle == 0)]
  d_detector <- d_detector %>% tibble() # subsequence ID
  d_detector <-
    d_detector %>% data.table()
  d_detector[, idle_max_val := max(idle), by = idle_id]
  d_detector <- d_detector %>% tibble() # rolling max
  d_detector['idle_max_single'] <-
    0
  d_detector$idle_max_single[d_detector$idle_start_new_sequence %>% map_lgl(~isTRUE(.x))] <-
    d_detector$idle_max_val[d_detector$idle_start_new_sequence %>% map_lgl(~isTRUE(.x))] # retain singles
  
  idle_vars <- d_detector %>%
    group_by(student_id) %>%
    dplyr::summarize(
      idle_avg_score = mean(idle, na.rm = TRUE),
      idle_n_seq = sum(idle_start_new_sequence, na.rm = TRUE),
      idle_avg_length = sum(idle_ongoing_sequence, na.rm = TRUE) / sum(idle_start_new_sequence, na.rm = TRUE),
      idle_avg_max_of_sequence = mean(idle_max_single[idle_max_single != 0], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(idle_avg_max_of_sequence = idle_avg_max_of_sequence %>% (function(v) {
      v[is.na(v)] <- 0
      return(v)
    })) %>%
    mutate(idle_avg_length = idle_avg_length %>% (function(v) {
      v[is.na(v)] <- 0
      return(v)
    })) %>%
    mutate(idle_avg_length = idle_avg_length %>% (function(v) {
      v[is.infinite(v)] <- 0
      return(v)
    }))
  
  d_detector['struggle_start_new_sequence'] <-
    d_detector$struggle != 0 & d_detector$struggle_lag == 0
  d_detector['struggle_stop_new_sequence'] <-
    d_detector$struggle == 0 & d_detector$struggle_lag != 0
  d_detector['struggle_ongoing_sequence'] <-
    d_detector$struggle != 0
  d_detector <-
    d_detector %>% data.table()
  d_detector[, struggle_id := rleid(struggle == 0)]
  d_detector <- d_detector %>% tibble() # subsequence ID
  d_detector <-
    d_detector %>% data.table()
  d_detector[, struggle_max_val := max(struggle), by = struggle_id]
  d_detector <- d_detector %>% tibble() # rolling max
  d_detector['struggle_max_single'] <-
    0
  d_detector$struggle_max_single[d_detector$struggle_start_new_sequence %>% map_lgl(~isTRUE(.x))] <-
    d_detector$struggle_max_val[d_detector$struggle_start_new_sequence %>% map_lgl(~isTRUE(.x))] # retain singles
  
  struggle_vars <- d_detector %>%
    group_by(student_id) %>%
    dplyr::summarize(
      struggle_avg_score = mean(struggle, na.rm = TRUE),
      struggle_n_seq = sum(struggle_start_new_sequence, na.rm = TRUE),
      struggle_avg_length = sum(struggle_ongoing_sequence, na.rm = TRUE) /
        sum(struggle_start_new_sequence, na.rm = TRUE),
      struggle_avg_max_of_sequence = mean(struggle_max_single[struggle_max_single !=
                                                                0], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(struggle_avg_max_of_sequence = struggle_avg_max_of_sequence %>% (function(v) {
      v[is.na(v)] <- 0
      return(v)
    })) %>%
    mutate(struggle_avg_length = struggle_avg_length %>% (function(v) {
      v[is.na(v)] <- 0
      return(v)
    })) %>%
    mutate(struggle_avg_length = struggle_avg_length %>% (function(v) {
      v[is.infinite(v)] <- 0
      return(v)
    }))
  
  detector_vars <- misuse_vars %>%
    full_join(gaming_vars, by = 'student_id') %>%
    full_join(idle_vars, by = 'student_id') %>%
    full_join(struggle_vars, by = 'student_id')
  
  if (add_suffix != '') {
    names(detector_vars)[names(detector_vars) != 'student_id'] <-
      paste(names(detector_vars)[names(detector_vars) != 'student_id'], add_suffix, sep =
              '_')
  }
  
  return(detector_vars)
}

diff_last_visit <- function(a, b) {
  ##
  # We want to return the difference to the time when the teacher last visited
  # Reference should be end of timestamp of visit timeframe
  ##
  pos_b <- b[(a - b) >= 0]
  if (length(pos_b) == 0)
    return(NA)
  closest_b <- pos_b[which.min(a - pos_b)]
  return(a - closest_b)
}

diff_next_visit <- function(a, b) {
  ##
  # We want to return the difference to the time when the teacher will next visit
  # Reference should be start of timestamp of visit timeframe
  ##
  neg_b <- b[(a - b) <= 0]
  if (length(neg_b) == 0)
    return(NA)
  next_b <- neg_b[which.max(a - neg_b)]
  return(a - next_b)
}

diff_closest_visit <- function(a, b) {
  ##
  # We want to return the difference to the next closest visit, independent of whether it
  # is in the future or was in the past
  # Reference should be time frame center of timestamp of visit timeframe
  ##
  if (length(b) == 0)
    return(NA)
  best_b <- b[which.min(abs(a - b))]
  return(a - best_b)
}
