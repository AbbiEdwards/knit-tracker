library(dplyr)
library(lubridate)

# ---- progress ---------------------------------------------------------

# Progress per project, weighted by est_hours where available (else equal
# weight per stage). "done" counts as 1, "not_started" as 0. An "in_progress"
# stage counts as its row-completion fraction if it has row tracking,
# otherwise a flat 0.5 guess.
project_progress <- function(projects, stages) {
  stages %>%
    mutate(
      weight = ifelse(is.na(est_hours) | est_hours <= 0, 1, est_hours),
      row_frac = ifelse(
        !is.na(total_rows) & total_rows > 0,
        coalesce(rows_done, 0) / total_rows,
        0.5
      )
    ) %>%
    group_by(project_id) %>%
    summarise(
      pct_complete = sum(weight * case_when(
        status == "done" ~ 1,
        status == "in_progress" ~ row_frac,
        TRUE ~ 0
      )) / sum(weight),
      n_stages = n(),
      n_done = sum(status == "done"),
      .groups = "drop"
    ) %>%
    right_join(projects, by = "project_id") %>%
    mutate(pct_complete = coalesce(pct_complete, 0)) %>%
    arrange(deadline)
}

# ---- time estimates from history ---------------------------------------

# Average hours actually logged per stage, by yarn weight, to refine future
# estimates / sanity-check est_hours (and, over time, to judge how many
# projects of a given yarn weight are realistic to take on at once).
hours_per_stage_by_weight <- function(sessions, stages, projects) {
  sessions %>%
    left_join(stages %>% select(stage_id, stage_name), by = "stage_id") %>%
    left_join(projects %>% select(project_id, yarn_weight, size), by = "project_id") %>%
    group_by(yarn_weight, stage_name) %>%
    summarise(total_hours = sum(hours), n_sessions = n(), .groups = "drop") %>%
    mutate(avg_hours_per_session = round(total_hours / n_sessions, 2)) %>%
    arrange(yarn_weight, stage_name)
}

total_hours_per_project <- function(sessions, projects) {
  sessions %>%
    group_by(project_id) %>%
    summarise(total_hours = sum(hours), n_sessions = n(), .groups = "drop") %>%
    right_join(projects %>% select(project_id, name, yarn_weight, size), by = "project_id") %>%
    mutate(total_hours = coalesce(total_hours, 0)) %>%
    arrange(desc(total_hours))
}

# Remaining estimated hours for a project: sum of est_hours for stages not
# yet done (in_progress counted at half), falling back to average historical
# hours/stage for that yarn weight when est_hours is missing, or a generic
# 4-hour guess when there's no history either.
project_remaining_hours <- function(projects, stages, sessions) {
  # Historical average is per completed STAGE, not per logged session - a
  # stage finished across four 1-hour sessions took 4 hours, not 1, so
  # sessions are summed up to stage level (for stages actually marked
  # "done") before averaging by yarn weight.
  hist_avg <- sessions %>%
    group_by(stage_id) %>%
    summarise(stage_hours = sum(hours), .groups = "drop") %>%
    left_join(stages %>% select(stage_id, project_id, status), by = "stage_id") %>%
    filter(status == "done") %>%
    left_join(projects %>% select(project_id, yarn_weight), by = "project_id") %>%
    group_by(yarn_weight) %>%
    summarise(avg_hours_per_stage = mean(stage_hours, na.rm = TRUE), .groups = "drop")

  stages %>%
    left_join(projects %>% select(project_id, yarn_weight), by = "project_id") %>%
    left_join(hist_avg, by = "yarn_weight") %>%
    mutate(
      fallback = coalesce(avg_hours_per_stage, 4),
      remaining_factor = case_when(
        status == "done" ~ 0,
        status == "in_progress" ~ 0.5,
        TRUE ~ 1
      ),
      stage_remaining = remaining_factor * coalesce(est_hours, fallback)
    ) %>%
    group_by(project_id) %>%
    summarise(remaining_hours = sum(stage_remaining), .groups = "drop")
}

# ---- current stage per project (first not "done", in order) -----------

current_stage <- function(stages) {
  stages %>%
    filter(status != "done") %>%
    group_by(project_id) %>%
    slice_min(stage_order, n = 1) %>%
    ungroup() %>%
    rename(stage_status = status)
}

# ---- recommender ---------------------------------------------------------

location_min_portability <- c(
  "1 - Home (space to spread out)" = 1,
  "2 - Work/Cafe/Gathering (space but limited)" = 2,
  "3 - On the Move (confined space)" = 3
)

energy_max_focus <- c(
  "1 - Low / tired" = 1,
  "2 - Medium" = 2,
  "3 - High / fresh" = 3
)

# A short session isn't worth the setup/orientation cost of starting a stage
# cold, so it's restricted to stages already under way.
TIME_CHOICES <- c("Quick (under ~45 min)", "Medium (up to ~2 hrs)", "Long / open-ended")

# Suggests active projects whose current stage fits the given location,
# energy level and time available, ranked by "slack" (deadline minus the
# time still needed at hours_per_week) so the most time-pressured project
# comes first - not just whichever deadline happens to be soonest.
recommend_projects <- function(projects, stages, sessions, location, energy,
                                time_available, hours_per_week = 14) {
  min_portability <- location_min_portability[[location]]
  max_focus <- energy_max_focus[[energy]]
  current <- current_stage(stages)
  remaining <- project_remaining_hours(projects, stages, sessions)

  candidates <- projects %>%
    filter(status == "active") %>%
    inner_join(current, by = "project_id") %>%
    filter(portability >= min_portability, focus <= max_focus)

  if (time_available == "Quick (under ~45 min)") {
    candidates <- candidates %>% filter(stage_status == "in_progress")
  }

  candidates %>%
    left_join(remaining, by = "project_id") %>%
    mutate(
      days_left = as.numeric(as.Date(deadline) - Sys.Date()),
      remaining_hours = coalesce(remaining_hours, 0),
      days_needed = remaining_hours / hours_per_week * 7,
      slack_days = round(days_left - days_needed, 1),
      row_progress = ifelse(
        !is.na(total_rows),
        paste0("row ", coalesce(rows_done, 0), " of ", total_rows),
        NA
      )
    ) %>%
    arrange(slack_days) %>%
    select(project_id, name, stage_name, row_progress, portability, focus, deadline, days_left, slack_days)
}

# ---- gantt data ---------------------------------------------------------

# For each active project: a bar from today to the estimated finish date if
# worked on exclusively from now at hours_per_week, plus the real deadline.
gantt_data <- function(projects, stages, sessions, hours_per_week = 14) {
  remaining <- project_remaining_hours(projects, stages, sessions)

  projects %>%
    filter(status == "active") %>%
    left_join(remaining, by = "project_id") %>%
    mutate(
      remaining_hours = coalesce(remaining_hours, 0),
      est_days_needed = pmax(1, ceiling(remaining_hours / hours_per_week * 7)),
      start = Sys.Date(),
      finish_if_started_now = Sys.Date() + est_days_needed,
      deadline = as.Date(deadline)
    ) %>%
    select(project_id, name, start, finish_if_started_now, deadline, remaining_hours)
}
