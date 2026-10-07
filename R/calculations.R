library(dplyr)
library(lubridate)

# ---- progress ---------------------------------------------------------

# Project completion %, weighted by est_hours per stage (equal weight if
# blank). done = 1, in_progress = row fraction (or 0.5 without row
# tracking), not_started = 0.
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

# Total and average hours logged per session, grouped by stage category and
# yarn weight.
hours_per_stage_by_weight <- function(sessions, stages, projects) {
  sessions %>%
    left_join(stages %>% select(stage_id, stage_category), by = "stage_id") %>%
    left_join(projects %>% select(project_id, yarn_weight, size), by = "project_id") %>%
    group_by(yarn_weight, stage_category) %>%
    summarise(total_hours = sum(hours), n_sessions = n(), .groups = "drop") %>%
    mutate(avg_hours_per_session = round(total_hours / n_sessions, 2)) %>%
    arrange(yarn_weight, stage_category)
}

# Total hours logged per project.
total_hours_per_project <- function(sessions, projects) {
  sessions %>%
    group_by(project_id) %>%
    summarise(total_hours = sum(hours), n_sessions = n(), .groups = "drop") %>%
    right_join(projects %>% select(project_id, name, yarn_weight, size, colour), by = "project_id") %>%
    mutate(total_hours = coalesce(total_hours, 0)) %>%
    arrange(desc(total_hours))
}

# Total hours logged per calendar day, from the first logged session to
# today, with 0 filled in for days nothing was logged.
hours_per_day <- function(sessions) {
  if (nrow(sessions) == 0) {
    return(tibble(date = as.Date(character()), total_hours = numeric()))
  }
  all_days <- tibble(date = seq(min(sessions$date), Sys.Date(), by = "day"))
  sessions %>%
    group_by(date) %>%
    summarise(total_hours = sum(hours), .groups = "drop") %>%
    right_join(all_days, by = "date") %>%
    mutate(total_hours = coalesce(total_hours, 0)) %>%
    arrange(date)
}

# Completed stages with a recorded original estimate: how actual hours
# logged compared to that estimate. Stages finished before
# original_est_hours existed have no comparison available.
estimate_accuracy <- function(stages, sessions, projects) {
  stages %>%
    filter(status == "done", !is.na(original_est_hours)) %>%
    left_join(
      sessions %>% group_by(stage_id) %>% summarise(actual_hours = sum(hours), .groups = "drop"),
      by = "stage_id"
    ) %>%
    left_join(projects %>% select(project_id, name), by = "project_id") %>%
    mutate(
      actual_hours = coalesce(actual_hours, 0),
      diff_hours = round(actual_hours - original_est_hours, 2),
      diff_pct = round((actual_hours - original_est_hours) / original_est_hours * 100, 0)
    ) %>%
    select(name, stage_name, stage_category, original_est_hours, actual_hours, diff_hours, diff_pct) %>%
    arrange(desc(abs(diff_pct)))
}

# Remaining hours per project: sum of est_hours across not-done stages
# (in_progress at half weight). Missing est_hours falls back to the average
# hours per completed stage of that yarn weight, or 4 hours if no history.
project_remaining_hours <- function(projects, stages, sessions) {
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

# Ranks every active project's current stage using the same critical-path
# logic as the Gantt tab's bottleneck calc, not each project's own isolated
# slack: a project's urgency is how much schedule buffer is left once you
# account for every other commitment due at or before its own deadline
# (cum_hours_needed from critical_pace_by_deadline()), at the user's
# assumed shared knitting pace. This means the top suggestion can change
# whenever any project's remaining hours or deadline shifts, even one not
# shown in the list - it's reflecting real competition for the same hours,
# not a stable per-project score.
recommend_projects <- function(projects, stages, sessions, hours_per_week = 14) {
  current <- current_stage(stages)
  daily_pace <- hours_per_week / 7

  candidates <- projects %>%
    filter(status == "active") %>%
    inner_join(current, by = "project_id")

  empty_result <- tibble(
    project_id = character(), name = character(), stage_name = character(),
    row_progress = character(), portability = integer(), focus = integer(),
    deadline = as.Date(character()), days_left = numeric(),
    critical_slack_days = numeric(), required_pace = numeric()
  )
  if (nrow(candidates) == 0) {
    return(empty_result)
  }

  cp <- critical_pace_by_deadline(projects, stages, sessions)
  own_remaining <- project_remaining_hours(projects, stages, sessions)

  # A candidate whose deadline has already passed (but is still marked
  # active) won't appear in cp, which only covers upcoming deadlines - fall
  # back to its own remaining hours so it still surfaces as urgent instead
  # of silently dropping to the bottom via an NA slack.
  candidates %>%
    left_join(cp %>% select(project_id, days_to_here, cum_hours_needed, required_pace), by = "project_id") %>%
    left_join(own_remaining, by = "project_id") %>%
    mutate(
      days_left = as.numeric(as.Date(deadline) - Sys.Date()),
      days_to_here = coalesce(days_to_here, days_left),
      cum_hours_needed = coalesce(cum_hours_needed, remaining_hours, 0),
      critical_slack_days = round(days_to_here - cum_hours_needed / daily_pace, 1),
      required_pace = round(required_pace, 2),
      row_progress = ifelse(
        !is.na(total_rows),
        paste0("row ", coalesce(rows_done, 0), " of ", total_rows),
        NA
      )
    ) %>%
    arrange(critical_slack_days, days_left) %>%
    select(project_id, name, stage_name, row_progress, portability, focus, deadline, days_left, critical_slack_days, required_pace)
}

# ---- gantt data ---------------------------------------------------------

# Per active project: a bar from today to the estimated finish date at
# hours_per_week, plus the real deadline.
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

# Remaining hours per project (deadline not yet passed), sorted by deadline,
# with the cumulative hours needed by each deadline and the pace that would
# be required, assuming today's hours can be freely allocated to whichever
# project is due soonest.
critical_pace_by_deadline <- function(projects, stages, sessions) {
  active <- projects %>% filter(is.na(deadline) | as.Date(deadline) >= Sys.Date())
  remaining <- project_remaining_hours(active, stages, sessions)

  df <- active %>%
    left_join(remaining, by = "project_id") %>%
    mutate(remaining_hours = coalesce(remaining_hours, 0), deadline = as.Date(deadline)) %>%
    select(project_id, name, deadline, remaining_hours) %>%
    arrange(deadline)

  df %>%
    rowwise() %>%
    mutate(
      days_to_here = as.numeric(deadline - Sys.Date()),
      cum_hours_needed = sum(df$remaining_hours[df$deadline <= deadline]),
      required_pace = ifelse(days_to_here > 0, cum_hours_needed / days_to_here, Inf)
    ) %>%
    ungroup()
}

# The single tightest deadline: the one needing the highest sustained daily
# pace (from today) to hit every deadline up to and including it.
critical_bottleneck <- function(projects, stages, sessions) {
  cp <- critical_pace_by_deadline(projects, stages, sessions)
  cp[which.max(cp$required_pace), ]
}

# ---- burndown ---------------------------------------------------------

# Actual total remaining hours (active projects) on each day since the
# first logged session, reconstructed as today's total plus hours logged
# after that day. Paired with an ideal straight-line pace to 0 hours at
# the furthest deadline.
burndown_data <- function(projects, stages, sessions) {
  active <- projects %>% filter(is.na(deadline) | as.Date(deadline) >= Sys.Date())
  remaining_now <- sum(project_remaining_hours(active, stages, sessions)$remaining_hours)
  final_deadline <- suppressWarnings(max(as.Date(active$deadline), na.rm = TRUE))

  # Nothing to plot without at least one project with a real deadline.
  if (!is.finite(final_deadline)) {
    return(tibble(date = as.Date(character()), hours = numeric(), line = character()))
  }

  start_date <- if (nrow(sessions) == 0) Sys.Date() else min(sessions$date)
  actual_dates <- seq(start_date, Sys.Date(), by = "day")
  actual <- tibble(date = actual_dates) %>%
    rowwise() %>%
    mutate(hours = remaining_now + sum(sessions$hours[sessions$date > date])) %>%
    ungroup() %>%
    mutate(line = "Actual")

  ideal_dates <- seq(start_date, final_deadline, by = "day")
  start_hours <- actual$hours[1]
  span_days <- as.numeric(final_deadline - start_date)
  ideal <- tibble(date = ideal_dates) %>%
    mutate(
      hours = pmax(0, start_hours * (1 - as.numeric(date - start_date) / span_days)),
      line = "Ideal"
    )

  bind_rows(actual, ideal)
}
