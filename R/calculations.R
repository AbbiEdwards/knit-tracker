library(dplyr)

# ---- shared helpers ------------------------------------------------------

# Total hours logged against each stage.
stage_hours <- function(sessions) {
  sessions %>% group_by(stage_id) %>% summarise(actual_hours = sum(hours), .groups = "drop")
}

# Projects whose deadline hasn't passed yet. Undated projects (e.g. on hold)
# are kept only when include_undated is TRUE: they still belong on Overview,
# but can't be scheduled against a date.
upcoming_projects <- function(projects, include_undated = FALSE) {
  projects %>% filter(coalesce(deadline >= Sys.Date(), include_undated))
}

# ---- progress ---------------------------------------------------------

# Fraction complete for a single stage: done = 1, not_started = 0.
# in_progress uses real row progress where tracked; otherwise hours logged
# against est_hours, capped just under 1 so a stage can't visually read as
# finished before you've actually marked it done. Falls back to a flat 0.5
# only when there's neither row tracking nor an est_hours to compare against.
stage_progress_fraction <- function(stages, sessions) {
  stages %>%
    left_join(stage_hours(sessions), by = "stage_id") %>%
    mutate(
      frac = case_when(
        status == "done" ~ 1,
        status == "in_progress" & !is.na(total_rows) & total_rows > 0 ~
          coalesce(rows_done, 0) / total_rows,
        status == "in_progress" & !is.na(est_hours) & est_hours > 0 ~
          pmin(coalesce(actual_hours, 0) / est_hours, 0.95),
        status == "in_progress" ~ 0.5,
        TRUE ~ 0
      )
    ) %>%
    select(-actual_hours)
}

# Project completion %, weighted by est_hours per stage (equal weight if
# blank), using stage_progress_fraction() for each stage's own completion.
project_progress <- function(projects, stages, sessions) {
  stage_progress_fraction(stages, sessions) %>%
    mutate(weight = ifelse(is.na(est_hours) | est_hours <= 0, 1, est_hours)) %>%
    group_by(project_id) %>%
    summarise(pct_complete = sum(weight * frac) / sum(weight), .groups = "drop") %>%
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
    left_join(projects %>% select(project_id, yarn_weight), by = "project_id") %>%
    group_by(yarn_weight, stage_category) %>%
    summarise(total_hours = sum(hours), n_sessions = n(), .groups = "drop") %>%
    mutate(avg_hours_per_session = round(total_hours / n_sessions, 2)) %>%
    arrange(yarn_weight, stage_category)
}

# Total hours logged per project.
total_hours_per_project <- function(sessions, projects) {
  sessions %>%
    group_by(project_id) %>%
    summarise(total_hours = sum(hours), .groups = "drop") %>%
    right_join(projects %>% select(project_id, name, colour), by = "project_id") %>%
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
    left_join(stage_hours(sessions), by = "stage_id") %>%
    left_join(projects %>% select(project_id, name), by = "project_id") %>%
    mutate(
      actual_hours = coalesce(actual_hours, 0),
      diff_hours = round(actual_hours - original_est_hours, 2),
      diff_pct = round((actual_hours - original_est_hours) / original_est_hours * 100, 0)
    ) %>%
    select(name, stage_name, stage_category, original_est_hours, actual_hours, diff_hours, diff_pct) %>%
    arrange(desc(abs(diff_pct)))
}

# Remaining hours per project: each stage's est_hours times the share of it
# still left (from stage_progress_fraction()). Missing est_hours falls back
# to the average hours per completed stage of that yarn weight, or 4 hours
# if no history.
project_remaining_hours <- function(projects, stages, sessions) {
  hist_avg <- stage_hours(sessions) %>%
    left_join(stages %>% select(stage_id, project_id, status), by = "stage_id") %>%
    filter(status == "done") %>%
    left_join(projects %>% select(project_id, yarn_weight), by = "project_id") %>%
    group_by(yarn_weight) %>%
    summarise(avg_hours_per_stage = mean(actual_hours, na.rm = TRUE), .groups = "drop")

  stage_progress_fraction(stages, sessions) %>%
    left_join(projects %>% select(project_id, yarn_weight), by = "project_id") %>%
    left_join(hist_avg, by = "yarn_weight") %>%
    mutate(
      fallback = coalesce(avg_hours_per_stage, 4),
      stage_remaining = (1 - frac) * coalesce(est_hours, fallback)
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

# ---- deadline pace -------------------------------------------------------

# Upcoming-deadline projects with their current remaining hours: the pool
# that the deadline-pace calculations schedule against. counts_from is the
# day a project joined your workload (its pattern-received date, or today if
# that's set in the future); NA means it has always counted.
upcoming_pool <- function(projects, stages, sessions) {
  upcoming <- upcoming_projects(projects)
  upcoming %>%
    left_join(project_remaining_hours(upcoming, stages, sessions), by = "project_id") %>%
    mutate(remaining_hours = coalesce(remaining_hours, 0), counts_from = pmin(pattern_received, Sys.Date())) %>%
    select(project_id, name, deadline, remaining_hours, counts_from)
}

# The pool as it stood on a past day: only projects that had joined your
# workload by then, each with today's remaining hours plus whatever was
# logged against it after that day. Stages added, estimates revised or
# deadlines changed since then aren't reflected.
pool_on <- function(pool, sessions, d) {
  pool %>%
    filter(is.na(counts_from) | counts_from <= d) %>%
    mutate(remaining_hours = remaining_hours + vapply(
      project_id,
      function(id) sum(sessions$hours[sessions$project_id == id & sessions$date > d]),
      numeric(1)
    ))
}

# For a pool of projects, as of a given date: the hours needed across
# everything due by each deadline, and the daily pace that requires.
# Projects sharing a deadline share the same total.
deadline_pace <- function(pool, as_of = Sys.Date()) {
  pool %>%
    arrange(deadline) %>%
    mutate(
      days_to_here = as.numeric(deadline - as_of),
      cum_hours_needed = vapply(deadline, function(d) sum(remaining_hours[deadline <= d]), numeric(1)),
      required_pace = ifelse(days_to_here > 0, cum_hours_needed / days_to_here, Inf)
    )
}

# Each upcoming deadline with the pace required from today, assuming hours
# always go to whichever project is due soonest.
critical_pace_by_deadline <- function(projects, stages, sessions) {
  deadline_pace(upcoming_pool(projects, stages, sessions))
}

# The single tightest deadline: the one needing the highest sustained daily
# pace (from today) to hit every deadline up to and including it.
critical_bottleneck <- function(projects, stages, sessions) {
  cp <- critical_pace_by_deadline(projects, stages, sessions)
  cp[which.max(cp$required_pace), ]
}

# The pace that would have been needed, on each day since the first logged
# session, to hit every upcoming deadline (using pool_on() for each day).
historical_required_pace <- function(projects, stages, sessions) {
  pool <- upcoming_pool(projects, stages, sessions)
  if (nrow(pool) == 0 || nrow(sessions) == 0) {
    return(tibble(date = as.Date(character()), required_pace = numeric()))
  }

  # A deadline falling on the day itself gives an infinite pace, which is
  # left out rather than letting it swamp that day's figure. NA (a gap in
  # the line) on days before any project had joined.
  pace_on <- function(d) {
    pace <- deadline_pace(pool_on(pool, sessions, d), as_of = d)$required_pace
    pace <- pace[is.finite(pace)]
    if (length(pace) == 0) NA_real_ else max(pace)
  }

  dates <- seq(min(sessions$date), Sys.Date(), by = "day")
  tibble(date = dates, required_pace = vapply(dates, pace_on, numeric(1)))
}

# ---- recommender ---------------------------------------------------------

# Ranks every active project's current stage by critical-path slack: the
# schedule buffer left once every commitment due at or before its deadline
# is accounted for (cum_hours_needed), at the assumed daily pace. The top
# pick can change when any project's remaining hours or deadline shifts,
# because all projects compete for the same hours.
recommend_projects <- function(projects, stages, sessions, daily_pace = 2) {
  cp <- critical_pace_by_deadline(projects, stages, sessions)
  own_remaining <- project_remaining_hours(projects, stages, sessions)

  # An active project whose deadline has passed isn't in cp, so it falls
  # back to its own remaining hours and still surfaces as urgent, rather
  # than sorting last with an NA slack.
  projects %>%
    filter(status == "active") %>%
    inner_join(current_stage(stages), by = "project_id") %>%
    left_join(cp %>% select(project_id, cum_hours_needed, required_pace), by = "project_id") %>%
    left_join(own_remaining, by = "project_id") %>%
    mutate(
      days_left = as.numeric(deadline - Sys.Date()),
      cum_hours_needed = coalesce(cum_hours_needed, remaining_hours),
      critical_slack_days = round(days_left - cum_hours_needed / daily_pace, 1),
      row_progress = ifelse(!is.na(total_rows), paste0("row ", coalesce(rows_done, 0), " of ", total_rows), NA)
    ) %>%
    arrange(critical_slack_days, days_left) %>%
    select(project_id, name, stage_name, row_progress, portability, focus, deadline, days_left, critical_slack_days, required_pace)
}

# ---- burndown ---------------------------------------------------------

# Total remaining hours across projects with an upcoming deadline (the same
# pool as the Capacity figure) on each day since the first logged session,
# using pool_on() for each day, so taking on a new project shows as a step
# up on the day it arrived. The ideal line burns each project's hours down
# from the day it joined to 0 at the furthest deadline, so it steps up by
# the same amount: being above it means behind on pace, not more work.
burndown_data <- function(projects, stages, sessions) {
  pool <- upcoming_pool(projects, stages, sessions)
  if (nrow(pool) == 0) {
    return(tibble(date = as.Date(character()), hours = numeric(), line = character()))
  }
  final_deadline <- max(pool$deadline)

  start_date <- if (nrow(sessions) == 0) Sys.Date() else min(sessions$date)
  actual <- tibble(date = seq(start_date, Sys.Date(), by = "day")) %>%
    mutate(
      hours = vapply(date, function(d) sum(pool_on(pool, sessions, d)$remaining_hours), numeric(1)),
      line = "Actual"
    )

  joined <- pmax(coalesce(pool$counts_from, start_date), start_date)
  hours_at_join <- vapply(seq_len(nrow(pool)), function(i) {
    on_join <- pool_on(pool, sessions, joined[i])
    on_join$remaining_hours[on_join$project_id == pool$project_id[i]]
  }, numeric(1))
  ideal_on <- function(d) {
    span <- as.numeric(final_deadline - joined)
    share_left <- ifelse(span > 0, pmax(0, 1 - as.numeric(d - joined) / span), 0)
    sum((hours_at_join * share_left)[joined <= d])
  }
  ideal <- tibble(date = seq(start_date, final_deadline, by = "day")) %>%
    mutate(hours = vapply(date, ideal_on, numeric(1)), line = "Ideal")

  bind_rows(actual, ideal)
}
