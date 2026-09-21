library(readr)
library(dplyr)

# APP_DATA_DIR is set by app.R before sourcing this file.
if (!exists("APP_DATA_DIR")) {
  APP_DATA_DIR <- "data"
}

# ---- column specs --------------------------------------------------------

projects_spec <- cols(
  project_id       = col_character(),
  name             = col_character(),
  designer         = col_character(),
  yarn_weight      = col_character(),
  needle_size      = col_character(),
  size             = col_character(),
  pattern_received = col_date(format = ""),
  start_date       = col_date(format = ""),
  deadline         = col_date(format = ""),
  status           = col_character(),
  ravelry_project  = col_logical(),
  colour           = col_character(),
  notes            = col_character()
)

stages_spec <- cols(
  stage_id    = col_character(),
  project_id  = col_character(),
  stage_name  = col_character(),
  stage_order = col_integer(),
  portability = col_integer(),
  focus       = col_integer(),
  est_hours   = col_double(),
  status      = col_character(),
  total_rows  = col_integer(),
  rows_done   = col_integer()
)

sessions_spec <- cols(
  session_id = col_character(),
  date       = col_date(format = ""),
  project_id = col_character(),
  stage_id   = col_character(),
  hours      = col_double(),
  location   = col_character(),
  energy     = col_character(),
  notes      = col_character()
)

# ---- read / write ---------------------------------------------------------

read_projects <- function() {
  read_csv(file.path(APP_DATA_DIR, "projects.csv"), col_types = projects_spec)
}

read_stages <- function() {
  read_csv(file.path(APP_DATA_DIR, "stages.csv"), col_types = stages_spec)
}

read_sessions <- function() {
  read_csv(file.path(APP_DATA_DIR, "sessions.csv"), col_types = sessions_spec)
}

# na = "" keeps missing values as blank cells rather than literal "NA" text,
# so the CSVs stay easy to read and hand-edit outside the app.
write_projects <- function(df) {
  write_csv(df, file.path(APP_DATA_DIR, "projects.csv"), na = "")
}

write_stages <- function(df) {
  write_csv(df, file.path(APP_DATA_DIR, "stages.csv"), na = "")
}

write_sessions <- function(df) {
  write_csv(df, file.path(APP_DATA_DIR, "sessions.csv"), na = "")
}

# ---- id generation ---------------------------------------------------------

# Generates the next sequential id for a given prefix, e.g. next_id(c("P1",
# "P2"), "P") returns "P3". Ignores any existing ids that don't match the
# prefix + integer pattern.
next_id <- function(existing_ids, prefix) {
  nums <- suppressWarnings(as.integer(gsub(paste0("^", prefix), "", existing_ids)))
  nums <- nums[!is.na(nums)]
  next_n <- if (length(nums) == 0) 1 else max(nums) + 1
  paste0(prefix, next_n)
}

# ---- mutations ---------------------------------------------------------

append_session <- function(date, project_id, stage_id, hours, location, energy, notes) {
  sessions <- read_sessions()
  new_id <- next_id(sessions$session_id, "L")
  new_row <- tibble(
    session_id = new_id,
    date = as.Date(date),
    project_id = project_id,
    stage_id = stage_id,
    hours = hours,
    location = location,
    energy = energy,
    notes = notes
  )
  write_sessions(bind_rows(sessions, new_row))
  invisible(new_id)
}

add_project <- function(name, designer, yarn_weight, needle_size, size,
                         pattern_received, start_date, deadline, notes,
                         colour = "#C9C2CE") {
  projects <- read_projects()
  new_id <- next_id(projects$project_id, "P")
  new_row <- tibble(
    project_id = new_id,
    name = name,
    designer = designer,
    yarn_weight = yarn_weight,
    needle_size = needle_size,
    size = size,
    pattern_received = as.Date(pattern_received),
    start_date = as.Date(start_date),
    deadline = as.Date(deadline),
    status = "active",
    ravelry_project = FALSE,
    colour = colour,
    notes = notes
  )
  write_projects(bind_rows(projects, new_row))
  invisible(new_id)
}

set_ravelry_flag <- function(project_id, value) {
  projects <- read_projects()
  projects$ravelry_project[projects$project_id == project_id] <- value
  write_projects(projects)
}

add_stage <- function(project_id, stage_name, stage_order, portability, focus,
                       est_hours, total_rows = NA) {
  stages <- read_stages()
  # stage_id is a sequential id per project, independent of stage_order, so
  # two stages accidentally given the same order never collide into the
  # same id (which would silently make every lookup by stage_id affect or
  # return both rows at once).
  existing_ids <- stages$stage_id[stages$project_id == project_id]
  new_id <- next_id(existing_ids, paste0(project_id, "-S"))
  new_row <- tibble(
    stage_id = new_id,
    project_id = project_id,
    stage_name = stage_name,
    stage_order = as.integer(stage_order),
    portability = as.integer(portability),
    focus = as.integer(focus),
    est_hours = as.numeric(est_hours),
    status = "not_started",
    total_rows = as.integer(total_rows),
    rows_done = if (is.na(total_rows)) NA_integer_ else 0L
  )
  write_stages(bind_rows(stages, new_row))
  invisible(new_id)
}

update_stage_status <- function(stage_id, new_status) {
  stages <- read_stages()
  stages$status[stages$stage_id == stage_id] <- new_status
  write_stages(stages)
}

update_stage_rows <- function(stage_id, rows_done) {
  stages <- read_stages()
  stages$rows_done[stages$stage_id == stage_id] <- as.integer(rows_done)
  write_stages(stages)
}

# Updates every editable field of an existing stage at once (used by the
# "Edit a stage" panel). Keeps rows_done consistent if total_rows is added
# or removed.
edit_stage <- function(stage_id, stage_name, portability, focus, est_hours,
                        total_rows, status) {
  stages <- read_stages()
  i <- which(stages$stage_id == stage_id)

  stages$stage_name[i] <- stage_name
  stages$portability[i] <- as.integer(portability)
  stages$focus[i] <- as.integer(focus)
  stages$est_hours[i] <- as.numeric(est_hours)
  stages$total_rows[i] <- as.integer(total_rows)
  stages$status[i] <- status

  if (is.na(stages$total_rows[i])) {
    stages$rows_done[i] <- NA_integer_
  } else if (is.na(stages$rows_done[i])) {
    stages$rows_done[i] <- 0L
  }

  write_stages(stages)
}
