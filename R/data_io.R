library(readr)
library(dplyr)
library(lubridate)

# APP_DATA_DIR is set by app.R before sourcing this file.
if (!exists("APP_DATA_DIR")) {
  APP_DATA_DIR <- "data"
}

# ---- column specs --------------------------------------------------------

# Date columns are read as text and parsed with parse_flex_date(), not
# col_date(), so both ISO and locale-reformatted (e.g. Excel) dates work.
projects_spec <- cols(
  project_id       = col_character(),
  name             = col_character(),
  designer         = col_character(),
  yarn_weight      = col_character(),
  needle_size      = col_character(),
  gauge            = col_character(),
  size             = col_character(),
  pattern_received = col_character(),
  start_date       = col_character(),
  deadline         = col_character(),
  status           = col_character(),
  ravelry_project  = col_logical(),
  colour           = col_character(),
  notes            = col_character()
)

stages_spec <- cols(
  stage_id       = col_character(),
  project_id     = col_character(),
  stage_name     = col_character(),
  stage_category = col_character(),
  stage_order    = col_integer(),
  portability    = col_integer(),
  focus          = col_integer(),
  est_hours      = col_double(),
  status         = col_character(),
  stage_deadline = col_character(),
  total_rows     = col_integer(),
  rows_done      = col_integer()
)

sessions_spec <- cols(
  session_id = col_character(),
  date       = col_character(),
  project_id = col_character(),
  stage_id   = col_character(),
  hours      = col_double(),
  location   = col_character(),
  energy     = col_character(),
  notes      = col_character()
)

# Parses ISO, UK (dmy) or US (mdy) date text into a Date, or NA.
parse_flex_date <- function(x) {
  as.Date(parse_date_time(x, orders = c("ymd", "dmy", "mdy"), quiet = TRUE))
}

# ---- read / write ---------------------------------------------------------

read_projects <- function() {
  read_csv(file.path(APP_DATA_DIR, "projects.csv"), col_types = projects_spec) %>%
    mutate(across(c(pattern_received, start_date, deadline), parse_flex_date))
}

read_stages <- function() {
  read_csv(file.path(APP_DATA_DIR, "stages.csv"), col_types = stages_spec) %>%
    mutate(stage_deadline = parse_flex_date(stage_deadline))
}

read_sessions <- function() {
  read_csv(file.path(APP_DATA_DIR, "sessions.csv"), col_types = sessions_spec) %>%
    mutate(date = parse_flex_date(date))
}

# na = "" writes missing values as blank cells, not literal "NA" text.
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

# Next sequential id for a prefix, e.g. next_id(c("P1", "P2"), "P") -> "P3".
next_id <- function(existing_ids, prefix) {
  nums <- suppressWarnings(as.integer(gsub(paste0("^", prefix), "", existing_ids)))
  nums <- nums[!is.na(nums)]
  next_n <- if (length(nums) == 0) 1 else max(nums) + 1
  paste0(prefix, next_n)
}

# A NULL or zero-length value (e.g. a cleared date input) is replaced with
# na instead of being assigned as-is, which would error on a single row.
or_na <- function(x, na = NA) {
  if (is.null(x) || length(x) == 0) na else x
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

add_project <- function(name, designer, yarn_weight, needle_size, gauge, size,
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
    gauge = gauge,
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

# Overwrites every editable field of an existing project in place.
edit_project <- function(project_id, name, designer, yarn_weight, needle_size,
                          gauge, size, pattern_received, start_date, deadline,
                          status, ravelry_project, colour, notes) {
  projects <- read_projects()
  i <- which(projects$project_id == project_id)

  projects$name[i] <- or_na(name)
  projects$designer[i] <- or_na(designer)
  projects$yarn_weight[i] <- or_na(yarn_weight)
  projects$needle_size[i] <- or_na(needle_size)
  projects$gauge[i] <- or_na(gauge)
  projects$size[i] <- or_na(size)
  projects$pattern_received[i] <- as.Date(or_na(pattern_received, as.Date(NA)))
  projects$start_date[i] <- as.Date(or_na(start_date, as.Date(NA)))
  projects$deadline[i] <- as.Date(or_na(deadline, as.Date(NA)))
  projects$status[i] <- or_na(status)
  projects$ravelry_project[i] <- or_na(ravelry_project, FALSE)
  projects$colour[i] <- or_na(colour)
  projects$notes[i] <- or_na(notes)

  write_projects(projects)
}

# stage_id is a sequential id per project, independent of stage_order, so
# two stages sharing an order never collide into the same id.
add_stage <- function(project_id, stage_name, stage_category, stage_order, portability, focus,
                       est_hours, total_rows = NA, stage_deadline = NA) {
  stages <- read_stages()
  existing_ids <- stages$stage_id[stages$project_id == project_id]
  new_id <- next_id(existing_ids, paste0(project_id, "-S"))
  new_row <- tibble(
    stage_id = new_id,
    project_id = project_id,
    stage_name = stage_name,
    stage_category = stage_category,
    stage_order = as.integer(stage_order),
    portability = as.integer(portability),
    focus = as.integer(focus),
    est_hours = as.numeric(est_hours),
    status = "not_started",
    stage_deadline = as.Date(or_na(stage_deadline, as.Date(NA))),
    total_rows = as.integer(total_rows),
    rows_done = if (is.na(total_rows)) NA_integer_ else 0L
  )
  write_stages(bind_rows(stages, new_row))
  invisible(new_id)
}

# Replaces a stage's est_hours with its real total logged hours, once it's
# marked "done". Left untouched if nothing's been logged for it yet.
sync_est_hours_from_sessions <- function(stages, stage_id) {
  sessions <- read_sessions()
  actual_hours <- sum(sessions$hours[sessions$stage_id == stage_id], na.rm = TRUE)
  if (actual_hours > 0) {
    stages$est_hours[stages$stage_id == stage_id] <- actual_hours
  }
  stages
}

update_stage_status <- function(stage_id, new_status) {
  stages <- read_stages()
  stages$status[stages$stage_id == stage_id] <- new_status
  if (new_status == "done") {
    stages <- sync_est_hours_from_sessions(stages, stage_id)
  }
  write_stages(stages)
}

update_stage_rows <- function(stage_id, rows_done) {
  stages <- read_stages()
  stages$rows_done[stages$stage_id == stage_id] <- as.integer(rows_done)
  write_stages(stages)
}

# Overwrites every editable field of an existing stage in place. Keeps
# rows_done consistent if total_rows is added or removed.
edit_stage <- function(stage_id, stage_name, stage_category, portability, focus, est_hours,
                        total_rows, status, stage_deadline = NA) {
  stages <- read_stages()
  i <- which(stages$stage_id == stage_id)

  stages$stage_name[i] <- stage_name
  stages$stage_category[i] <- stage_category
  stages$portability[i] <- as.integer(portability)
  stages$focus[i] <- as.integer(focus)
  stages$est_hours[i] <- as.numeric(est_hours)
  stages$total_rows[i] <- as.integer(total_rows)
  stages$status[i] <- status
  stages$stage_deadline[i] <- as.Date(or_na(stage_deadline, as.Date(NA)))

  if (is.na(stages$total_rows[i])) {
    stages$rows_done[i] <- NA_integer_
  } else if (is.na(stages$rows_done[i])) {
    stages$rows_done[i] <- 0L
  }

  if (status == "done") {
    stages <- sync_est_hours_from_sessions(stages, stage_id)
  }

  write_stages(stages)
}
