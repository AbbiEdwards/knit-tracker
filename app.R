library(shiny)
library(bslib)
library(DT)
library(plotly)
library(dplyr)
library(lubridate)

APP_DATA_DIR <- "data"
source("R/data_io.R")
source("R/calculations.R")

YARN_WEIGHTS <- c("lace", "fingering", "sport", "dk", "worsted", "aran", "bulky")

# Suggested stage categories for Analytics grouping. Free text is also
# accepted (selectizeInput create = TRUE).
STAGE_CATEGORIES <- c(
  "Swatch", "Back", "Front", "Right Front", "Left Front", "Body", "Sleeves",
  "Cuffs", "Shoulders", "Yoke", "Colourwork Yoke", "Neckband", "Back Neck",
  "Collar", "Hem", "Button band", "Embroidery", "Finishing", "Other"
)
LOCATIONS <- names(location_min_portability)
ENERGIES <- names(energy_max_focus)

PORTABILITY_LABEL <- "Portability (1 = needs full kit, 3 = grab-and-go)"
FOCUS_LABEL <- "Focus required (1 = mindless, 3 = full concentration)"

# Overdue, then darkest to lightest as days_left increases, then safe.
URGENCY_COLOURS <- c(overdue = "#7A1F1F", red = "#C25C5C", amber = "#E8B54A", safe = "#8FAE8B")

# Maps days_left to the urgency colour scale. safe_colour is returned once
# nothing is urgent (8+ days, or NA).
urgency_colour <- function(days_left, safe_colour = URGENCY_COLOURS[["safe"]]) {
  if (is.na(days_left)) {
    return(safe_colour)
  }
  if (days_left < 0) {
    URGENCY_COLOURS[["overdue"]]
  } else if (days_left <= 3) {
    URGENCY_COLOURS[["red"]]
  } else if (days_left <= 7) {
    URGENCY_COLOURS[["amber"]]
  } else {
    safe_colour
  }
}

# Converts a hex colour to a translucent rgba() string.
hex_to_rgba <- function(hex, alpha = 0.35) {
  hex <- gsub("#", "", hex)
  r <- strtoi(substr(hex, 1, 2), 16)
  g <- strtoi(substr(hex, 3, 4), 16)
  b <- strtoi(substr(hex, 5, 6), 16)
  sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha)
}

# Keeps projects whose deadline hasn't passed yet (or has none set).
not_past_deadline <- function(projects) {
  projects %>% filter(is.na(deadline) | as.Date(deadline) >= Sys.Date())
}

# Adds a stage/project's currently-saved value to a fixed choice list, so a
# previously typed custom value (via selectize create = TRUE) still shows
# up as selected instead of appearing blank.
choices_with_current <- function(base_choices, current_value) {
  if (is.na(current_value)) base_choices else union(base_choices, current_value)
}

# Sets a dateInput's value, clearing it properly when date_value is NA.
# updateDateInput(value = NULL) is silently ignored rather than clearing
# the field, so a blank date is sent via the lower-level input message.
set_date_input <- function(session, input_id, date_value) {
  if (is.na(date_value)) {
    session$sendInputMessage(input_id, list(value = ""))
  } else {
    updateDateInput(session, input_id, value = date_value)
  }
}

# Notification suffix reporting a stage's est_hours after it's marked done.
done_hours_note <- function(stage_id) {
  hours <- read_stages() %>% filter(stage_id == !!stage_id) %>% pull(est_hours)
  paste0(" — marked done, est. hours updated to ", hours, "h")
}

# A small, low-opacity 5-petal floral sprig used as a decorative accent.
FLORAL_SVG <- paste0(
  "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='120' ",
  "height='120' viewBox='0 0 120 120'%3E%3Cg fill='%23D98BA0' fill-opacity=",
  "'0.35'%3E%3Ccircle cx='60' cy='40' r='14'/%3E%3Ccircle cx='40' cy='55' ",
  "r='14'/%3E%3Ccircle cx='80' cy='55' r='14'/%3E%3Ccircle cx='50' cy='75' ",
  "r='14'/%3E%3Ccircle cx='70' cy='75' r='14'/%3E%3C/g%3E%3Ccircle cx='60' ",
  "cy='60' r='9' fill='%23E8B54A' fill-opacity='0.4'/%3E%3C/svg%3E"
)

app_theme <- bs_theme(
  bg = "#FFF8F3",
  fg = "#4A3540",
  primary = "#C9789A",
  secondary = "#EBC9CE",
  success = URGENCY_COLOURS[["safe"]],
  warning = URGENCY_COLOURS[["amber"]],
  danger = URGENCY_COLOURS[["red"]],
  base_font = "'Segoe UI', 'Helvetica Neue', Arial, sans-serif",
  "card-border-radius" = "0.9rem",
  "border-radius" = "0.6rem"
)

app_css <- tags$style(HTML(sprintf("
  .align-with-input { margin-top: 1.9rem; }
  .navbar {
    background-image: url('%s');
    background-repeat: no-repeat;
    background-position: right 4px top 2px;
    background-size: 64px 64px;
  }
  .card { box-shadow: 0 2px 10px rgba(74, 53, 64, 0.06); border: 1px solid #F1DDE2; }
  .card-header { background-color: #FBEFF1; font-weight: 600; border-bottom: 1px solid #F1DDE2; }
  /* Opts specific dropdown-holding cards out of bslib's default corner
     clipping, which otherwise cuts off the flyout menu. */
  .dropdown-card { overflow: visible !important; }
  body {
    background-image: url('%s');
    background-repeat: no-repeat;
    background-position: bottom -20px right -20px;
    background-size: 260px 260px;
    background-attachment: fixed;
  }
", FLORAL_SVG, FLORAL_SVG)))

# ---- UI ---------------------------------------------------------------

ui <- page_navbar(
  title = "Knit Tracker",
  theme = app_theme,
  header = app_css,
  fillable = FALSE,

  nav_panel(
    "Overview",
    card(
      card_header("Progress & deadlines"),
      uiOutput("progress_bars")
    ),
    card(
      card_header("Upcoming stage mini-deadlines"),
      p(style = "font-size:12px; color:#8A6E78;", "Any not-yet-done stage due within the next 30 days (or overdue), across all active projects, soonest first. Project name coloured by its yarn colour; days left coloured by urgency."),
      DTOutput("upcoming_stage_deadlines")
    )
  ),

  nav_panel(
    "Project details",
    card(
      class = "dropdown-card",
      card_header("Choose a project to view or edit"),
      selectizeInput("detail_project", "Project", choices = NULL, options = list(dropdownParent = "body"))
    ),
    card(
      card_header("Details"),
      p("Editing here updates this project in place - it will not create a duplicate."),
      uiOutput("detail_colour_swatch"),
      strong("Progress by stage"),
      p(style = "font-size:12px; color:#8A6E78; margin-bottom:4px;", "One numbered segment per stage, coloured in by how complete it is - see the key below the bar for what each number is."),
      uiOutput("detail_stage_bar"),
      layout_columns(
        col_widths = c(6, 6),
        textInput("detail_name", "Name"),
        textInput("detail_designer", "Designer")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectizeInput("detail_weight", "Yarn weight", choices = YARN_WEIGHTS, options = list(create = TRUE)),
        textInput("detail_needle", "Needle size"),
        textInput("detail_gauge", "Gauge (e.g. '22 sts x 30 rows = 4in')")
      ),
      layout_columns(
        col_widths = c(6, 6),
        textInput("detail_size", "Size"),
        textInput("detail_colour", "Yarn colour (hex)")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        dateInput("detail_pattern_received", "Pattern received date"),
        dateInput("detail_start_date", "Start date"),
        dateInput("detail_deadline", "Deadline")
      ),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("detail_status", "Status", choices = c("active", "pending", "finished")),
        checkboxInput("detail_ravelry", "Ravelry project page made?")
      ),
      textAreaInput("detail_notes", "Notes"),
      actionButton("detail_submit", "Save changes", class = "btn-primary")
    ),
    card(
      card_header("Add a new project"),
      textInput("np_name", "Name"),
      textInput("np_designer", "Designer"),
      selectizeInput("np_weight", "Yarn weight", choices = YARN_WEIGHTS, options = list(create = TRUE, placeholder = "pick or type e.g. 'sport (held: fingering + lace)'")),
      textInput("np_needle", "Needle size (actual, not just recommended)"),
      textInput("np_gauge", "Gauge (e.g. '22 sts x 30 rows = 4in')"),
      textInput("np_size", "Size you're making"),
      textInput("np_colour", "Yarn colour, as a hex code (used for its progress bar)", value = "#C9789A", placeholder = "e.g. #2F4F3A for bottle green"),
      dateInput("np_pattern_received", "Pattern received date", value = NULL),
      dateInput("np_start_date", "Start date (leave as today if not cast on yet)", value = NULL),
      dateInput("np_deadline", "Deadline"),
      textAreaInput("np_notes", "Notes"),
      actionButton("np_submit", "Add project", class = "btn-primary")
    )
  ),

  nav_panel(
    "Gantt",
    card(
      card_header("Remaining time needed vs. deadline"),
      p(paste(
        "Bars show the window from today to your estimated finish date if",
        "you started that project exclusively right now, at your chosen",
        "pace. The diamond marks the real deadline. Remaining hours are",
        "currently rough guesses (a flat 4 hrs/stage) until you log real",
        "session hours or fill in est_hours per stage."
      )),
      numericInput("gantt_hours_per_day", "Assumed knitting pace (hours/day)", value = 2, min = 0.25, step = 0.25),
      plotlyOutput("gantt_plot", height = "450px")
    )
  ),

  nav_panel(
    "What to knit now",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Right now I am..."),
        selectInput("rec_location", "Where I'm knitting:", choices = LOCATIONS),
        selectInput("rec_energy", "Energy / focus available:", choices = ENERGIES),
        selectInput("rec_time", "Time I have:", choices = TIME_CHOICES),
        numericInput("rec_hours_per_day", "Assumed knitting pace (hours/day, for ranking urgency)", value = 2, min = 0.25, step = 0.25)
      ),
      card(
        card_header("Suggested projects (most time-pressured first)"),
        p("Ranked by slack: deadline minus the time still needed at your pace - not just whichever deadline is soonest."),
        DTOutput("recommend_table")
      )
    )
  ),

  nav_panel(
    "Log a session",
    card(
      card_header("Log time"),
      layout_columns(
        col_widths = c(4, 4, 4),
        dateInput("log_date", "Date", value = NULL),
        selectInput("log_project", "Project", choices = NULL),
        selectInput("log_stage", "Stage", choices = NULL)
      ),
      uiOutput("log_row_progress_ui"),
      layout_columns(
        col_widths = c(4, 4, 4),
        numericInput("log_rows_done", "Chart/row-tracked stage: now at row # (optional)", value = NA, min = 0, step = 1),
        numericInput("log_hours", "Hours", value = 1, min = 0, step = 0.25),
        selectInput("log_location", "Where", choices = LOCATIONS)
      ),
      textAreaInput("log_notes", "Notes (optional)", placeholder = "e.g. frogged the raglan increases twice, gauge issue"),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("log_energy", "Energy", choices = ENERGIES),
        selectInput("log_stage_status", "Update this stage's status to:", choices = c("(leave unchanged)", "in_progress", "done")),
        div(class = "align-with-input", actionButton("log_submit", "Log session", class = "btn-primary"))
      )
    ),
    card(
      card_header("Recent sessions"),
      DTOutput("recent_sessions")
    )
  ),

  nav_panel(
    "Manage stages",
    card(
      card_header("Add a stage to a project"),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("ns_project", "Project", choices = NULL),
        textInput("ns_name", "Stage name")
      ),
      selectizeInput("ns_category", "Stage category (for Analytics grouping)", choices = STAGE_CATEGORIES, options = list(create = TRUE, placeholder = "pick or type a new category")),
      layout_columns(
        col_widths = c(6, 6),
        sliderInput("ns_portability", PORTABILITY_LABEL, min = 1, max = 3, value = 2),
        sliderInput("ns_focus", FOCUS_LABEL, min = 1, max = 3, value = 2)
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        numericInput("ns_order", "Order", value = 1, min = 1, step = 1),
        numericInput("ns_est_hours", "Estimated hours (optional)", value = NA, min = 0, step = 0.5),
        numericInput("ns_total_rows", "Total rows in chart (optional)", value = NA, min = 1, step = 1),
        dateInput("ns_stage_deadline", "Mini-deadline (optional)", value = NA)
      ),
      div(class = "align-with-input", actionButton("ns_submit", "Add stage", class = "btn-primary"))
    ),
    card(
      height = "480px",
      card_header("All stages"),
      DTOutput("stages_table")
    ),
    card(
      height = "560px",
      card_header("Edit a stage"),
      p("Pick a stage to load its current values, adjust anything, then save."),
      selectInput("edit_stage", "Stage", choices = NULL),
      layout_columns(
        col_widths = c(6, 6),
        textInput("edit_name", "Stage name"),
        selectInput("edit_status", "Status", choices = c("not_started", "in_progress", "done"))
      ),
      selectizeInput("edit_category", "Stage category (for Analytics grouping)", choices = STAGE_CATEGORIES, options = list(create = TRUE)),
      layout_columns(
        col_widths = c(6, 6),
        sliderInput("edit_portability", PORTABILITY_LABEL, min = 1, max = 3, value = 2),
        sliderInput("edit_focus", FOCUS_LABEL, min = 1, max = 3, value = 2)
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        numericInput("edit_est_hours", "Estimated hours", value = NA, min = 0, step = 0.5),
        numericInput("edit_total_rows", "Total rows in chart", value = NA, min = 1, step = 1),
        dateInput("edit_stage_deadline", "Mini-deadline (optional)", value = NA)
      ),
      div(class = "align-with-input", actionButton("edit_submit", "Save changes", class = "btn-primary"))
    )
  ),

  nav_panel(
    "Analytics",
    layout_columns(
      col_widths = 12,
      card(
        card_header("Total hours logged per project"),
        plotlyOutput("hours_per_project_plot", height = "350px")
      ),
      card(
        card_header("Hours per logged session, by stage category and yarn weight (from your history)"),
        DTOutput("hours_per_stage_table")
      )
    )
  )
)

# ---- server -------------------------------------------------------------

server <- function(input, output, session) {

  # ---- shared setup ----

  # Refreshes today's date per session (a UI-level default is set once at
  # app start and goes stale over a long-running session).
  updateDateInput(session, "log_date", value = Sys.Date())
  updateDateInput(session, "np_pattern_received", value = Sys.Date())
  updateDateInput(session, "np_start_date", value = Sys.Date())

  refresh <- reactiveVal(0)
  bump <- function() refresh(refresh() + 1)

  projects_r <- reactive({ refresh(); read_projects() })
  stages_r   <- reactive({ refresh(); read_stages() })
  sessions_r <- reactive({ refresh(); read_sessions() })

  # Keeps project dropdowns in sync with the data, preserving the current
  # selection (a refresh would otherwise reset each to its first option).
  observe({
    p <- projects_r()
    choices <- setNames(p$project_id, p$name)
    not_finished <- not_past_deadline(p)
    not_finished_choices <- setNames(not_finished$project_id, not_finished$name)

    updateSelectInput(session, "log_project", choices = choices, selected = input$log_project)
    updateSelectInput(session, "ns_project", choices = not_finished_choices, selected = input$ns_project)
    updateSelectInput(session, "detail_project", choices = choices, selected = input$detail_project)
  })

  # ---- Overview ----

  output$progress_bars <- renderUI({
    not_finished <- not_past_deadline(projects_r())
    prog <- project_progress(not_finished, stages_r()) %>%
      mutate(days_left = as.numeric(as.Date(deadline) - Sys.Date()))

    bars <- lapply(seq_len(nrow(prog)), function(i) {
      row <- prog[i, ]
      pct <- round(row$pct_complete * 100)
      bar_colour <- if (!is.na(row$colour) && nzchar(row$colour)) row$colour else "#C9789A"
      badge_colour <- urgency_colour(row$days_left)
      tagList(
        div(
          style = "margin-bottom: 6px;",
          strong(row$name),
          span(
            style = paste0(
              "float:right; background:", badge_colour, "; color:white;",
              "border-radius:999px; padding:2px 10px; font-size:12px;"
            ),
            paste0(row$days_left, " days left (", format(row$deadline, "%d %b %Y"), ")")
          )
        ),
        div(
          style = "background:#F1DDE2; border-radius:999px; height:22px; margin-bottom:18px; overflow:hidden;",
          div(
            style = paste0(
              "background:", bar_colour, "; width:", pct, "%; height:100%; color:white;",
              "text-align:right; padding-right:8px; font-size:12px; line-height:22px; border-radius:999px;"
            ),
            paste0(pct, "%")
          )
        )
      )
    })
    tagList(bars)
  })

  # Includes pending projects; excludes only those past their deadline.
  output$upcoming_stage_deadlines <- renderDT({
    not_finished <- not_past_deadline(projects_r()) %>% select(project_id, name, colour)
    upcoming <- stages_r() %>%
      filter(status != "done", !is.na(stage_deadline)) %>%
      inner_join(not_finished, by = "project_id") %>%
      mutate(days_left = as.numeric(stage_deadline - Sys.Date())) %>%
      filter(days_left <= 30) %>%
      arrange(stage_deadline) %>%
      select(name, stage_name, status, stage_deadline, days_left, colour)

    display <- upcoming %>% select(-colour)
    dt <- datatable(display, options = list(dom = "t", pageLength = 15), rownames = FALSE)

    if (nrow(upcoming) == 0) {
      return(dt)
    }

    project_colours <- upcoming %>% distinct(name, colour)
    dt %>%
      formatStyle(
        "name",
        backgroundColor = styleEqual(project_colours$name, sapply(project_colours$colour, hex_to_rgba))
      ) %>%
      formatStyle(
        "days_left",
        backgroundColor = styleInterval(c(0, 4, 8), unname(URGENCY_COLOURS)),
        color = "white"
      )
  })

  # ---- Project details ----

  # Loads the selected project's current values into the edit form.
  observeEvent(input$detail_project, {
    req(input$detail_project)
    proj <- projects_r() %>% filter(project_id == input$detail_project)
    if (nrow(proj) == 0) {
      return(NULL)
    }
    proj <- proj[1, ]

    updateTextInput(session, "detail_name", value = proj$name)
    updateTextInput(session, "detail_designer", value = proj$designer)
    updateSelectizeInput(
      session, "detail_weight",
      choices = choices_with_current(YARN_WEIGHTS, proj$yarn_weight), selected = proj$yarn_weight
    )
    updateTextInput(session, "detail_needle", value = proj$needle_size)
    updateTextInput(session, "detail_gauge", value = proj$gauge)
    updateTextInput(session, "detail_size", value = proj$size)
    updateTextInput(session, "detail_colour", value = proj$colour)
    set_date_input(session, "detail_pattern_received", proj$pattern_received)
    set_date_input(session, "detail_start_date", proj$start_date)
    set_date_input(session, "detail_deadline", proj$deadline)
    updateSelectInput(session, "detail_status", selected = proj$status)
    updateCheckboxInput(session, "detail_ravelry", value = isTRUE(proj$ravelry_project))
    updateTextAreaInput(session, "detail_notes", value = proj$notes)
  })

  output$detail_colour_swatch <- renderUI({
    req(input$detail_colour)
    div(style = paste0(
      "width:28px; height:28px; border-radius:50%; margin-bottom:12px;",
      "background:", input$detail_colour, "; border:1px solid #F1DDE2;"
    ))
  })

  # A numbered segment per stage, coloured by completion. Stage names are
  # listed in the legend below rather than as hover tooltips.
  output$detail_stage_bar <- renderUI({
    req(input$detail_project)
    st <- stages_r() %>% filter(project_id == input$detail_project) %>% arrange(stage_order)
    if (nrow(st) == 0) {
      return(p(em("No stages added yet for this project.")))
    }
    bar_colour <- if (!is.null(input$detail_colour) && nzchar(input$detail_colour)) input$detail_colour else "#C9789A"

    segments <- lapply(seq_len(nrow(st)), function(i) {
      row <- st[i, ]
      frac <- case_when(
        row$status == "done" ~ 1,
        row$status == "in_progress" & !is.na(row$total_rows) & row$total_rows > 0 ~
          coalesce(row$rows_done, 0) / row$total_rows,
        row$status == "in_progress" ~ 0.5,
        TRUE ~ 0
      )
      pct <- round(frac * 100)
      # A mini-deadline's urgency only shows while the stage isn't done.
      border_colour <- "#F1DDE2"
      if (row$status != "done" && !is.na(row$stage_deadline)) {
        days_left <- as.numeric(row$stage_deadline - Sys.Date())
        border_colour <- urgency_colour(days_left, safe_colour = "#F1DDE2")
      }
      div(
        style = paste0(
          "position:relative; flex:1; height:22px; margin:0 2px; border-radius:6px; overflow:hidden;",
          "background:#F1DDE2; border:2px solid ", border_colour, ";"
        ),
        div(style = paste0("background:", bar_colour, "; width:", pct, "%; height:100%;")),
        div(
          style = "position:absolute; inset:0; display:flex; align-items:center; justify-content:center; font-size:11px; font-weight:600; color:#4A3540; text-shadow:0 0 3px white, 0 0 3px white;",
          row$stage_order
        )
      )
    })

    deadline_suffix <- ifelse(is.na(st$stage_deadline), "", paste0(" (due ", format(st$stage_deadline, "%d %b"), ")"))
    legend <- paste0(
      st$stage_order, ". ", st$stage_name, " [", st$status, "]", deadline_suffix,
      collapse = "  •  "
    )

    tagList(
      div(style = "display:flex; margin-bottom:6px;", segments),
      p(style = "font-size:12px; color:#8A6E78;", legend)
    )
  })

  observeEvent(input$detail_submit, {
    req(input$detail_project)
    edit_project(
      input$detail_project, input$detail_name, input$detail_designer, input$detail_weight,
      input$detail_needle, input$detail_gauge, input$detail_size, input$detail_pattern_received,
      input$detail_start_date, input$detail_deadline, input$detail_status, input$detail_ravelry,
      input$detail_colour, input$detail_notes
    )
    showNotification(paste0("Saved changes to ", input$detail_name), type = "message", duration = 4)
    bump()
  })

  observeEvent(input$np_submit, {
    req(input$np_name)
    add_project(
      input$np_name, input$np_designer, input$np_weight, input$np_needle, input$np_gauge,
      input$np_size, input$np_pattern_received, input$np_start_date, input$np_deadline,
      input$np_notes, input$np_colour
    )
    showNotification(paste0("Added new project: ", input$np_name), type = "message", duration = 4)
    updateTextInput(session, "np_name", value = "")
    updateTextInput(session, "np_designer", value = "")
    updateTextInput(session, "np_size", value = "")
    updateTextAreaInput(session, "np_notes", value = "")
    bump()
  })

  # ---- Gantt ----

  output$gantt_plot <- renderPlotly({
    gd <- gantt_data(projects_r(), stages_r(), sessions_r(), hours_per_week = input$gantt_hours_per_day * 7)
    if (nrow(gd) == 0) {
      return(plotly_empty())
    }

    p <- plot_ly()
    for (i in seq_len(nrow(gd))) {
      row <- gd[i, ]
      p <- p %>% add_segments(
        x = row$start, xend = row$finish_if_started_now,
        y = row$name, yend = row$name,
        line = list(width = 14, color = "#4682B4"),
        showlegend = FALSE, name = row$name
      )
      p <- p %>% add_markers(
        x = row$deadline, y = row$name,
        marker = list(symbol = "diamond", size = 14, color = "#d9534f"),
        showlegend = FALSE, name = "deadline"
      )
    }
    p %>% layout(xaxis = list(title = "Date"), yaxis = list(title = ""))
  })

  # ---- What to knit now ----

  output$recommend_table <- renderDT({
    rec <- recommend_projects(
      projects_r(), stages_r(), sessions_r(), input$rec_location, input$rec_energy,
      input$rec_time, hours_per_week = input$rec_hours_per_day * 7
    )
    datatable(rec, options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })

  # ---- Log a session ----

  observeEvent(input$log_project, {
    st <- stages_r() %>% filter(project_id == input$log_project) %>% arrange(stage_order)
    choices <- if (nrow(st) == 0) {
      character(0)
    } else {
      setNames(st$stage_id, paste0(st$stage_order, ". ", st$stage_name, " [", st$status, "]"))
    }
    updateSelectInput(session, "log_stage", choices = choices)
  }, ignoreNULL = FALSE)

  output$log_row_progress_ui <- renderUI({
    req(input$log_stage)
    st <- stages_r() %>% filter(stage_id == input$log_stage)
    if (nrow(st) == 0 || is.na(st$total_rows[1])) {
      return(NULL)
    }
    p(strong(paste0("Currently at row ", coalesce(st$rows_done[1], 0), " of ", st$total_rows[1], ".")))
  })

  # Caps "now at row #" at the stage's total_rows.
  observeEvent(input$log_stage, {
    st <- stages_r() %>% filter(stage_id == input$log_stage)
    max_rows <- if (nrow(st) == 0) NA else st$total_rows[1]
    updateNumericInput(session, "log_rows_done", max = max_rows)
  })

  observeEvent(input$log_submit, {
    req(input$log_project, input$log_stage)
    append_session(
      date = input$log_date, project_id = input$log_project, stage_id = input$log_stage,
      hours = input$log_hours, location = input$log_location, energy = input$log_energy,
      notes = input$log_notes
    )
    if (input$log_stage_status != "(leave unchanged)") {
      update_stage_status(input$log_stage, input$log_stage_status)
    }
    if (!is.na(input$log_rows_done)) {
      update_stage_rows(input$log_stage, input$log_rows_done)
    }
    proj_name <- projects_r() %>% filter(project_id == input$log_project) %>% pull(name)
    stage_name <- stages_r() %>% filter(stage_id == input$log_stage) %>% pull(stage_name)
    note <- if (input$log_stage_status == "done") done_hours_note(input$log_stage) else ""
    showNotification(paste0("Logged ", input$log_hours, "h to ", proj_name, " — ", stage_name, note), type = "message", duration = 5)

    updateNumericInput(session, "log_hours", value = 1)
    updateNumericInput(session, "log_rows_done", value = NA)
    updateTextAreaInput(session, "log_notes", value = "")
    bump()
  })

  output$recent_sessions <- renderDT({
    s <- sessions_r() %>%
      left_join(projects_r() %>% select(project_id, name), by = "project_id") %>%
      arrange(desc(date)) %>%
      select(date, name, stage_id, hours, location, energy, notes)
    datatable(s, options = list(pageLength = 8), rownames = FALSE)
  })

  # ---- Manage stages ----

  observeEvent(input$ns_submit, {
    req(input$ns_project, input$ns_name)
    add_stage(
      input$ns_project, input$ns_name, input$ns_category, input$ns_order, input$ns_portability,
      input$ns_focus, input$ns_est_hours, input$ns_total_rows, input$ns_stage_deadline
    )
    proj_name <- projects_r() %>% filter(project_id == input$ns_project) %>% pull(name)
    showNotification(paste0("Added stage \"", input$ns_name, "\" to ", proj_name), type = "message", duration = 4)
    updateTextInput(session, "ns_name", value = "")
    updateDateInput(session, "ns_stage_deadline", value = NA)
    bump()
  })

  output$stages_table <- renderDT({
    not_finished <- not_past_deadline(projects_r()) %>% select(project_id, name)
    st <- stages_r() %>%
      inner_join(not_finished, by = "project_id") %>%
      arrange(name, stage_order) %>%
      select(name, stage_order, stage_name, stage_category, portability, focus, est_hours, status, stage_deadline, total_rows, rows_done)
    datatable(st, options = list(pageLength = 10), rownames = FALSE)
  })

  observe({
    p <- not_past_deadline(projects_r())
    st <- stages_r() %>% filter(project_id %in% p$project_id) %>% arrange(project_id, stage_order)
    if (nrow(st) == 0) {
      updateSelectInput(session, "edit_stage", choices = character(0))
    } else {
      labels <- paste0(p$name[match(st$project_id, p$project_id)], " — ", st$stage_order, ". ", st$stage_name, " [", st$status, "]")
      choices <- setNames(st$stage_id, labels)
      updateSelectInput(session, "edit_stage", choices = choices, selected = input$edit_stage)
    }
  })

  # Loads the selected stage's current values into the edit form.
  observeEvent(input$edit_stage, {
    req(input$edit_stage)
    st <- stages_r() %>% filter(stage_id == input$edit_stage)
    if (nrow(st) == 0) {
      return(NULL)
    }
    updateTextInput(session, "edit_name", value = st$stage_name[1])
    updateSelectizeInput(
      session, "edit_category",
      choices = choices_with_current(STAGE_CATEGORIES, st$stage_category[1]), selected = st$stage_category[1]
    )
    updateSelectInput(session, "edit_status", selected = st$status[1])
    updateSliderInput(session, "edit_portability", value = st$portability[1])
    updateSliderInput(session, "edit_focus", value = st$focus[1])
    updateNumericInput(session, "edit_est_hours", value = st$est_hours[1])
    updateNumericInput(session, "edit_total_rows", value = st$total_rows[1])
    set_date_input(session, "edit_stage_deadline", st$stage_deadline[1])
  })

  observeEvent(input$edit_submit, {
    req(input$edit_stage)
    edit_stage(
      input$edit_stage, input$edit_name, input$edit_category, input$edit_portability, input$edit_focus,
      input$edit_est_hours, input$edit_total_rows, input$edit_status, input$edit_stage_deadline
    )
    note <- if (input$edit_status == "done") done_hours_note(input$edit_stage) else ""
    showNotification(paste0("Saved changes to stage: ", input$edit_name, note), type = "message", duration = 5)
    bump()
  })

  # ---- Analytics ----

  output$hours_per_project_plot <- renderPlotly({
    hp <- total_hours_per_project(sessions_r(), projects_r())
    plot_ly(hp, x = ~name, y = ~total_hours, type = "bar", marker = list(color = ~colour)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Total hours"), showlegend = FALSE)
  })

  output$hours_per_stage_table <- renderDT({
    hs <- hours_per_stage_by_weight(sessions_r(), stages_r(), projects_r())
    datatable(hs, options = list(pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui, server)
