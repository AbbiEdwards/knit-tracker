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
LOCATIONS <- names(location_min_portability)
ENERGIES <- names(energy_max_focus)

PORTABILITY_LABEL <- "Portability (1 = needs full kit, 3 = grab-and-go)"
FOCUS_LABEL <- "Focus required (1 = mindless, 3 = full concentration)"

# A small, low-opacity 5-petal floral sprig used as a subtle decorative accent.
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
  success = "#8FAE8B",
  warning = "#E8B54A",
  danger = "#C25C5C",
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
  /* bslib clips card content to its rounded corners by default, which cuts
     off a select dropdown's flyout menu - only cards holding a dropdown
     that needs to open past the card's edge opt out of that clipping. */
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
      dateInput("np_pattern_received", "Pattern received date", value = Sys.Date()),
      dateInput("np_start_date", "Start date (leave as today if not cast on yet)", value = Sys.Date()),
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
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Log time"),
        dateInput("log_date", "Date", value = Sys.Date()),
        selectInput("log_project", "Project", choices = NULL),
        selectInput("log_stage", "Stage", choices = NULL),
        uiOutput("log_row_progress_ui"),
        numericInput("log_rows_done", "Chart/row-tracked stage: now at row # (leave blank if not applicable)", value = NA, min = 0, step = 1),
        numericInput("log_hours", "Hours", value = 1, min = 0, step = 0.25),
        selectInput("log_location", "Where", choices = LOCATIONS),
        selectInput("log_energy", "Energy", choices = ENERGIES),
        textAreaInput("log_notes", "Notes (optional)", placeholder = "e.g. frogged the raglan increases twice, gauge issue"),
        selectInput("log_stage_status", "Update this stage's status to:", choices = c("(leave unchanged)", "in_progress", "done")),
        actionButton("log_submit", "Log session", class = "btn-primary")
      ),
      card(
        card_header("Recent sessions"),
        DTOutput("recent_sessions")
      )
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
        div(class = "align-with-input", actionButton("ns_submit", "Add stage", class = "btn-primary"))
      )
    ),
    card(
      height = "480px",
      card_header("All stages"),
      DTOutput("stages_table")
    ),
    card(
      height = "440px",
      card_header("Edit a stage"),
      p("Pick a stage to load its current values, adjust anything, then save."),
      selectInput("edit_stage", "Stage", choices = NULL),
      layout_columns(
        col_widths = c(6, 6),
        textInput("edit_name", "Stage name"),
        selectInput("edit_status", "Status", choices = c("not_started", "in_progress", "done"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        sliderInput("edit_portability", PORTABILITY_LABEL, min = 1, max = 3, value = 2),
        sliderInput("edit_focus", FOCUS_LABEL, min = 1, max = 3, value = 2)
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        numericInput("edit_est_hours", "Estimated hours", value = NA, min = 0, step = 0.5),
        numericInput("edit_total_rows", "Total rows in chart", value = NA, min = 1, step = 1),
        div(class = "align-with-input", actionButton("edit_submit", "Save changes", class = "btn-primary"))
      )
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
        card_header("Hours per logged session, by stage and yarn weight (from your history)"),
        DTOutput("hours_per_stage_table")
      )
    )
  )
)

# ---- server -------------------------------------------------------------

server <- function(input, output, session) {

  refresh <- reactiveVal(0)
  bump <- function() refresh(refresh() + 1)

  projects_r <- reactive({ refresh(); read_projects() })
  stages_r   <- reactive({ refresh(); read_stages() })
  sessions_r <- reactive({ refresh(); read_sessions() })

  # Keep project dropdowns in sync with the underlying data. Rebuilding
  # choices without re-selecting the current value would otherwise silently
  # reset each dropdown to its first option on every refresh (e.g. right
  # after saving an edit).
  observe({
    p <- projects_r()
    choices <- setNames(p$project_id, p$name)
    # finished projects are done with, so they're left out of "add a stage"
    # - but still findable via Project details, Analytics, and Log a session
    not_finished <- p %>% filter(status != "finished")
    not_finished_choices <- setNames(not_finished$project_id, not_finished$name)

    updateSelectInput(session, "log_project", choices = choices, selected = input$log_project)
    updateSelectInput(session, "ns_project", choices = not_finished_choices, selected = input$ns_project)
    updateSelectInput(session, "detail_project", choices = choices, selected = input$detail_project)
  })

  # load the selected project's current values into the edit form
  observeEvent(input$detail_project, {
    req(input$detail_project)
    proj <- projects_r() %>% filter(project_id == input$detail_project)
    if (nrow(proj) == 0) {
      return(NULL)
    }
    proj <- proj[1, ]

    updateTextInput(session, "detail_name", value = proj$name)
    updateTextInput(session, "detail_designer", value = proj$designer)
    updateSelectizeInput(session, "detail_weight", selected = proj$yarn_weight)
    updateTextInput(session, "detail_needle", value = proj$needle_size)
    updateTextInput(session, "detail_gauge", value = proj$gauge)
    updateTextInput(session, "detail_size", value = proj$size)
    updateTextInput(session, "detail_colour", value = proj$colour)
    # updateDateInput(value = NULL) is silently dropped rather than clearing
    # the field (it leaves whatever the previous project showed on screen),
    # so a missing date is set directly via the lower-level input message
    # instead - the same mechanism updateDateInput itself uses, just without
    # its NULL-means-skip behaviour.
    set_date_field <- function(input_id, date_value) {
      if (is.na(date_value)) {
        session$sendInputMessage(input_id, list(value = ""))
      } else {
        updateDateInput(session, input_id, value = date_value)
      }
    }
    set_date_field("detail_pattern_received", proj$pattern_received)
    set_date_field("detail_start_date", proj$start_date)
    set_date_field("detail_deadline", proj$deadline)
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

  # A numbered segment per stage, coloured in by how complete it is. Native
  # HTML title tooltips are unreliable/hard to discover, so the stage names
  # are shown in a plain-text legend below the bar instead of relying on
  # hover.
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
      div(
        style = paste0(
          "position:relative; flex:1; height:22px; margin:0 2px; border-radius:6px; overflow:hidden;",
          "background:#F1DDE2; border:1px solid #F1DDE2;"
        ),
        div(style = paste0("background:", bar_colour, "; width:", pct, "%; height:100%;")),
        div(
          style = "position:absolute; inset:0; display:flex; align-items:center; justify-content:center; font-size:11px; font-weight:600; color:#4A3540; text-shadow:0 0 3px white, 0 0 3px white;",
          row$stage_order
        )
      )
    })

    legend <- paste0(
      st$stage_order, ". ", st$stage_name, " [", st$status, "]",
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
    bump()
  })

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

  # cap "now at row #" at the stage's total_rows, if it has row tracking
  observeEvent(input$log_stage, {
    st <- stages_r() %>% filter(stage_id == input$log_stage)
    max_rows <- if (nrow(st) == 0) NA else st$total_rows[1]
    updateNumericInput(session, "log_rows_done", max = max_rows)
  })

  observe({
    p <- projects_r() %>% filter(status != "finished")
    st <- stages_r() %>% filter(project_id %in% p$project_id) %>% arrange(project_id, stage_order)
    if (nrow(st) == 0) {
      updateSelectInput(session, "edit_stage", choices = character(0))
    } else {
      labels <- paste0(p$name[match(st$project_id, p$project_id)], " — ", st$stage_order, ". ", st$stage_name, " [", st$status, "]")
      choices <- setNames(st$stage_id, labels)
      updateSelectInput(session, "edit_stage", choices = choices, selected = input$edit_stage)
    }
  })

  # load the selected stage's current values into the edit form
  observeEvent(input$edit_stage, {
    req(input$edit_stage)
    st <- stages_r() %>% filter(stage_id == input$edit_stage)
    if (nrow(st) == 0) {
      return(NULL)
    }
    updateTextInput(session, "edit_name", value = st$stage_name[1])
    updateSelectInput(session, "edit_status", selected = st$status[1])
    updateSliderInput(session, "edit_portability", value = st$portability[1])
    updateSliderInput(session, "edit_focus", value = st$focus[1])
    updateNumericInput(session, "edit_est_hours", value = st$est_hours[1])
    updateNumericInput(session, "edit_total_rows", value = st$total_rows[1])
  })

  observeEvent(input$edit_submit, {
    req(input$edit_stage)
    edit_stage(
      input$edit_stage, input$edit_name, input$edit_portability, input$edit_focus,
      input$edit_est_hours, input$edit_total_rows, input$edit_status
    )
    bump()
  })

  # ---- Overview: progress bars ---
  output$progress_bars <- renderUI({
    not_finished <- projects_r() %>% filter(status != "finished")
    prog <- project_progress(not_finished, stages_r()) %>%
      mutate(days_left = as.numeric(as.Date(deadline) - Sys.Date()))

    bars <- lapply(seq_len(nrow(prog)), function(i) {
      row <- prog[i, ]
      pct <- round(row$pct_complete * 100)
      bar_colour <- if (!is.na(row$colour) && nzchar(row$colour)) row$colour else "#C9789A"
      urgency_colour <- if (row$days_left < 7) {
        "#C25C5C"
      } else if (row$days_left < 21) {
        "#E8B54A"
      } else {
        "#8FAE8B"
      }
      tagList(
        div(
          style = "margin-bottom: 6px;",
          strong(row$name),
          span(
            style = paste0(
              "float:right; background:", urgency_colour, "; color:white;",
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

  # ---- Gantt ---
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

  # ---- Recommender ---
  output$recommend_table <- renderDT({
    rec <- recommend_projects(
      projects_r(), stages_r(), sessions_r(), input$rec_location, input$rec_energy,
      input$rec_time, hours_per_week = input$rec_hours_per_day * 7
    )
    datatable(rec, options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })

  # ---- Log a session ---
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

  # ---- Manage: add project ---
  observeEvent(input$np_submit, {
    req(input$np_name)
    add_project(
      input$np_name, input$np_designer, input$np_weight, input$np_needle, input$np_gauge,
      input$np_size, input$np_pattern_received, input$np_start_date, input$np_deadline,
      input$np_notes, input$np_colour
    )
    updateTextInput(session, "np_name", value = "")
    updateTextInput(session, "np_designer", value = "")
    updateTextInput(session, "np_size", value = "")
    updateTextAreaInput(session, "np_notes", value = "")
    bump()
  })

  # ---- Manage: add stage ---
  observeEvent(input$ns_submit, {
    req(input$ns_project, input$ns_name)
    add_stage(
      input$ns_project, input$ns_name, input$ns_order, input$ns_portability,
      input$ns_focus, input$ns_est_hours, input$ns_total_rows
    )
    updateTextInput(session, "ns_name", value = "")
    bump()
  })

  output$stages_table <- renderDT({
    not_finished <- projects_r() %>% filter(status != "finished") %>% select(project_id, name)
    st <- stages_r() %>%
      inner_join(not_finished, by = "project_id") %>%
      arrange(name, stage_order) %>%
      select(name, stage_order, stage_name, portability, focus, est_hours, status, total_rows, rows_done)
    datatable(st, options = list(pageLength = 10), rownames = FALSE)
  })

  # ---- Analytics ---
  output$hours_per_project_plot <- renderPlotly({
    hp <- total_hours_per_project(sessions_r(), projects_r())
    plot_ly(hp, x = ~name, y = ~total_hours, type = "bar") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Total hours"))
  })

  output$hours_per_stage_table <- renderDT({
    hs <- hours_per_stage_by_weight(sessions_r(), stages_r(), projects_r())
    datatable(hs, options = list(pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui, server)
