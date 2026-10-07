# Knit Tracker

A R Shiny dashboard for tracking multiple test-knit (or any) projects at once: deadlines, progress by stage, time logged, and a recommender that ranks every project by real schedule pressure so you know what to prioritise.

Everything is stored in plain CSV files on your own machine. 

## Features

- **Overview** — a progress bar per project, coloured by the project's own yarn colour, with a separate urgency badge for the deadline. Below it, an "Upcoming stage mini-deadlines" panel lists every not-done stage due within 30 days (or overdue) across active/pending projects, tinted by project colour and traffic-light urgency. A project stays visible here until its deadline actually passes, even if you've marked it `finished` early.
- **Project details** — pick a project to view and edit everything about it (designer, yarn weight, needle size, gauge, size, dates, status, the Ravelry-page flag, notes), add a brand new project, and see a segmented bar showing each of its stages coloured in by how complete it is, with a red/amber outline on any stage whose own mini-deadline is close or overdue.
- **Gantt** — three views of whether you're over-committed. A headline figure gives the minimum daily pace you'd need to sustain from today to hit *every* deadline in turn, and names the project setting that constraint (your real bottleneck, which often isn't the soonest deadline). A burndown chart plots your actual total remaining hours over time, reconstructed from logged sessions, against an ideal straight line to zero at your furthest deadline. Below those, a bar per active project shows the window from today to its estimated finish date at a chosen pace, with a diamond marking the real deadline.
- **What to knit now** — every active project's current stage, ranked by the same critical-path logic as the Gantt tab: slack once every *other* deadline due before this one is accounted for, not just this project's own remaining hours. Because all your knitting hours are one shared pool across every project, the top pick can shift whenever any project's progress or deadline changes, not just the one you're looking at. A button sets the assumed pace to your actual logged mean, pulled straight from Analytics, so you don't have to go check it yourself.
- **Log a session** — record hours per stage per day, with optional row-by-row progress for chart-heavy stages (e.g. "row 23 of 52"). Marking a stage `done` here (or in Manage stages) replaces its time estimate with the real hours you actually logged.
- **Manage stages** — add a project's construction stages (with an optional mini-deadline and category) and edit any stage's details later. A project's stages drop out of this tab once its deadline has passed.
- **Analytics** — hours logged per calendar day with a mean line (so you can see whether you're really hitting the pace you assume you are), total hours per project (bars coloured by project colour), hours per logged session grouped by stage *category* and yarn weight, and an estimate-accuracy table comparing each completed stage's actual logged hours against the estimate you made before starting it. All built from your own history; unlike the other tabs, this always includes every project regardless of deadline or status.

## Requirements

- R (4.3 or later recommended)
- These packages, installable from CRAN:

```r
install.packages(c(
  "shiny", "bslib", "DT", "plotly",
  "dplyr", "tidyr", "lubridate", "readr", "stringr"
))
```

## Running it

Clone the repo, then from R:

```r
shiny::runApp("path/to/knit-tracker", launch.browser = TRUE)
```

Or open `app.R` in RStudio and click **Run App** — though if RStudio's built-in Viewer window misbehaves on your OS, use the console command above instead, which opens it in your regular browser.

The `data/` CSVs have headers only in this repo. Add your first project via the **Project details** tab, or edit the CSVs directly.

## Data model

Three CSV files under `data/`, read and written by `R/data_io.R`:

**`projects.csv`** — one row per project: name, designer, yarn weight (free text, so held-together yarns like "sport (held: fingering + lace)" work fine), needle size, gauge, size, pattern-received date, start (cast-on) date, deadline, status (`active`/`pending`/`finished`), whether the Ravelry project page exists, a hex `colour` for its progress bar, and free-text notes. `status` is informational only for visibility purposes — a project stays on Overview and Manage stages until its `deadline` actually passes, regardless of status, and always stays visible in Project details and Analytics.

**`stages.csv`** — one row per construction stage of a project (however many you like, in whatever order fits the pattern), each rated on two independent 1–3 scales:

- `portability`: 1 = needs full kit/space at home, 2 = bag-knittable with the pattern and limited equipment, 3 = fully grab-and-go
- `focus`: 1 = mindless, fine when tired or in a lecture, 2 = needs some attention, 3 = needs full concentration (cables, colourwork, shaping)

Also tracks `est_hours` (optional — automatically overwritten with the real total hours logged once you mark the stage `done`, if any were logged) alongside `original_est_hours` (set once when the stage is created and never touched again, so you can later compare what you *actually* took against what you guessed beforehand — see Analytics), `status` (`not_started`/`in_progress`/`done`), an optional `stage_deadline` (a designer-set or self-imposed mini-deadline distinct from the project's overall deadline), a `stage_category` (a normalised label like "Swatch" or "Sleeves" used to group Analytics across projects even when the free-text `stage_name` is worded differently each time), and optional `total_rows`/`rows_done` for chart-heavy stages.

**`sessions.csv`** — one row per logged knitting session: date, project, stage, hours, where you were, your energy level, and notes.

## The recommender

"What to knit now" lists every active project's *current* stage (the first one not marked `done`) and ranks them using the same critical-path logic as the Gantt tab's bottleneck figure.

Each candidate's urgency is its **schedule slack**: its own deadline, minus the cumulative hours needed across *every* project with a deadline on or before it, divided by your assumed daily pace. This matters because all your knitting hours are one shared pool — a project isn't urgent just because its own deadline is close or its own remaining hours are high, it's urgent if the *combined* workload of everything due by then outpaces the time you actually have. A project with a distant deadline but a lot of other work competing for the same days ahead of it can rank well above one with a near deadline and almost nothing left to do. This also means the ranking can change whenever *any* project's progress or deadline shifts, not only the one you're currently looking at — accept that volatility as the cost of the ranking actually being correct.

The assumed daily pace is whatever you set it to; a button next to it fills in your actual mean hours/day from logged sessions (the same figure Analytics shows), so you're not guessing at your own pace.

"Hours needed" per stage comes from each stage's `est_hours` if you filled it in; otherwise it falls back to the average time your *completed* stages of that yarn weight have actually taken (summed across however many sessions each one took), or a generic 4-hour guess if you have no history at all yet. This means the estimate is based on very little data at first and gets more reliable the more stages you finish and log.

The table still shows each stage's `portability` and `focus` rating alongside the ranking, as context for whether the top pick actually suits where you are and how much energy you have right now — it just no longer filters the list down to only matching stages.

## Notes

- All data lives on disk in `data/*.csv`.
- `portability` (1–3: needs full kit/space at home, through to fully grab-and-go) and `focus` (1–3: mindless, through to needs full concentration) are two independent axes recorded per stage, shown for context on the recommender — a stage can be portable but demanding (cables on the bus) or immobile but mindless (weighing as you knit gradient balls at home while zoned out).
