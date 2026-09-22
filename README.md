# Knit Tracker

A R Shiny dashboard for tracking multiple test-knit (or any) projects at once: deadlines, progress by stage, time logged, and a simple recommender for "what should I knit right now" based on where you are and how much energy/time you have.

Everything is stored in plain CSV files on your own machine. 

## Features

- **Overview** — a progress bar per active project, coloured by the project's own yarn colour, with a separate urgency badge for the deadline. Below it, an "Upcoming stage mini-deadlines" panel lists every not-done stage due within 30 days (or overdue) across all active/pending projects, tinted by project colour and traffic-light urgency. Projects marked `finished` drop off both.
- **Project details** — pick a project to view and edit everything about it (designer, yarn weight, needle size, gauge, size, dates, status, the Ravelry-page flag, notes), add a brand new project, and see a segmented bar showing each of its stages coloured in by how complete it is, with a red/amber outline on any stage whose own mini-deadline is close or overdue.
- **Gantt** — for each active project, a bar from today to your estimated finish date at a chosen pace, plus the real deadline, so you can see which projects are at risk.
- **What to knit now** — tell it where you are, your energy level, and how much time you have; it suggests active projects whose current stage fits, ranked by how time-pressured they actually are (deadline minus remaining work at your pace), not just whichever deadline is soonest.
- **Log a session** — record hours per stage per day, with optional row-by-row progress for chart-heavy stages (e.g. "row 23 of 52").
- **Manage stages** — add a project's construction stages (with an optional mini-deadline and category) and edit any stage's details later. Stages belonging to a `finished` project are hidden here.
- **Analytics** — total hours per project (bars coloured by project colour) and hours per logged session grouped by stage *category* and yarn weight, built from your own history. Unlike the other tabs, this always includes every project, `finished` ones included.

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

**`projects.csv`** — one row per project: name, designer, yarn weight (free text, so held-together yarns like "sport (held: fingering + lace)" work fine), needle size, gauge, size, pattern-received date, start (cast-on) date, deadline, status (`active`/`pending`/`finished`), whether the Ravelry project page exists, a hex `colour` for its progress bar, and free-text notes. Marking a project `finished` removes it from Overview and Manage stages, but it stays fully visible and editable in Project details, and still shows up in Analytics.

**`stages.csv`** — one row per construction stage of a project (however many you like, in whatever order fits the pattern), each rated on two independent 1–3 scales:

- `portability`: 1 = needs full kit/space at home, 2 = bag-knittable with the pattern and limited equipment, 3 = fully grab-and-go
- `focus`: 1 = mindless, fine when tired or in a lecture, 2 = needs some attention, 3 = needs full concentration (cables, colourwork, shaping)

Also tracks `est_hours` (optional), `status` (`not_started`/`in_progress`/`done`), an optional `stage_deadline` (a designer-set or self-imposed mini-deadline distinct from the project's overall deadline), a `stage_category` (a normalised label like "Swatch" or "Sleeves" used to group Analytics across projects even when the free-text `stage_name` is worded differently each time), and optional `total_rows`/`rows_done` for chart-heavy stages.

**`sessions.csv`** — one row per logged knitting session: date, project, stage, hours, where you were, your energy level, and notes.

## The recommender

"What to knit now" maps your chosen location to a minimum `portability` and your energy to a maximum `focus`, filters each active project's *current* stage (the first one not marked `done`) against both, and — unless you said you only have a quick session (in which case it only suggests stages already `in_progress`, skipping the setup cost of starting something new) — ranks the results by **slack**: deadline minus the time still needed at your assumed knitting pace. A project with a distant deadline but a lot of work left can rank above one with a near deadline and almost nothing left.

"Time still needed" comes from each stage's `est_hours` if you filled it in; otherwise it falls back to the average time your *completed* stages of that yarn weight have actually taken (summed across however many sessions each one took), or a generic 4-hour guess if you have no history at all yet. This means the estimate is based on very little data at first and gets more reliable the more stages you finish and log.

## Notes

- All data lives on disk in `data/*.csv`.
- Two independent axes drive the recommender by design: `portability`/location answer "can I physically knit this here", `focus`/energy answers "do I have the mental bandwidth for this right now" — a stage can be portable but demanding (cables on the bus) or immobile but mindless (weighing as you knit gradient balls at home while zoned out).
