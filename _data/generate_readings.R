# _data/generate_readings.R
# Reads the community reading list from Google Sheets and writes a single
# YAML file that the Reading List page lists from.
#
# Unlike the other three generators, this one writes NO per-item pages.
# Every reading points off-site, so a generated page per item would never be
# linked (that is already true of all 13 Resources pages). A Quarto listing
# can take a YAML file as its `contents`, so one file is enough.
#
# NOTE: the output must NOT live in _data/. Directories beginning with "_" are
# excluded from the Quarto project, and a listing cannot read from them —
# verified: the listing renders zero items. Hence the project root.
#
# Run automatically as a Quarto pre-render script.

library(googlesheets4)
library(dplyr)
library(stringr)
library(purrr)
library(yaml)

# ── Configuration ────────────────────────────────────────────────────────────

SHEET_ID     <- "1cMgRXn2YwB9GL_QZsceIysWgow3IMIFmvHGX3X2DScM"
READINGS_TAB <- "Readings"
OUTPUT_FILE  <- "readings.yml"   # project root — see note above

# ── Authenticate ─────────────────────────────────────────────────────────────
gs4_auth()

# ── Helper: convert full name to lowercase hyphenated slug ───────────────────
# Duplicated from generate_team.R so that a sharer's name can be linked to
# their team page. Worth extracting to a shared _data/_helpers.R along with
# opt_field() and build_categories(), which are also duplicated across the
# generators.
make_slug <- function(name) {
  name |>
    str_remove_all("[^\\w\\s]") |>
    str_to_lower() |>
    str_replace_all("\\s+", "-")
}

# ── Helper: split a comma-separated cell into a character vector ─────────────
split_list <- function(x) {
  x <- as.character(x)
  if (length(x) == 0 || is.na(x) || x %in% c("", "NA")) return(character(0))
  out <- str_split(x, ",")[[1]] |> trimws()
  out[out != ""]
}

# ── Helper: normalise a single cell to a scalar string or NULL ───────────────
opt <- function(x) {
  x <- as.character(x)
  if (length(x) == 0 || is.na(x) || x %in% c("", "NA")) return(NULL)
  x
}

# ── Helper: coerce a spreadsheet truthy cell to logical ──────────────────────
as_flag <- function(x) {
  x <- tolower(trimws(as.character(x)))
  if (length(x) == 0 || is.na(x) || x == "") return(FALSE)
  x %in% c("true", "yes", "y", "1", "x")
}

# ── Read the Readings tab ────────────────────────────────────────────────────
# The tab may not exist yet. A missing tab must not break the whole site
# render, so fall back to leaving any existing readings.yml untouched.
sheet_names <- tryCatch(sheet_names(SHEET_ID), error = function(e) character(0))

if (!(READINGS_TAB %in% sheet_names)) {
  message("No '", READINGS_TAB, "' tab found in the sheet — skipping the reading list.")
  if (!file.exists(OUTPUT_FILE)) {
    writeLines("[]", OUTPUT_FILE)
    message("Wrote an empty ", OUTPUT_FILE, " so the page still renders.")
  }
  quit(save = "no", status = 0)
}

readings <- read_sheet(SHEET_ID, sheet = READINGS_TAB) |>
  dplyr::filter(!is.na(title), !is.na(url))

# Drop anything still being staged
if ("draft" %in% names(readings)) {
  readings <- readings |> dplyr::filter(!map_lgl(draft, as_flag))
}

# ── Resolve sharer names to team profile URLs ────────────────────────────────
# A sharer who matches someone in the People sheet links to their team page;
# anyone else (a Discord member without a profile) is credited by name only.
people <- tryCatch(
  read_sheet(SHEET_ID, sheet = "People") |> dplyr::filter(!is.na(full_name)),
  error = function(e) NULL
)

sharer_url <- function(name) {
  nm <- opt(name)
  if (is.null(nm) || is.null(people)) return(NULL)
  hit <- people |> dplyr::filter(full_name == nm)
  if (nrow(hit) == 0) return(NULL)
  paste0("/Team/", make_slug(hit$full_name[1]), "/")
}

# ── Build one listing item per reading ───────────────────────────────────────
items <- pmap(readings, function(...) {
  r <- list(...)

  item <- list(
    # `path` is what a Quarto listing treats as the item's link target.
    path        = as.character(r$url),
    url         = as.character(r$url),
    title       = as.character(r$title),
    # `date` drives sorting and is rendered by Quarto's date formatter
    date        = opt(r$date_added),
    categories  = split_list(r$categories)
  )

  item$source         <- opt(r$source)
  item$authors        <- opt(r$authors)
  item$`date-published` <- opt(r$date_published)
  item$type           <- opt(r$type)
  item$note           <- opt(r$note)
  item$`shared-by`    <- opt(r$shared_by)
  item$`shared-by-url` <- sharer_url(r$shared_by)
  item$`open-access`  <- as_flag(r$open_access)
  item$`oa-url`       <- opt(r$oa_url)

  # drop NULLs so absent fields simply don't appear in the YAML
  item[!map_lgl(item, is.null)]
})

# Newest first; Quarto re-sorts too, but a stable file makes diffs readable
dates <- map_chr(items, function(i) if (is.null(i$date)) "" else i$date)
items <- items[order(dates, decreasing = TRUE)]

write_yaml(items, OUTPUT_FILE)
message("Done. ", length(items), " reading(s) written to ", OUTPUT_FILE, ".")
