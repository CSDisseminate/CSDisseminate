# _data/generate_team.R

# generate_team.R
# Reads team member metadata from Google Sheets and generates
# individual index.qmd files for each team member.
# Run automatically as a Quarto pre-render script.

library(googlesheets4)
library(dplyr)
library(stringr)
library(purrr)

# ── Configuration ────────────────────────────────────────────────────────────

# Paste your Google Sheet ID here (from the sheet URL)
SHEET_ID <- "1cMgRXn2YwB9GL_QZsceIysWgow3IMIFmvHGX3X2DScM"

# Path to your team folder, relative to project root
TEAM_DIR <- "Team"

HEADSHOTS_DIR    <- "../_headshots"   # relative to each member's folder

# ── Authenticate ─────────────────────────────────────────────────────────────
gs4_auth()

# ── Helper: convert full name to lowercase hyphenated slug ───────────────────
make_slug <- function(name) {
  name |> 
    str_remove_all("[^\\w\\s]") |>
    str_to_lower() |> 
    str_replace_all("\\s+", "-")
}

# ── Read sheet ───────────────────────────────────────────────────────────────
all_members <- read_sheet(SHEET_ID, sheet = "People") |>
  dplyr::filter(!is.na(full_name))

# Active collaborators (exclude Former Collaborator)
team <- all_members |>
  dplyr::filter(str_detect(categories, "Collaborator")) |>
  dplyr::filter(!str_detect(categories, "Former Collaborator"))

# Former collaborators
former <- all_members |>
  dplyr::filter(str_detect(categories, "Former Collaborator"))

# ── Helper: sort order by highest-ranking category ───────────────────────────
role_order <- c(
  "Chair"                  = 1,
  "Vice-Chair & Secretary" = 2,
  "Treasurer"              = 3,
  "Board Member"           = 4,
  "Website Team Lead"      = 5,
  "Ambassador Team Lead"   = 6,
  "Social Media Team Lead" = 7,
  "Collaborator"           = 8,
  "Coauthor"               = 9,
  "Former Collaborator"    = 10
)

get_sort_order <- function(categories_str) {
  cats <- str_split(categories_str, ",")[[1]] |> trimws()
  orders <- role_order[cats]
  orders <- orders[!is.na(orders)]
  if (length(orders) == 0) return(99)
  min(orders)
}

# ── Helper: write field only if non-empty ────────────────────────────────────
opt_field <- function(key, val) {
  val <- as.character(val)
  if (is.na(val) || val == "" || val == "NA") return("")
  paste0(key, ': "', val, '"\n')
}

# ── Helper: build categories YAML lines ──────────────────────────────────────
build_categories <- function(categories_str) {
  cats <- str_split(categories_str, ",")[[1]] |> trimws()
  paste0('  - "', cats, '"', collapse = "\n")
}

# ── Remove folders for anyone moved to Former Collaborator ───────────────────
former_slugs <- map_chr(former$full_name, make_slug)
active_slugs <- tolower(map_chr(team$full_name, make_slug))

for (slug in former_slugs) {
  if (tolower(slug) %in% active_slugs) next
  old_path <- file.path(TEAM_DIR, slug)
  if (dir.exists(old_path)) {
    unlink(old_path, recursive = TRUE)
    message("Removed former collaborator folder: ", old_path)
  }
}

# ── Build frontmatter for active team member ─────────────────────────────────
build_frontmatter <- function(member, draft = FALSE) {
  
  headshot_val <- as.character(member$headshot_file)
  headshot_line <- if (!is.na(headshot_val) && headshot_val != "" && headshot_val != "NA") {
    paste0('headshot: "', HEADSHOTS_DIR, '/', headshot_val, '"\n')
  } else {
    ""
  }
  
  affil_val <- as.character(member$affiliation_1)
  affil_line <- if (!is.na(affil_val) && affil_val != "" && affil_val != "NA") {
    paste0('affiliation: "', affil_val, '"\n')
  } else {
    ""
  }
  
  alias_line <- if (!is.na(member$alias) && member$alias != "" && member$alias != "NA") {
    paste0('aliases:\n  - /Team/', member$alias, '/\n')
  } else {
    ""
  }
  
  paste0(
    '---\n',
    'title: "',       member$full_name,  '"\n',
    'pagetitle: "',   member$full_name,  '"\n',
    'first-name: "',  member$first_name, '"\n',
    'last-name: "',   member$last_name,  '"\n',
    opt_field("credentials", member$credentials),
    opt_field("position",    member$position),
    affil_line,
    opt_field("pronouns",    member$pronouns),
    opt_field("orcid",       member$orcid),
    'date-joined: "', member$date_joined, '"\n',
    'order: ',        get_sort_order(member$categories), '\n',
    'categories:\n',  build_categories(member$categories), '\n',
    opt_field("description-research", member$description_research),
    opt_field("description-os",       member$description_os),
    opt_field("link",                 member$website_link),
    opt_field("website-button",       member$website_button_text),
    headshot_line,
    'lightbox: true\n',
    'draft: ', tolower(as.character(draft)), '\n',
    'layout: article\n',
    'title-block-categories: false\n',
    alias_line,
    'editor: source\n',
    '---\n'
  )
}

# ── Build body for active team member ────────────────────────────────────────
build_body <- function(member) {
  link_val   <- as.character(member$website_link)
  button_val <- as.character(member$website_button_text)
  button_line <- if (!is.na(link_val) && link_val != "" && link_val != "NA" &&
                     !is.na(button_val) && button_val != "" && button_val != "NA") {
    '[{{< meta website-button>}}]({{< meta link >}}){.btn .btn-outline-primary .btn role="button" data-toggle="button"}\\\n\\\n'
  } else {
    ""
  }
  
  os_val  <- as.character(member$description_os)
  os_line <- if (!is.na(os_val) && os_val != "" && os_val != "NA") {
    "{{< meta description-os >}}\n\n"
  } else {
    ""
  }
  
  headshot_val <- as.character(member$headshot_file)
  has_headshot <- !is.na(headshot_val) && headshot_val != "" && headshot_val != "NA"
  
  if (has_headshot) {
    paste0(
      ':::::: columns\n',
      '::: {.column width="30%"}\n',
      '![{{< meta title >}}, {{< meta credentials >}}<br>{{< meta position >}}<br>{{< meta affiliation >}}]({{< meta headshot >}})\n',
      ':::\n',
      '::: {.column width="5%"}\n',
      ':::\n',
      '::: {.column width="65%"}\n',
      '{{< meta description-research >}}\n\n',
      os_line,
      button_line,
      ':::\n',
      '::::::\n'
    )
  } else {
    paste0(
      '{{< meta description-research >}}\n\n',
      os_line,
      button_line
    )
  }
}

# ── Main loop: active team members ───────────────────────────────────────────
pwalk(team, function(...) {
  member      <- list(...)
  slug        <- make_slug(member$full_name)
  folder_path <- file.path(TEAM_DIR, slug)
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    message("Created folder: ", folder_path)
  }
  
  qmd_content <- paste0(build_frontmatter(member, draft = FALSE), "\n", build_body(member))
  writeLines(qmd_content, file.path(folder_path, "index.qmd"))
  message("Written: ", file.path(folder_path, "index.qmd"))
})

# ── Former collaborator stubs (draft: true, minimal frontmatter) ─────────────
pwalk(former, function(...) {
  member      <- list(...)
  slug        <- make_slug(member$full_name)
  folder_path <- file.path(TEAM_DIR, slug)
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    message("Created stub folder: ", folder_path)
  }
  
  # Minimal frontmatter — just enough for the listing to pick up
  frontmatter <- paste0(
    '---\n',
    'title: "',      member$full_name, '"\n',
    'last-name: "',  member$last_name, '"\n',
    opt_field("credentials", member$credentials),
    'categories:\n', build_categories(member$categories), '\n',
    'draft: false\n',
    'robots: "noindex"\n',
    'editor: source\n',
    '---\n'
  )
  
  writeLines(frontmatter, file.path(folder_path, "index.qmd"))
  message("Written stub: ", file.path(folder_path, "index.qmd"))
})

message("Done. ", nrow(team), " active member(s), ", nrow(former), " former member(s).")
