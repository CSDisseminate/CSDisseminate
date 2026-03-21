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

# ── Authenticate ─────────────────────────────────────────────────────────────
gs4_auth()

# ── Read sheet ───────────────────────────────────────────────────────────────
team <- read_sheet(SHEET_ID, sheet = "People") |>
  dplyr::filter(!is.na(full_name)) |>
  dplyr::filter(str_detect(categories, "Collaborator"))

# ── Helper: convert full name to lowercase hyphenated slug ───────────────────
make_slug <- function(name) {
  name |> 
    str_remove_all("[^\\w\\s]") |>  # remove punctuation
    str_to_lower() |> 
    str_replace_all("\\s+", "-")
}

# ── Helper: assign sort order based on highest-ranking category ──────────────
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

# ── Helper: build full YAML frontmatter for one team member ──────────────────
build_frontmatter <- function(member) {
  
  slug        <- make_slug(member$full_name)
  
  # Parse categories
  categories <- str_split(member$categories, ",")[[1]] |> trimws()
  category_lines <- paste0('  - "', categories, '"', collapse = "\n")
  
  # Helper: write a field only if non-empty
  opt_field <- function(key, val) {
    val <- as.character(val)
    if (is.na(val) || val == "" || val == "NA") return("")
    paste0(key, ': "', val, '"\n')
  }
  
  # Build affiliations — use affiliation_1, fall back gracefully
  affil_val <- as.character(member$affiliation_1)
  affil_line <- if (!is.na(affil_val) && affil_val != "" && affil_val != "NA") {
    paste0('affiliation: "', affil_val, '"\n')
  } else {
    ""
  }
  
  frontmatter <- paste0(
    '---\n',
    'title: "',        member$full_name,  '"\n',
    'pagetitle: "',    member$full_name,  '"\n',
    'first-name: "',   member$first_name, '"\n',
    'last-name: "',    member$last_name,  '"\n',
    opt_field("credentials", member$credentials),
    opt_field("position",    member$position),
    affil_line,
    opt_field("pronouns",    member$pronouns),
    opt_field("orcid",       member$orcid),
    'date-joined: "',  member$date_joined, '"\n',
    'order: ', get_sort_order(member$categories), '\n',
    'categories:\n',   category_lines,     '\n',
    opt_field("description-research", member$description_research),
    opt_field("description-os",       member$description_os),
    opt_field("link",                 member$website_link),
    opt_field("website-button",       member$website_button_text),
    opt_field("headshot", if (!is.na(member$headshot_file) && member$headshot_file != "" && member$headshot_file != "NA") paste0("../_headshots/", member$headshot_file) else ""),
    'lightbox: true\n',
    'draft: false\n',
    'layout: article\n',
    'title-block-categories: false\n',
    if (!is.na(member$alias) && member$alias != "" && member$alias != "NA") paste0('aliases:\n  - /Team/', member$alias, '/\n') else '',
    'editor: source\n',
    '---\n'
  )
  
  frontmatter
}

# ── Helper: build full qmd body ───────────────────────────────────────────────
build_qmd <- function(member) {
  frontmatter <- build_frontmatter(member)
  
  # Only include website button row if link is present
  link_val <- as.character(member$website_link)
  button_val <- as.character(member$website_button_text)
  button_line <- if (!is.na(link_val) && link_val != "" && link_val != "NA" &&
                     !is.na(button_val) && button_val != "" && button_val != "NA") {
    '[{{< meta website-button>}}]({{< meta link >}}){.btn .btn-outline-primary .btn role="button" data-toggle="button"}\\\n\\\n'
  } else {
    ""
  }
  
  os_val <- as.character(member$description_os)
  os_line <- if (!is.na(os_val) && os_val != "" && os_val != "NA") {
    "{{< meta description-os >}}\n\n"
  } else {
    ""
  }
  
  headshot_val <- as.character(member$headshot_file)
  has_headshot <- !is.na(headshot_val) && headshot_val != "" && headshot_val != "NA"
  
  body <- if (has_headshot) {
    paste0(
      ':::::: columns\n',
      '::: {.column width="30%"}\n',
      '![{{< meta title >}}, {{< meta credentials >}}<br>{{< meta position >}}<br>{{< meta affiliation >}}]({{< meta headshot >}})\n',
      ':::\n',
      '::: {.column width="5%"}\n',
      ':::\n',
      '::: {.column width="65%"}\n',
      '{{< meta description-research >}}\n',
      '\n',
      os_line,
      button_line,
      ':::\n',
      '::::::\n'
    )
  } else {
    paste0(
      '{{< meta description-research >}}\n',
      '\n',
      os_line,
      button_line
    )
  }
  
  paste0(frontmatter, "\n", body)
}

# ── Main loop: generate one folder + index.qmd per team member ───────────────
pwalk(team, function(...) {
  member <- list(...)
  
  slug        <- make_slug(member$full_name)
  folder_path <- file.path(TEAM_DIR, slug)
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    message("Created folder: ", folder_path)
  }
  
  qmd_content <- build_qmd(member)
  
  output_path <- file.path(folder_path, "index.qmd")
  writeLines(qmd_content, output_path)
  message("Written: ", output_path)
})

message("Done. ", nrow(team), " team member(s) generated.")