# generate_resources.R
# Reads resource metadata from Google Sheets and generates
# individual index.qmd files for each resource.
# Run automatically as a Quarto pre-render script.

library(googlesheets4)
library(dplyr)
library(stringr)
library(purrr)

# ── Configuration ────────────────────────────────────────────────────────────

SHEET_ID      <- "1cMgRXn2YwB9GL_QZsceIysWgow3IMIFmvHGX3X2DScM"
RESOURCES_DIR <- "Resources"

# ── Authenticate ─────────────────────────────────────────────────────────────
gs4_auth()

# ── Read sheet ───────────────────────────────────────────────────────────────
resources <- read_sheet(SHEET_ID, sheet = "Resources") |>
  dplyr::filter(!is.na(folder_name))

# ── Remove old alias folders ──────────────────────────────────────────────────
active_folders <- tolower(resources$folder_name)
old_folders <- c(resources$alias_1, resources$alias_2)
old_folders <- old_folders[!is.na(old_folders) & old_folders != ""]

for (old_name in old_folders) {
  if (tolower(old_name) %in% active_folders) {
    message("Skipping alias folder (matches active resource): ", old_name)
    next
  }
  old_path <- file.path(RESOURCES_DIR, old_name)
  if (dir.exists(old_path)) {
    unlink(old_path, recursive = TRUE)
    message("Removed old alias folder: ", old_path)
  }
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
  paste0(" - ", cats, collapse = "\n")
}

# ── Helper: build aliases block ──────────────────────────────────────────────
build_aliases <- function(pub) {
  aliases <- c(pub$alias_1, pub$alias_2)
  aliases <- aliases[!is.na(aliases) & aliases != ""]
  if (length(aliases) == 0) return("")
  alias_lines <- paste0("  - /Resources/", aliases, "/", collapse = "\n")
  paste0("aliases:\n", alias_lines, "\n")
}

# ── Helper: build frontmatter ────────────────────────────────────────────────
build_frontmatter <- function(res) {
  paste0(
    '---\n',
    'title: "',        res$title,  '"\n',
    'pagetitle: "',    res$title,  '"\n',
    'date: "',         res$date,   '"\n',
    'date-title: "Date Added"\n',
    opt_field("author",            res$author),
    'author-title: "Contributor"\n',
    opt_field("short-description", res$short_description),
    opt_field("resource-link",     res$resource_link),
    opt_field("resource-button",   res$resource_button),
    'categories:\n',
    build_categories(res$categories), '\n',
    build_aliases(res),
    'editor: source\n',
    '---\n'
  )
}

# ── Helper: build body ───────────────────────────────────────────────────────
build_body <- function(res) {
  link_val   <- as.character(res$resource_link)
  button_val <- as.character(res$resource_button)
  has_link   <- !is.na(link_val) && link_val != "" && link_val != "NA"
  has_button <- !is.na(button_val) && button_val != "" && button_val != "NA"
  
  button_line <- if (has_link && has_button) {
    paste0('\n[{{< meta resource-button>}}]({{< meta resource-link >}}){.btn .btn-outline-primary .btn role="button" data-toggle="button"}\n')
  } else if (has_link) {
    paste0('\n[Visit Resource]({{< meta resource-link >}}){.btn .btn-outline-primary .btn role="button" data-toggle="button"}\n')
  } else {
    ""
  }
  
  paste0(
    '----\n',
    '## About this Resource\n',
    '{{< meta short-description >}}\n',
    '\\\n',
    button_line
  )
}

# ── Main loop ────────────────────────────────────────────────────────────────
pwalk(resources, function(...) {
  res <- list(...)
  
  folder_path <- file.path(RESOURCES_DIR, res$folder_name)
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    message("Created folder: ", folder_path)
  }
  
  qmd_content <- paste0(build_frontmatter(res), "\n", build_body(res))
  output_path <- file.path(folder_path, "index.qmd")
  writeLines(qmd_content, output_path)
  message("Written: ", output_path)
})

message("Done. ", nrow(resources), " resource(s) generated.")
