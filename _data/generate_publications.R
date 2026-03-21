# _data/generate_publications.R
# Reads publication metadata from Google Sheets and generates
# individual index.qmd files for each publication.
# Run automatically as a Quarto pre-render script.

library(googlesheets4)
library(dplyr)
library(stringr)
library(purrr)

# ── Configuration ────────────────────────────────────────────────────────────

# Paste your Google Sheet ID here (from the sheet URL)
publications_sheet_id <- "1cMgRXn2YwB9GL_QZsceIysWgow3IMIFmvHGX3X2DScM"

# Path to your publications folder, relative to project root
publications_dir <- "Research/Publications"

# ── Authenticate ─────────────────────────────────────────────────────────────
# On first run this will open a browser to authenticate.
# Subsequent runs will use the cached token.
#gs4_auth()

# ──Read sheets ──────────────────────────────────────────────────────────────
publications <- read_sheet(publications_sheet_id, sheet = "Publications") %>%
  dplyr::filter(!is.na(folder_name))
authors_db   <- read_sheet(publications_sheet_id, sheet = "People")

# ── Remove old alias folders ──────────────────────────────────────────────────
# If a publication was renamed, its old folder name is stored in alias_1/alias_2.
# The script deletes those folders so orphaned pages don't remain on the site.
old_folders <- c(publications$alias_1, publications$alias_2)
old_folders <- old_folders[!is.na(old_folders) & old_folders != ""]

for (old_name in old_folders) {
  old_path <- file.path(publications_dir, old_name)
  if (dir.exists(old_path)) {
    unlink(old_path, recursive = TRUE)
    message("Removed old alias folder: ", old_path)
  }
}

# ── Helper: build author YAML block for one author ───────────────────────────
build_author_yaml <- function(author_name, authors_db) {
  search_name <- trimws(author_name)
  
  # First try exact match on full_name
  author_row <- authors_db |> filter(full_name == search_name)
  
  # If no match, try name_aliases column
  if (nrow(author_row) == 0 && "name_aliases" %in% names(authors_db)) {
    author_row <- authors_db |>
      filter(map_lgl(name_aliases, function(aliases) {
        if (is.na(aliases) || aliases == "") return(FALSE)
        any(trimws(str_split(aliases, ",")[[1]]) == search_name)
      }))
  }
  
  if (nrow(author_row) == 0) {
    warning(paste("Author not found in Authors sheet:", author_name))
    return(paste0("  - name: ", author_name))
  }
  
  # Always use full_name from the sheet, regardless of which name matched
  lines <- c(paste0("  - name: ", author_row$full_name))
  
  # Only write orcid if present
  orcid_val <- as.character(author_row$orcid)
  if (!is.na(orcid_val) && orcid_val != "") {
    lines <- c(lines, paste0("    orcid: ", orcid_val))
  }
  
  # Add affiliations (affiliation_1, affiliation_2, etc.)
  affil_cols <- names(author_row)[str_starts(names(author_row), "affiliation_")]
  affiliations <- author_row |> select(all_of(affil_cols)) |>
    unlist() |> na.omit() |> as.character()
  
  if (length(affiliations) > 0) {
    lines <- c(lines, "    affiliations:")
    for (affil in affiliations) {
      lines <- c(lines, paste0("      - ", affil))
    }
  }
  
  paste(lines, collapse = "\n")
}

# ── Helper: build show/link block for one link type ──────────────────────────
link_block <- function(key, val) {
  val <- as.character(val)
  if (is.null(val) || length(val) == 0 || is.na(val) || val == "") {
    show <- "false"
    url  <- ""
  } else {
    show <- "true"
    url  <- val
  }
  paste0(
    "  ", key, ":\n",
    "    show: ", show, "\n",
    "    link: \"", url, "\"\n"
  )
}

# ── Helper: build full YAML frontmatter for one publication ──────────────────
build_frontmatter <- function(pub, authors_db) {
  
  # Parse author order
  author_names <- str_split(pub$author_order, ",")[[1]] |> trimws()
  
  # Build each author block
  author_yaml <- map_chr(author_names, build_author_yaml, authors_db = authors_db)
  author_block <- paste(author_yaml, collapse = "\n")
  
  # Parse categories
  categories <- str_split(pub$categories, ",")[[1]] |> trimws()
  category_lines <- paste0('  - "', categories, '"', collapse = "\n")
  
  # Build aliases — stored as bare folder names in sheet, expanded to full paths here
  aliases <- c(pub$alias_1, pub$alias_2)
  aliases <- aliases[!is.na(aliases) & aliases != ""]
  alias_block <- if (length(aliases) > 0) {
    full_alias_paths <- paste0("/", publications_dir, "/", aliases, "/")
    alias_lines <- paste0("  - ", full_alias_paths, collapse = "\n")
    paste0("aliases:\n", alias_lines, "\n")
  } else {
    ""
  }
  
  # Build nested links block with show/link for each type
  links_block <- paste0(
    "links:\n",
    link_block("doi",             pub$doi),
    link_block("pubmed",          pub$pubmed),
    link_block("postprint",       pub$postprint),
    link_block("preprint",        pub$preprint),
    link_block("oa",              pub$oa),
    link_block("materials",       pub$materials),
    link_block("preregistration", pub$preregistration)
  )
  
  # Escape quotes in abstract
  abstract_escaped <- str_replace_all(pub$abstract, '"', '\\"')
  
  # Assemble full frontmatter
  frontmatter <- paste0(
    '---\n',
    'title: "', pub$title, '"\n',
    'publication: "', pub$publication, '"\n',
    'date: "', pub$date, '"\n',
    'year: "', pub$year, '"\n',
    'author:\n',
    author_block, '\n',
    'categories:\n',
    category_lines, '\n',
    links_block,
    alias_block,
    'abstract: "', abstract_escaped, '"\n',
    'title-block-banner: true\n',
    'editor: source\n',
    '---\n'
  )
  
  frontmatter
}

# ── Helper: build full qmd file content ──────────────────────────────────────
build_qmd <- function(pub, authors_db) {
  frontmatter <- build_frontmatter(pub, authors_db)
  
  body <- '\n\\\n\\\n\n## Important Links\n\n{{< include ../../../_includes/pub-buttons.qmd >}}\n'
  
  paste0(frontmatter, body)
}

# ── Main loop: generate one folder + index.qmd per publication ───────────────
pwalk(publications, function(...) {
  pub <- list(...)
  
  folder_path <- file.path(publications_dir, pub$folder_name)
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    message("Created folder: ", folder_path)
  }
  
  qmd_content <- build_qmd(pub, authors_db)
  
  output_path <- file.path(folder_path, "index.qmd")
  writeLines(qmd_content, output_path)
  message("Written: ", output_path)
})

message("Done. ", nrow(publications), " publication(s) generated.")
