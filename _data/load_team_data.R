# _data/load_team_data.R

library(googlesheets4)
library(yaml)
library(dplyr)

# First-time only: authenticate interactively
# (Quarto will cache your token in ~/.R/gargle)
# gs4_auth(email = "your_email@uh.edu")

# --- CONFIG ------------------------------------------------------------
sheet_id <- "197QV8dW_Pmb2GL_0a7DgEiy-5mqv9FpBuEsLSFrrb7M"  # your actual ID
sheet_name <- "Sheet1"  # or whatever your tab is named
output_dir <- "_data/team"

#googlesheets4::gs4_auth(scope = "https://www.googleapis.com/auth/drive")


# --- READ SHEET -------------------------------------------------------
message("Reading Google Sheet...")
team <-
  read_sheet("https://docs.google.com/spreadsheets/d/197QV8dW_Pmb2GL_0a7DgEiy-5mqv9FpBuEsLSFrrb7M/edit?usp=sharing")

# --- CLEAN COLUMN NAMES (optional) -----------------------------------
names(team) <- tolower(gsub("\\s+", "-", names(team)))

# --- WRITE ONE YAML PER PERSON ---------------------------------------
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

team |>
  rowwise() |>
  do({
    person <- as.list(.)
    # Create a filename based on first name or a slug column
    file_name <- paste0(output_dir, "/", gsub("\\s+", "_", person$`first-name`), ".yml")
    yaml::write_yaml(person, file_name)
    tibble()
  })

message("✅ Team metadata written to _data/team/")
