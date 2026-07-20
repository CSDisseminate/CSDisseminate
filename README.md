# CSDisseminate

Source for [www.csdisseminate.com](https://www.csdisseminate.com), the site of CSDisseminate — a working group promoting the adoption and acceptance of open science practices in Communication Sciences and Disorders (CSD).

## How the site is built

It is a [Quarto](https://quarto.org) website. `quarto render` writes the static site to `docs/`, which GitHub Pages serves (see `CNAME`). **`docs/` is committed** — a render is part of every content change.

```sh
quarto preview   # local dev server with live reload
quarto render    # full build into docs/
```

## Content sources

Most content pages are **generated, not hand-written**. Three pre-render scripts in `_data/` (wired up under `project.pre-render` in `_quarto.yml`) read a shared Google Sheet and write one `index.qmd` per item:

| Script | Sheet tab | Writes to |
| --- | --- | --- |
| `generate_publications.R` | `Publications`, `People` | `Research/Publications/` |
| `generate_team.R` | `People` | `Team/` |
| `generate_resources.R` | `Resources` | `Resources/` |

Because these run on every render, **edits made directly to files in those three directories will be overwritten.** Change the Google Sheet instead.

The scripts authenticate with `googlesheets4::gs4_auth()`, which prompts interactively on first use and then caches a token. A render on a machine without that token will stall at the auth prompt.

Hand-written content lives in `Announcements/`, `Events/`, `Featured-Scientists/`, and the top-level `.qmd` pages.

### Renaming an item

Each sheet has alias columns (`alias` for people, `alias_1`/`alias_2` for resources and publications) holding the item's previous folder name(s), comma-separated. The scripts use these to emit `aliases:` redirects **and** to delete the stale folder, so a renamed item does not linger as a duplicate page. Always fill the alias in when renaming something.

## Layout

- `_quarto.yml` — site config, navbar, themes, pre-render hooks
- `ejs/` — custom EJS templates for Quarto listings
- `styles.css`, `styles/*.scss` — site styling (`cosmo` light / `solar` dark, each with a custom SCSS layer)
- `_includes/pub-buttons.qmd` — shared DOI / preprint / materials button row for publication pages
- `custom-footer.html` — footer appended to every page

## License

Code is MIT licensed (see `LICENSE`). Site content is licensed [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
