# CLAUDE.md

Guidance for Claude when working in this repository.

## What this is

Materials for the workshop "Introduction to Bayesian Multilevel Latent Time Series Models with the R-package `mlts`" (EAM 2026), by Kenneth Koslowski & Fabian F. Münch. The repo is a Quarto reveal.js slide deck plus R scripts for two hands-on live sessions. It was started as a copy of the SAA workshop repo (single initial commit), so leftover SAA references may exist.

## Layout

- `index.qmd` – main deck; sources `R/setup.R` and pulls in the section files via `{{< include sections/*.qmd >}}`. Includes are toggled by commenting them out (usually only the section being worked on is active). Re-enable all of them before the final render.
- `sections/01-welcome.qmd` … `07-wrap_up.qmd` – one file per workshop part (welcome, DSEM intro, live session 1, practical aspects, model extensions, live session 2, wrap-up). Slides use `#` for sections and `##` for slides.
- `sections/<section>/` – cached model fits (`.rds`, `.Rdata`), summaries, and figures that the slides load instead of refitting. Do not delete or regenerate these casually: fits take a long time to run.
- `live_session/` – participant scripts (`live_session1.R`, `live_session2.R`), solutions (`*_solution.R`), `install_packages.R`, `bolzen_prepare.R` (downloads and preprocesses the data), and prefitted models in `live_session*_fits/`.
- `bolzen.qmd` – standalone documentation of the example dataset (Bolzenkötter et al., 2024).
- `R/setup.R` – shared setup: libraries, ggplot/bayesplot themes, knitr options, and loading of `downloaded_data/bolzen*.rda`.
- `references/` – `bibliography.bib` and `apa.csl` (APA citation style); cite with `@key`.
- `styles.css`, `_quarto.yml` – theme and reveal.js options (1600x900, `embed-resources: true`, `echo: false`, `freeze: auto`).
- `img/` – images used in slides.

## Build and run

- Render the deck: `quarto render index.qmd` (output `index.html`, self-contained). Preview: `quarto preview index.qmd`.
- Requires R with `mlts` (installed from the `RDSEM` branch of github.com/munchfab/mlts via `live_session/install_packages.R`), `rstan`, `tidyverse`, `bayesplot`, `knitr`, `kableExtra`, `osfr`.
- `downloaded_data/` is git-ignored and created by `live_session/bolzen_prepare.R` (downloads from OSF). `R/setup.R` loads it, so run that script first on a fresh checkout. Slides assume the working directory is the project root.
- Model fits are slow. Prefer loading the saved `.rds`/`.Rdata` files; only refit when a model or its data actually changed, and save the result to the same path.
- Quarto freeze (`_freeze/`) and `*.html`/`*.pdf` outputs are git-ignored.

## Conventions

- Code is in R (tidyverse style). Keep slide chunks quiet (`echo: false` is the default; override per chunk with `#| echo: true`).
- Keep figure code consistent with the themes set in `R/setup.R` (base size 18).
- Live session scripts use numbered section banners (`# 1. ... ####`). When changing an exercise, update both the participant script and its `_solution.R`, and the matching slides in `03-live_session1.qmd` / `06-live_session2.qmd`.
- Instructions to participants (README, slides) are written in English.
- When touching `mlts` model calls, check that the argument names match the `RDSEM` branch of the package.
